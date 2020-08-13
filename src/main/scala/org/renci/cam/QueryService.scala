package org.renci.cam

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.generic.auto._
import io.circe.parser.parse
import io.circe.syntax._
import org.apache.commons.io.IOUtils
import org.apache.commons.lang3.StringUtils
import org.apache.jena.ext.com.google.common.base.CaseFormat
import org.apache.jena.query.{QueryFactory, ResultSet}
import org.renci.cam.domain._
import zio.config.ZConfig
import zio.{RIO, Task, ZIO, config => _}

import scala.collection.JavaConverters._
import scala.collection.mutable

object QueryService extends LazyLogging {

  case class NewTRAPIEdge(`type`: String, source_id: String, target_id: String)

  def getNodeTypes(nodes: List[TRAPIQueryNode]) =
    for {
      nodeTypes <- Task.effect(nodes.collect {
        case node if node.`type`.nonEmpty => (node.id, "bl:" + CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, node.`type`))
      }.toMap)
      newNodeTypes <- Task.effect(nodeTypes ++ nodes.flatMap(node => node.curie.map(node.id -> _)).toMap)
    } yield newNodeTypes

  def readPrefixes =
    for {
      legacyJSONString <-
        Task.effect(IOUtils.resourceToString("legacy_prefixes.json", StandardCharsets.UTF_8, this.getClass.getClassLoader))
      legacyJSON <- Task.effect(parse(legacyJSONString).getOrElse(Json.Null))
      legacyMap <- Task.effect(legacyJSON.as[Map[String, String]].getOrElse(Map.empty))
      jsonString <- Task.effect(IOUtils.resourceToString("prefixes.json", StandardCharsets.UTF_8, this.getClass.getClassLoader))
      json <- Task.effect(parse(jsonString).getOrElse(Json.Null))
      map <- Task.effect(json.as[Map[String, String]].getOrElse(Map.empty))
    } yield legacyMap.++(map)

  def applyPrefix(value: String) =
    for {
      p <- readPrefixes
      ret =
        p
          .filter(entry => value.startsWith(entry._2))
          .map(entry => s"${entry._1}:" + value.substring(entry._2.length, value.length))
          .headOption
          .getOrElse(value)
    } yield ret

  def run(limit: Int, queryGraph: TRAPIQueryGraph): RIO[ZConfig[AppConfig], ResultSet] =
    for {
      nodeTypes <- getNodeTypes(queryGraph.nodes)
      predicates <- ZIO.foreach(queryGraph.edges.filter(_.`type`.nonEmpty)) { edge =>
        for {
          queryText <- Task.effect(
            s"""PREFIX bl: <https://w3id.org/biolink/vocab/>
               |SELECT DISTINCT ?predicate WHERE {
               |bl:${edge.`type`} <http://reasoner.renci.org/vocab/slot_mapping> ?predicate .
               |}""".stripMargin
          )
          query <- Task.effect(QueryFactory.create(queryText))
          resultSet <- SPARQLQueryExecutor.runSelectQuery(query)
          predicates = (for {
              solution <- resultSet.asScala
              v <- solution.varNames.asScala
              node = solution.get(v)
            } yield s"<$node>").mkString(" ")
          predicateValuesBlock = s"VALUES ?${edge.id} { $predicates }"
          triple = s"  ?${edge.source_id} ?${edge.id} ?${edge.target_id} ."
        } yield (Set(edge.source_id, edge.target_id),
                 Set(edge.source_id -> edge.source_id, edge.target_id -> edge.target_id),
                 s"$predicateValuesBlock\n$triple")
      }
      whereClauseParts =
        queryGraph.nodes
          .map(node => String.format("  ?%1$s sesame:directType ?%1$s_type .", node.id))
          .mkString("\n")
      whereClause = s"WHERE { \n$whereClauseParts"
      (instanceVars, instanceVarsToTypes, sparqlLines) = predicates.unzip3
      ids =
        instanceVars.toSet.flatten.map(a => s"?$a").toList :::
          queryGraph.nodes.map(a => s"?${a.id}_type") :::
          queryGraph.edges.map(a => s"?${a.id}")
      selectClause = s"SELECT DISTINCT ${ids.mkString(" ")} "
      moreLines = for {
        (subj, typ) <- instanceVarsToTypes.flatten
        v <- nodeTypes.get(typ)
      } yield s"?$subj rdf:type $v ."
      valuesClause = (sparqlLines ++ moreLines).mkString("\n")
      limitSparql = if (limit > 0) s" LIMIT $limit" else ""
      prefixMap <- readPrefixes
      prefixes = prefixMap.map(entry => s"PREFIX ${entry._1}: <${entry._2}>").mkString("\n")
      queryString = s"$prefixes\n$selectClause\n$whereClause\n$valuesClause \n } $limitSparql"
      query <- Task.effect(QueryFactory.create(queryString))
      response <- SPARQLQueryExecutor.runSelectQuery(query)
    } yield response

  def parseResultSet(queryGraph: TRAPIQueryGraph, resultSet: ResultSet): RIO[ZConfig[AppConfig], TRAPIMessage] =
    for {
      kgNodes <- Task.effect(mutable.ListBuffer[TRAPINode]())
      kgEdges <- Task.effect(mutable.ListBuffer[TRAPIEdge]())
      kgExtraNodes <- Task.effect(mutable.ListBuffer[TRAPINodeBinding]())
      kgExtraEdges <- Task.effect(mutable.ListBuffer[TRAPIEdgeBinding]())
      messageDigest <- Task.effect(MessageDigest.getInstance("SHA-256"))
      results <- ZIO.foreach(resultSet.asScala.toList) { querySolution =>
        for {
          nodeMap <- Task.effect(queryGraph.nodes.map(n => (n.id, querySolution.get(s"${n.id}_type").toString)).toMap)
          nodeBindings <- ZIO.foreach(queryGraph.nodes) { n =>
            for {
              abbreviatedNodeType <- applyPrefix(nodeMap.get(n.id).get)
              nodeDetails <- getKnowledgeGraphNodeDetails(s"<${nodeMap.get(n.id).get}>")
              nodeDetailsHead <- ZIO.fromOption(nodeDetails.headOption).orElseFail(new Exception(s"Missing node details: ${n.id}"))
              _ = {
                val attribute = TRAPINodeAttribute(
                  None,
                  abbreviatedNodeType.substring(abbreviatedNodeType.indexOf(":") + 1, abbreviatedNodeType.length),
                  abbreviatedNodeType,
                  nodeMap.get(n.id),
                  Some(abbreviatedNodeType.substring(0, abbreviatedNodeType.indexOf(":")))
                )
                kgNodes += TRAPINode(Some(nodeDetailsHead._1), nodeDetailsHead._2.sorted, List[TRAPINodeAttribute](attribute))
              }
              nodeBinding <- Task.effect(TRAPINodeBinding(Some(n.id), abbreviatedNodeType))
            } yield nodeBinding
          }
          edgeBindings <- ZIO.foreach(queryGraph.edges) { e =>
            for {
              predicateRDFNode <- Task.effect(querySolution.get(e.id).toString)
              sourceRDFNode <- Task.effect(querySolution.get(e.source_id).toString)
              targetRDFNode <- Task.effect(querySolution.get(e.target_id).toString)
              knowledgeGraphId = {
                val newTRAPIEdge =
                  NewTRAPIEdge(e.`type`, e.source_id, e.target_id).asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
                String.format("%064x", new BigInteger(1, messageDigest.digest(newTRAPIEdge)))
              }
              prefixedSource <- applyPrefix(nodeMap.get(e.source_id).get)
              prefixedTarget <- applyPrefix(nodeMap.get(e.target_id).get)
              _ = kgEdges += TRAPIEdge(knowledgeGraphId, Some(e.`type`), prefixedSource, prefixedTarget)
              prov <- getProvenance(sourceRDFNode, predicateRDFNode, targetRDFNode)
            } yield TRAPIEdgeBinding(Some(e.id), knowledgeGraphId, Some(prov.toString))
          }
          _ <- ZIO.foreach(edgeBindings.map(a => a.provenance.get).toList) { p =>
            for {
              camStuffTriples <- getCAMStuff(p)
              slotStuffList <- getSlotStuff(camStuffTriples.map(a => a._2).distinct.toList)
              _ <- ZIO.foreach(camStuffTriples.map(a => a._1).distinct) { sourceId =>
                for {
                  abbreviatedNodeType <- applyPrefix(sourceId)
                  nodeDetails <- getKnowledgeGraphNodeDetails(s"<$sourceId>")
                  nodeDetailsHead <- Task.effect(nodeDetails.head)
                  _ = {
                    val attribute = TRAPINodeAttribute(
                      None,
                      abbreviatedNodeType.substring(abbreviatedNodeType.indexOf(":") + 1, abbreviatedNodeType.length),
                      abbreviatedNodeType,
                      Some(sourceId),
                      Some(abbreviatedNodeType.substring(0, abbreviatedNodeType.indexOf(":")))
                    )
                    kgNodes += TRAPINode(Some(nodeDetailsHead._1), nodeDetailsHead._2.sorted, List[TRAPINodeAttribute](attribute))
                    kgExtraNodes += TRAPINodeBinding(None, abbreviatedNodeType)
                  }
                } yield ()
              }
              _ <- ZIO.foreach(camStuffTriples) { triple =>
                for {
                  prefixedSource <- applyPrefix(triple._1)
                  prefixedTarget <- applyPrefix(triple._3)
                  prefixedPredicate <- applyPrefix(triple._2)
                  _ = {
                    val edge = NewTRAPIEdge(triple._2, triple._1, triple._3).asJson.deepDropNullValues.noSpaces
                    val knowledgeGraphId =
                      String.format("%064x", new BigInteger(1, messageDigest.digest(edge.getBytes(StandardCharsets.UTF_8))))
                    val resolvedType = slotStuffList.filter(a => a._2.equals(triple._2)).map(a => a._4).headOption.getOrElse(prefixedPredicate)
                    kgEdges += TRAPIEdge(knowledgeGraphId, Some(resolvedType), prefixedSource, prefixedTarget)
                    kgExtraEdges += TRAPIEdgeBinding(None, knowledgeGraphId, Some(p))
                  }
                } yield ()
              }
            } yield ()
          }

        } yield TRAPIResult(nodeBindings, edgeBindings, Some(kgExtraNodes.toList), Some(kgExtraEdges.toList))
      }
    } yield TRAPIMessage(Some(queryGraph), Some(TRAPIKnowledgeGraph(kgNodes.toList, kgEdges.toList)), results)

  def getProvenance(source: String, predicate: String, target: String): RIO[ZConfig[AppConfig], String] =
    for {
      prefixMap <- readPrefixes
      prefixes <- Task.effect(prefixMap.map(entry => s"PREFIX ${entry._1}: <${entry._2}>").mkString("\n"))
      queryText <- Task.effect(
        s"""$prefixes
          |SELECT ?g ?other WHERE {
          |GRAPH ?g { <$source> <$predicate> <$target> } OPTIONAL { ?g prov:wasDerivedFrom ?other . }
          |}""".stripMargin
      )
      query <- Task.effect(QueryFactory.create(queryText))
      bindings <- SPARQLQueryExecutor.runSelectQuery(query)
      nextSolution = bindings.nextSolution()
      prov <- Task.effect(nextSolution.get("other").toString).orElse(Task.effect(nextSolution.get("g").toString))
    } yield prov

  def getKnowledgeGraphNodeDetails(nodeIds: String): RIO[ZConfig[AppConfig], Map[String, List[String]]] =
    for {
      prefixMap <- readPrefixes
      prefixes <- Task.effect(prefixMap.map(entry => s"PREFIX ${entry._1}: <${entry._2}>").mkString("\n"))
      queryText <- Task.effect(
        s"""$prefixes
            |SELECT DISTINCT ?kid ?blclass ?label WHERE {
            |VALUES ?kid { $nodeIds }
            |?kid rdfs:subClassOf ?blclass .
            |?blclass blml:is_a* bl:NamedThing .
            |OPTIONAL { ?kid rdfs:label ?label . }
            |}""".stripMargin
      )
      query <- Task.effect(QueryFactory.create(queryText))
      resultSet <- SPARQLQueryExecutor.runSelectQuery(query)
      map <- Task.effect(
        resultSet.asScala.toList.map(qs => (qs.get("label").toString, qs.get("blclass").toString)).groupBy(_._1).map {
          case (k, v) =>
            (k, v.map(a => CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, a._2.substring(a._2.lastIndexOf("/") + 1, a._2.length))))
        }
      )
    } yield map

  def getCAMStuff(prov: String): RIO[ZConfig[AppConfig], List[(String, String, String)]] =
    for {
      prefixMap <- readPrefixes
      prefixes <- Task.effect(prefixMap.map(entry => s"PREFIX ${entry._1}: <${entry._2}>").mkString("\n"))
      queryText <- Task.effect(
        s"""$prefixes
           |SELECT DISTINCT ?s_type ?p ?o_type WHERE {
           |GRAPH <$prov> {
           |?s ?p ?o .
           |?s rdf:type owl:NamedIndividual .
           |?o rdf:type owl:NamedIndividual .
           |}
           |?o sesame:directType ?o_type .
           |?s sesame:directType ?s_type .
           |}""".stripMargin
      )
      query <- Task.effect(QueryFactory.create(queryText))
      resultSet <- SPARQLQueryExecutor.runSelectQuery(query)
      response <-
        Task.effect(resultSet.asScala.toList.map(qs => (qs.get("s_type").toString, qs.get("p").toString, qs.get("o_type").toString)).toList)
    } yield response

  def getSlotStuff(predicateMap: List[String]): RIO[ZConfig[AppConfig], List[(String, String, String, String)]] =
    for {
      prefixMap <- readPrefixes
      prefixes <- Task.effect(prefixMap.map(entry => s"PREFIX ${entry._1}: <${entry._2}>").mkString("\n"))
      values <- Task.effect(
        predicateMap.zipWithIndex
          .map(a => String.format(" ( <%s> \"e%s\" ) ", a._1, StringUtils.leftPad(a._2.toString, 4, '0')))
          .mkString(" "))
      queryText <- Task.effect(
        s"""$prefixes
          |SELECT DISTINCT ?qid ?kid ?blslot ?label WHERE {
          |VALUES (?kid ?qid) { $values }
          |?blslot <http://reasoner.renci.org/vocab/slot_mapping> ?kid .
          |FILTER NOT EXISTS {
          |?other <http://reasoner.renci.org/vocab/slot_mapping> ?kid .
          |?other blml:is_a+/blml:mixins* ?blslot .
          |} OPTIONAL { ?kid rdfs:label ?label . }
          |}""".stripMargin
      )
      _ = logger.debug("queryText: {}", queryText)
      query <- Task.effect(QueryFactory.create(queryText))
      resultSet <- SPARQLQueryExecutor.runSelectQuery(query)
      response <-
        Task.effect(resultSet.asScala.toList.map(qs => (qs.get("qid").toString, qs.get("kid").toString, qs.get("blslot").toString, qs.get("label").toString)).toList.distinct)
    } yield response

}

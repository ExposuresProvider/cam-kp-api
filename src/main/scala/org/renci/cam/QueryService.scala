package org.renci.cam

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe.parser.parse
import io.circe.syntax._
import org.apache.commons.io.IOUtils
import org.apache.commons.lang3.StringUtils
import org.apache.jena.ext.com.google.common.base.CaseFormat
import org.apache.jena.query.{QueryFactory, QuerySolution, ResultSet}
import org.renci.cam.domain._
import zio.config.ZConfig
import zio.{RIO, Task, ZIO, config => _}

import scala.collection.JavaConverters._
import scala.collection.mutable

object QueryService extends LazyLogging {

  final case class NewTRAPIEdge(`type`: Option[String], source_id: String, target_id: String)

  final case class Triple(subj: String, pred: String, obj: String)

  def convertCase(v: String): String = CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, v)

  def getNodeTypes(nodes: List[TRAPIQueryNode]): Map[String, String] =
    nodes
      .map(node =>
        (node.`type`, node.curie) match {
          case (Some(t), Some(c)) => (node.id, c)
          case (None, Some(c)) => (node.id, c)
          case (Some(t), None) => (node.id, "bl:" + CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, t))
          case (None, None) => (node.id, "")
        })
      .toMap

  def readPrefixes: Task[Map[String, String]] =
    for {
      legacyJSONString <-
        Task.effect(IOUtils.resourceToString("legacy_prefixes.json", StandardCharsets.UTF_8, this.getClass.getClassLoader))
      legacyJSON <- ZIO.fromEither(parse(legacyJSONString))
      legacyMap = legacyJSON.as[Map[String, String]].getOrElse(Map.empty)
      jsonString <- Task.effect(IOUtils.resourceToString("prefixes.json", StandardCharsets.UTF_8, this.getClass.getClassLoader))
      json <- ZIO.fromEither(parse(jsonString))
      map = json.as[Map[String, String]].getOrElse(Map.empty)
    } yield legacyMap.++(map)

  def applyPrefix(value: String, prefixes: Map[String, String]): String =
    prefixes
      .filter(entry => value.startsWith(entry._2))
      .map(entry => s"${entry._1}:" + value.substring(entry._2.length, value.length))
      .headOption
      .getOrElse(value)

  private def queryEdgePredicates(edge: TRAPIQueryEdge): RIO[ZConfig[AppConfig], (Set[String], Set[(String, String)], String)] =
    for {
      edgeType <- ZIO.fromOption(edge.`type`).orElseFail(new Exception("failed to get edge type"))
      queryText = s"""PREFIX bl: <https://w3id.org/biolink/vocab/>
           |SELECT DISTINCT ?predicate WHERE {
           |bl:$edgeType <http://reasoner.renci.org/vocab/slot_mapping> ?predicate .
           |}""".stripMargin
      _ = logger.warn("queryText: {}", queryText)
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

  def run(limit: Int, queryGraph: TRAPIQueryGraph): RIO[ZConfig[AppConfig], ResultSet] = {
    val nodeTypes = getNodeTypes(queryGraph.nodes)
    for {
      predicates <- ZIO.foreachPar(queryGraph.edges.filter(_.`type`.nonEmpty))(queryEdgePredicates)
      (instanceVars, instanceVarsToTypes, sparqlLines) = predicates.unzip3
      whereClauseParts =
        queryGraph.nodes
          .map(node => String.format("  ?%1$s sesame:directType ?%1$s_type .", node.id))
          .mkString("\n")
      whereClause = s"WHERE { \n$whereClauseParts"
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
      prefixes = prefixMap.map { case (prefix, expansion) => s"PREFIX $prefix: <$expansion>" }.mkString("\n")
      queryString = s"$prefixes\n$selectClause\n$whereClause\n$valuesClause \n } $limitSparql"
      query <- Task.effect(QueryFactory.create(queryString))
      response <- SPARQLQueryExecutor.runSelectQuery(query)
    } yield response
  }

  def parseResultSet(queryGraph: TRAPIQueryGraph, resultSet: ResultSet): RIO[ZConfig[AppConfig], TRAPIMessage] =
    for {
      prefixes <- readPrefixes
      messageDigest <- Task.effect(MessageDigest.getInstance("SHA-256"))

      trapiKGNodes <- Task.effect(mutable.ListBuffer[TRAPINode]())
      trapiKGEdges <- Task.effect(mutable.ListBuffer[TRAPIEdge]())

      initialKGNodes <- getTRAPINodes(queryGraph, resultSet, prefixes)
      initialKGEdges <- getTRAPIEdges(queryGraph, resultSet, prefixes, messageDigest)

      _ = {
        trapiKGNodes ++= initialKGNodes
        trapiKGEdges ++= initialKGEdges
      }

      results <- ZIO.foreach(resultSet.asScala.toList) { querySolution =>
        for {
          trapiKGNodeBindings <- getTRAPINodeBindings(queryGraph, querySolution, prefixes)
          trapiKGEdgeBindings <- getTRAPIEdgeBindings(queryGraph, querySolution, prefixes, messageDigest)

          map <- ZIO.foreach(trapiKGEdgeBindings.map(a => a.provenance.get)) { prov =>
            for {
              camStuffTriples <- getCAMStuff(prov)
            } yield (prov, camStuffTriples)
          }
          prov2CAMStuffTripleMap = map.toMap

          extraKGNodes <- ZIO.foreach(trapiKGEdgeBindings.map(a => a.provenance.get)) { prov =>
            for {
              camStuffTriples <- ZIO.fromOption(prov2CAMStuffTripleMap.get(prov)).orElseFail(new Exception("failed to get cam triples"))
              slotStuffNodeDetails <- getTRAPINodeDetails(camStuffTriples.map(a => a.subj).distinct.map(v => s"<$v>"))
              extraKGNodes <- getExtraKGNodes(camStuffTriples, slotStuffNodeDetails, prefixes)
            } yield extraKGNodes
          }
          _ = trapiKGNodes ++= extraKGNodes.flatten

          extraKGEdges <- ZIO.foreach(trapiKGEdgeBindings.map(a => a.provenance.get)) { prov =>
            for {
              camStuffTriples <- ZIO.fromOption(prov2CAMStuffTripleMap.get(prov)).orElseFail(new Exception("failed to get cam triples"))
              slotStuffList <- getSlotStuff(camStuffTriples.map(a => a.pred).distinct)
              edges <- ZIO.foreach(camStuffTriples) { triple =>
                for {
                  edgeKey <- Task.effect(NewTRAPIEdge(Some(triple.pred), triple.subj, triple.obj).asJson.deepDropNullValues.noSpaces)
                  knowledgeGraphId =
                    String.format("%064x", new BigInteger(1, messageDigest.digest(edgeKey.getBytes(StandardCharsets.UTF_8))))
                  resolvedType =
                    slotStuffList
                      .filter(a => a._2.equals(triple.pred))
                      .map(a => a._4)
                      .headOption
                      .getOrElse(applyPrefix(triple.pred, prefixes))
                  edge =
                    TRAPIEdge(knowledgeGraphId, applyPrefix(triple.subj, prefixes), applyPrefix(triple.obj, prefixes), Some(resolvedType))
                } yield edge
              }
            } yield edges
          }
          _ = trapiKGEdges ++= extraKGEdges.flatten

          extraKGNodeBindings <- ZIO.foreach(trapiKGEdgeBindings.map(a => a.provenance.get)) { prov =>
            for {
              camStuffTriples <- ZIO.fromOption(prov2CAMStuffTripleMap.get(prov)).orElseFail(new Exception("failed to get cam triples"))
              nodeBindings = camStuffTriples.map(a => a.subj).distinct.map(a => TRAPINodeBinding(None, applyPrefix(a, prefixes)))
            } yield nodeBindings
          }

          extraKGEdgeBindings <- ZIO.foreach(trapiKGEdgeBindings.map(a => a.provenance.get)) { prov =>
            for {
              camStuffTriples <- ZIO.fromOption(prov2CAMStuffTripleMap.get(prov)).orElseFail(new Exception("failed to get cam triples"))
              slotStuffList <- getSlotStuff(camStuffTriples.map(a => a.pred).distinct)
              edgeBindings <- ZIO.foreach(camStuffTriples) { triple =>
                for {
                  edgeKey <- Task.effect(NewTRAPIEdge(Some(triple.pred), triple.subj, triple.obj).asJson.deepDropNullValues.noSpaces)
                  knowledgeGraphId =
                    String.format("%064x", new BigInteger(1, messageDigest.digest(edgeKey.getBytes(StandardCharsets.UTF_8))))
                  edgeBinding = TRAPIEdgeBinding(None, knowledgeGraphId, Some(prov))
                } yield edgeBinding
              }
            } yield edgeBindings
          }

        } yield TRAPIResult(trapiKGNodeBindings, trapiKGEdgeBindings, Some(extraKGNodeBindings.flatten), Some(extraKGEdgeBindings.flatten))
      }
    } yield TRAPIMessage(Some(queryGraph), Some(TRAPIKnowledgeGraph(trapiKGNodes.toList, trapiKGEdges.toList)), Some(results))

  private def getTRAPINodes(queryGraph: TRAPIQueryGraph,
                            resultSet: ResultSet,
                            prefixes: Map[String, String]): RIO[ZConfig[AppConfig], List[TRAPINode]] =
    for {
      trapiNodes <- ZIO.foreach(resultSet.asScala.toList) { querySolution =>
        for {
          nodeMap <- Task.effect(queryGraph.nodes.map(n => (n.id, querySolution.get(s"${n.id}_type").toString)).toMap)
          nodeDetails <- getTRAPINodeDetails(nodeMap.values.map(v => s"<$v>").toList)
          nodes <- ZIO.foreach(queryGraph.nodes) { n =>
            for {
              nodeIRI <- ZIO.fromOption(nodeMap.get(n.id)).orElseFail(new Exception(s"Missing node IRI: ${n.id}"))
              nodeDetailTypesMap <- Task.effect(
                nodeDetails
                  .filter(a => nodeIRI.equals(a.subj))
                  .map(a => (a.pred, a.obj))
                  .groupBy(_._1)
                  .map({ case (k, v) => (k, v.map(a => a._2)) })
              )
              nodeDetailTypes <- ZIO.fromOption(nodeDetailTypesMap.headOption).orElseFail(new Exception("failed to get details"))
              abbreviatedNodeType = applyPrefix(nodeIRI, prefixes)
              attribute = TRAPINodeAttribute(
                None,
                abbreviatedNodeType.substring(abbreviatedNodeType.indexOf(":") + 1, abbreviatedNodeType.length),
                abbreviatedNodeType,
                nodeMap.get(n.id),
                Some(abbreviatedNodeType.substring(0, abbreviatedNodeType.indexOf(":")))
              )
              trapiNode = TRAPINode(Some(n.id), nodeDetailTypes._2.sorted, List[TRAPINodeAttribute](attribute))
            } yield trapiNode
          }
        } yield nodes
      }
    } yield trapiNodes.flatten

  private def getTRAPIEdges(queryGraph: TRAPIQueryGraph,
                            resultSet: ResultSet,
                            prefixes: Map[String, String],
                            messageDigest: MessageDigest): RIO[ZConfig[AppConfig], List[TRAPIEdge]] =
    for {
      trapiEdges <- ZIO.foreach(resultSet.asScala.toList) { querySolution =>
        for {
          nodeMap <- Task.effect(queryGraph.nodes.map(n => (n.id, querySolution.get(s"${n.id}_type").toString)).toMap)
          edges <- ZIO.foreach(queryGraph.edges) { e =>
            for {
              sourceId <- ZIO.fromOption(nodeMap.get(e.source_id)).orElseFail(new Exception("could not get source id"))
              targetId <- ZIO.fromOption(nodeMap.get(e.target_id)).orElseFail(new Exception("could not get target id"))
              newTRAPIEdge =
                NewTRAPIEdge(e.`type`, e.source_id, e.target_id).asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
              encodedTRAPIEdge = String.format("%064x", new BigInteger(1, messageDigest.digest(newTRAPIEdge)))
              trapiEdge = TRAPIEdge(encodedTRAPIEdge, applyPrefix(sourceId, prefixes), applyPrefix(targetId, prefixes), e.`type`)
            } yield trapiEdge
          }
        } yield edges
      }
    } yield trapiEdges.flatten

  private def getTRAPINodeBindings(queryGraph: TRAPIQueryGraph,
                                   querySolution: QuerySolution,
                                   prefixes: Map[String, String]): RIO[ZConfig[AppConfig], List[TRAPINodeBinding]] =
    for {
      nodeMap <- Task.effect(queryGraph.nodes.map(n => (n.id, querySolution.get(s"${n.id}_type").toString)).toMap)
      nodeBindings <- ZIO.foreach(queryGraph.nodes) { n =>
        for {
          nodeIRI <- ZIO.fromOption(nodeMap.get(n.id)).orElseFail(new Exception(s"Missing node IRI: ${n.id}"))
          nodeBinding <- Task.effect(TRAPINodeBinding(Some(n.id), applyPrefix(nodeIRI, prefixes)))
        } yield nodeBinding
      }
    } yield nodeBindings

  private def getTRAPIEdgeBindings(queryGraph: TRAPIQueryGraph,
                                   querySolution: QuerySolution,
                                   prefixes: Map[String, String],
                                   messageDigest: MessageDigest): RIO[ZConfig[AppConfig], List[TRAPIEdgeBinding]] =
    for {
      nodeMap <- Task.effect(queryGraph.nodes.map(n => (n.id, querySolution.get(s"${n.id}_type").toString)).toMap)
      edgeBindings <- ZIO.foreach(queryGraph.edges) { e =>
        for {
          predicateRDFNode <- Task.effect(querySolution.get(e.id).toString)
          sourceRDFNode <- Task.effect(querySolution.get(e.source_id).toString)
          targetRDFNode <- Task.effect(querySolution.get(e.target_id).toString)
          sourceId <- ZIO.fromOption(nodeMap.get(e.source_id)).orElseFail(new Exception("could not get source id"))
          targetId <- ZIO.fromOption(nodeMap.get(e.target_id)).orElseFail(new Exception("could not get target id"))
          newTRAPIEdge =
            NewTRAPIEdge(e.`type`, e.source_id, e.target_id).asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
          encodedTRAPIEdge = String.format("%064x", new BigInteger(1, messageDigest.digest(newTRAPIEdge)))
          prov <- getProvenance(sourceRDFNode, predicateRDFNode, targetRDFNode)
          trapiEdgeBinding = TRAPIEdgeBinding(Some(e.id), encodedTRAPIEdge, Some(prov))
        } yield trapiEdgeBinding
      }
    } yield edgeBindings

  private def getExtraKGNodes(camStuffTriples: List[Triple],
                              slotStuffNodeDetails: List[Triple],
                              prefixes: Map[String, String]): RIO[ZConfig[AppConfig], List[TRAPINode]] =
    for {
      extraKGNodes <- ZIO.foreach(camStuffTriples.map(a => a.subj).distinct) { sourceId =>
        for {
          slotStuffNodeDetailTypes <-
            ZIO
              .fromOption(
                slotStuffNodeDetails
                  .filter(a => sourceId.equals(a.subj))
                  .map(a => (a.pred, a.obj))
                  .groupBy(_._1)
                  .map({ case (k, v) => (k, v.map(a => a._2)) })
                  .headOption)
              .orElseFail(new Exception("failed to get slot node detail types"))
          attribute = {
            val abbreviatedNodeType = applyPrefix(sourceId, prefixes)
            TRAPINodeAttribute(
              None,
              abbreviatedNodeType.substring(abbreviatedNodeType.indexOf(":") + 1, abbreviatedNodeType.length),
              abbreviatedNodeType,
              Some(sourceId),
              Some(abbreviatedNodeType.substring(0, abbreviatedNodeType.indexOf(":")))
            )
          }
          kgNode = TRAPINode(Some(slotStuffNodeDetailTypes._1), slotStuffNodeDetailTypes._2.sorted, List[TRAPINodeAttribute](attribute))
        } yield kgNode
      }
    } yield extraKGNodes

  private def getProvenance(source: String, predicate: String, target: String): RIO[ZConfig[AppConfig], String] =
    for {
      prefixMap <- readPrefixes
      prefixes = prefixMap.map { case (prefix, expansion) => s"PREFIX $prefix: <$expansion>" }.mkString("\n")
      queryText = s"""$prefixes
          |SELECT ?g ?other WHERE {
          |GRAPH ?g { <$source> <$predicate> <$target> } OPTIONAL { ?g prov:wasDerivedFrom ?other . }
          |}""".stripMargin
      query <- Task.effect(QueryFactory.create(queryText))
      bindings <- SPARQLQueryExecutor.runSelectQuery(query)
      nextSolution = bindings.nextSolution()
      prov <- Task.effect(nextSolution.get("other").toString).orElse(Task.effect(nextSolution.get("g").toString))
    } yield prov

  private def getTRAPINodeDetails(nodeIdList: List[String]): RIO[ZConfig[AppConfig], List[Triple]] =
    for {
      prefixMap <- readPrefixes
      prefixes = prefixMap.map { case (prefix, expansion) => s"PREFIX $prefix: <$expansion>" }.mkString("\n")
      nodeIds = nodeIdList.mkString(" ")
      queryText = s"""$prefixes
            |SELECT DISTINCT ?kid ?blclass ?label WHERE {
            |VALUES ?kid { $nodeIds }
            |?kid rdfs:subClassOf ?blclass .
            |?blclass blml:is_a* bl:NamedThing .
            |OPTIONAL { ?kid rdfs:label ?label . }
            |}""".stripMargin
      query <- Task.effect(QueryFactory.create(queryText))
      resultSet <- SPARQLQueryExecutor.runSelectQuery(query)
      results <- Task.effect(
        resultSet.asScala.toList.map { qs =>
          val blClass = qs.get("blclass").toString
          Triple(qs.get("kid").toString,
                 qs.get("label").toString,
                 convertCase(blClass.substring(blClass.lastIndexOf("/") + 1, blClass.length)))
        }
      )
    } yield results

  private def getCAMStuff(prov: String): RIO[ZConfig[AppConfig], List[Triple]] =
    for {
      prefixMap <- readPrefixes
      prefixes = prefixMap.map { case (prefix, expansion) => s"PREFIX $prefix: <$expansion>" }.mkString("\n")
      queryText = s"""$prefixes
           |SELECT DISTINCT (?s_type AS ?subj) (?p AS ?pred) (?o_type AS ?obj) WHERE {
           |GRAPH <$prov> {
           |?s ?p ?o .
           |?s rdf:type owl:NamedIndividual .
           |?o rdf:type owl:NamedIndividual .
           |}
           |?o sesame:directType ?o_type .
           |?s sesame:directType ?s_type .
           |}""".stripMargin
      query <- Task.effect(QueryFactory.create(queryText))
      resultSet <- SPARQLQueryExecutor.runSelectQuery(query)
      // was throw error...something about 'variable binding is not a literal'
//      triples <- SPARQLQueryExecutor.runSelectQueryAs[Triple](query)
//      _ = logger.warn("triples: {}", triples)
      triples <- Task.effect(
        resultSet.asScala.toList
          .map(qs => Triple(qs.get("subj").toString, qs.get("pred").toString, qs.get("obj").toString))
          .distinct)
    } yield triples

  private def getSlotStuff(predicateMap: List[String]): RIO[ZConfig[AppConfig], List[(String, String, String, String)]] =
    for {
      prefixMap <- readPrefixes
      prefixes = prefixMap.map { case (prefix, expansion) => s"PREFIX $prefix: <$expansion>" }.mkString("\n")
      values =
        predicateMap.zipWithIndex
          .map(a => String.format(" ( <%s> \"e%s\" ) ", a._1, StringUtils.leftPad(a._2.toString, 4, '0')))
          .mkString(" ")
      queryText = s"""$prefixes
          |SELECT DISTINCT ?qid ?kid ?blslot ?label WHERE {
          |VALUES (?kid ?qid) { $values }
          |?blslot <http://reasoner.renci.org/vocab/slot_mapping> ?kid .
          |FILTER NOT EXISTS {
          |?other <http://reasoner.renci.org/vocab/slot_mapping> ?kid .
          |?other blml:is_a+/blml:mixins* ?blslot .
          |} OPTIONAL { ?kid rdfs:label ?label . }
          |}""".stripMargin
      query <- Task.effect(QueryFactory.create(queryText))
      resultSet <- SPARQLQueryExecutor.runSelectQuery(query)
      response <- Task.effect(
        resultSet.asScala.toList
          .map(qs => (qs.get("qid").toString, qs.get("kid").toString, qs.get("blslot").toString, qs.get("label").toString))
          .distinct)
    } yield response

}

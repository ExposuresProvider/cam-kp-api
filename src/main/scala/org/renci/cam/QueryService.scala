package org.renci.cam

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe.syntax._
import org.apache.commons.io.IOUtils
import org.apache.jena.ext.com.google.common.base.CaseFormat
import org.apache.jena.query.{QueryFactory, ResultSet}
import org.renci.cam.domain._
import zio.config.ZConfig
import zio.{RIO, Task, ZIO, config => _}
import io.circe._
import io.circe.parser.{parse, _}

import scala.collection.JavaConverters._
import scala.collection.mutable

object QueryService extends LazyLogging {

  case class NewTranslatorEdge(`type`: String, source_id: String, target_id: String)

  def getNodeTypes(nodes: List[TranslatorQueryNode]) =
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

  def run(limit: Int, queryGraph: TranslatorQueryGraph): RIO[Config[AppConfig], ResultSet] =
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
      valuesClause = {
        var bindings = mutable.ListBuffer[String]()
        bindings += sparqlLines.mkString("")
        instanceVarsToTypes.foreach(a =>
          a.foreach(b =>
            nodeTypes.get(b._2) match {
              case Some(v) => bindings += s"?${b._1} rdf:type $v ."
              case None => bindings += ""
            }))
        s"${bindings.mkString("\n")}"
      }
      limitSparql = if (limit > 0) s" LIMIT $limit" else ""
      prefixMap <- readPrefixes
      prefixes <- Task.effect(prefixMap.map(entry => s"PREFIX ${entry._1}: <${entry._2}>").mkString("\n"))
      _ = logger.debug("prefixes: {}", prefixes)
      queryString = s"$prefixes\n$selectClause\n$whereClause\n$valuesClause \n } $limitSparql"
      query <- Task.effect(QueryFactory.create(queryString))
      response <- SPARQLQueryExecutor.runSelectQuery(query)
    } yield response

  def parseResultSet(queryGraph: TranslatorQueryGraph, resultSet: ResultSet): RIO[ZConfig[AppConfig], TranslatorMessage] =
    for {
      kgNodes <- Task.effect(mutable.ListBuffer[TranslatorNode]())
      kgEdges <- Task.effect(mutable.ListBuffer[TranslatorEdge]())
      results <- ZIO.foreach(resultSet.asScala.toList) { querySolution =>
        for {
          nodeMap <- Task.effect(queryGraph.nodes.map(n => (n.id, querySolution.get(s"${n.id}_type").toString)).toMap)
          nodeBindings <- ZIO.foreach(queryGraph.nodes) { n =>
            for {
              abbreviatedNodeType <- applyPrefix(nodeMap.get(n.id).get)
              nodeDetails <- getKnowledgeGraphNodeDetails(String.format("<%s>", nodeMap.get(n.id).get))
              nodeDetailsHead <- Task.effect(nodeDetails.head)
              _ = kgNodes += TranslatorNode(abbreviatedNodeType,
                                            Some(nodeDetailsHead._1),
                                            nodeDetailsHead._2,
                                            List[TranslatorNodeAttribute]())
              nodeBinding <- Task.effect(TranslatorNodeBinding(n.id, abbreviatedNodeType))
            } yield nodeBinding
          }
          edgeBindings <- ZIO.foreach(queryGraph.edges) { e =>
            for {
              predicateRDFNode <- Task.effect(querySolution.get(e.id).toString)
              sourceRDFNode <- Task.effect(querySolution.get(e.source_id).toString)
              targetRDFNode <- Task.effect(querySolution.get(e.target_id).toString)
              knowledgeGraphId = {
                val newTranslatorEdge =
                  NewTranslatorEdge(e.`type`, e.source_id, e.target_id).asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
                val messageDigest = MessageDigest.getInstance("SHA-256")
                String.format("%064x", new BigInteger(1, messageDigest.digest(newTranslatorEdge)))
              }
              prefixedSource <- applyPrefix(nodeMap.get(e.source_id).get)
              prefixedTarget <- applyPrefix(nodeMap.get(e.target_id).get)
              _ = kgEdges += TranslatorEdge(knowledgeGraphId, Some(e.`type`), prefixedSource, prefixedTarget)
              prov <- getProvenance(sourceRDFNode, predicateRDFNode, targetRDFNode)
            } yield TranslatorEdgeBinding(e.id, knowledgeGraphId, Some(prov.toString))
          }
        } yield TranslatorResult(nodeBindings, edgeBindings)
      }
    } yield TranslatorMessage(Some(queryGraph), Some(TranslatorKnowledgeGraph(kgNodes.toList, kgEdges.toList)), results)

  def getProvenance(source: String, predicate: String, target: String): RIO[ZConfig[AppConfig], String] =
    for {
      prefixMap <- readPrefixes
      prefixes <- Task.effect(prefixMap.map(entry => s"PREFIX ${entry._1}: <${entry._2}>").mkString("\n"))
      _ = logger.debug("prefixes: {}", prefixes)
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
      _ = logger.debug("prefixes: {}", prefixes)
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

  def getCAMStuffQuery(prov: String): RIO[ZConfig[AppConfig], String] =
    for {
      prefixMap <- readPrefixes
      prefixes <- Task.effect(prefixMap.map(entry => s"PREFIX ${entry._1}: <${entry._2}>").mkString("\n"))
      _ = logger.debug("prefixes: {}", prefixes)
      queryText <- Task.effect(
        s"""$prefixes
           |SELECT ?s_type ?p ?o_type WHERE {
           |GRAPH <{prov}> { {
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
      response <- Task.effect(resultSet.asScala.toList.map(qs => (qs.get("s").toString, qs.get("p").toString, qs.get("o").toString)).toList)
    } yield response

}

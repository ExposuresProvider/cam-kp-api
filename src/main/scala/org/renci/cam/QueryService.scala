package org.renci.cam

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe.parser.parse
import io.circe.syntax._
import java.io

import org.apache.commons.io.IOUtils
import org.apache.commons.lang3.StringUtils
import org.apache.jena.ext.com.google.common.base.CaseFormat
import org.apache.jena.ext.xerces.util.URI
import org.apache.jena.query.{QueryFactory, QuerySolution, ResultSet}
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.Utilities._
import org.renci.cam.domain._
import zio.config.ZConfig
import zio.{Has, RIO, Task, URIO, ZIO, config => _}

import scala.collection.JavaConverters._

object QueryService extends LazyLogging {

  final case class TRAPIEdgeKey(`type`: Option[String], source_id: String, target_id: String)

  final case class Triple(subj: URI, pred: URI, obj: URI)

  final case class TripleString(subj: String, pred: String, obj: String)

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

//  def readPrefixes: URIO[Has[PrefixesMap], PrefixesMap] = biolinkPrefixes
//    for {
//      legacyJSONString <-
//        Task.effect(IOUtils.resourceToString("legacy_prefixes.json", StandardCharsets.UTF_8, this.getClass.getClassLoader))
//      legacyJSON <- ZIO.fromEither(parse(legacyJSONString))
//      legacyMap = legacyJSON.as[Map[String, String]].getOrElse(Map.empty)
//      jsonString <- Task.effect(IOUtils.resourceToString("prefixes.json", StandardCharsets.UTF_8, this.getClass.getClassLoader))
//      json <- ZIO.fromEither(parse(jsonString))
//      map = json.as[Map[String, String]].getOrElse(Map.empty)
//    } yield map

  def applyPrefix(value: String, prefixes: Map[String, String]): String =
    prefixes
      .filter(entry => value.startsWith(entry._2))
      .map(entry => StringUtils.prependIfMissing(value.substring(entry._2.length, value.length), s"${entry._1}:"))
      .headOption
      .getOrElse(value)

  private def queryEdgePredicates(
    edge: TRAPIQueryEdge): RIO[ZConfig[AppConfig] with HttpClient, (Set[String], Set[(String, String)], String)] =
    for {
      edgeType <- ZIO.fromOption(edge.`type`).orElseFail(new Exception("failed to get edge type"))
      queryText = s"""PREFIX bl: <https://w3id.org/biolink/vocab/>
           |SELECT DISTINCT ?predicate WHERE {
           |?predicate <http://www.w3.org/2000/01/rdf-schema#subPropertyOf>+ bl:$edgeType .
           |FILTER NOT EXISTS { ?predicate a <https://w3id.org/biolink/biolinkml/meta/types/SlotDefinition> }
           |}""".stripMargin
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

  def run(limit: Int, queryGraph: TRAPIQueryGraph): RIO[ZConfig[AppConfig] with HttpClient with Has[PrefixesMap], ResultSet] = {
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
      prefixMap <- biolinkPrefixes
      prefixes = prefixMap.prefixesMap.map { case (prefix, expansion) => s"PREFIX $prefix: <$expansion>" }.mkString("\n")
      queryString = s"$prefixes\n$selectClause\n$whereClause\n$valuesClause \n } $limitSparql"
      query <- Task.effect(QueryFactory.create(queryString))
      response <- SPARQLQueryExecutor.runSelectQuery(query)
    } yield response
  }

  def parseResultSet(queryGraph: TRAPIQueryGraph,
                     resultSet: ResultSet): RIO[ZConfig[AppConfig] with HttpClient with Has[PrefixesMap], TRAPIMessage] =
    for {
      prefixes <- biolinkPrefixes
      querySolutions = resultSet.asScala.toList
      //FIXME these initial nodes have incorrect 'name' properties; should not be query graph name
      initialKGNodes <- getTRAPINodes(queryGraph, querySolutions, prefixes.prefixesMap)
      initialKGEdges <- getTRAPIEdges(queryGraph, querySolutions, prefixes.prefixesMap)
      querySolutionsToEdgeBindings <- getTRAPIEdgeBindingsMany(queryGraph, querySolutions)
      trapiBindings <- ZIO.foreach(querySolutions) { querySolution =>
        getTRAPINodeBindings(queryGraph, querySolution, prefixes.prefixesMap) zip Task.effect(querySolutionsToEdgeBindings(querySolution))
      }
      allEdgeBindings = trapiBindings.flatMap(_._2)
      allCamIds = allEdgeBindings.toSet[TRAPIEdgeBinding].flatMap(_.provenance)
      prov2CAMStuffTripleMap <- ZIO.foreachPar(allCamIds)(prov => getCAMStuff(prov).map(prov -> _)).map(_.toMap)
      allCAMTriples = prov2CAMStuffTripleMap.values.toSet.flatten
      allTripleNodes = allCAMTriples.flatMap(t => Set(t.subj, t.obj))
      slotStuffNodeDetails <- getTRAPINodeDetails(allTripleNodes.map(v => s"<$v>").toList)
      extraKGNodes = getExtraKGNodes(allTripleNodes, slotStuffNodeDetails, prefixes.prefixesMap)
      allPredicates = allCAMTriples.map(_.pred)
      slotStuffList <- getSlotStuff(allPredicates.toList)
      extraKGEdges = allCAMTriples.map { triple =>
        val edgeKey = TRAPIEdgeKey(Some(triple.pred), triple.subj, triple.obj).asJson.deepDropNullValues.noSpaces
        val knowledgeGraphId = String.format("%064x", new BigInteger(1, messageDigest.digest(edgeKey.getBytes(StandardCharsets.UTF_8))))
        val resolvedType = slotStuffList
          .filter(a => a._2.equals(triple.pred))
          .map(a => a._4)
          .headOption
          .getOrElse(applyPrefix(triple.pred, prefixes.prefixesMap))
        TRAPIEdge(knowledgeGraphId,
                  applyPrefix(triple.subj, prefixes.prefixesMap),
                  applyPrefix(triple.obj, prefixes.prefixesMap),
                  Some(resolvedType))
      }
      results = trapiBindings.map { case (resultNodeBindings, resultEdgeBindings) =>
        val provsAndCamTriples =
          resultEdgeBindings.flatMap(_.provenance).map(prov => prov -> prov2CAMStuffTripleMap.get(prov).toSet.flatten).toMap
        val nodes = provsAndCamTriples.values.flatten.toSet[TripleString].flatMap(t => Set(t.subj, t.obj))
        val extraKGNodeBindings = nodes.map(n => TRAPINodeBinding(None, applyPrefix(n.toString, prefixes.prefixesMap)))
        val extraKGEdgeBindings = provsAndCamTriples.flatMap { case (prov, triples) =>
          triples.map { triple =>
            val edgeKey =
              TRAPIEdgeKey(Some(triple.pred.toString), triple.subj.toString, triple.obj.toString).asJson.deepDropNullValues.noSpaces
            val kgId = String.format("%064x", new BigInteger(1, messageDigest.digest(edgeKey.getBytes(StandardCharsets.UTF_8))))
            TRAPIEdgeBinding(None, kgId, Some(prov))
          }
        }.toSet
        TRAPIResult(resultNodeBindings, resultEdgeBindings, Some(extraKGNodeBindings.toList), Some(extraKGEdgeBindings.toList))
      }
      trapiKGNodes = initialKGNodes ++ extraKGNodes
      trapiKGEdges = initialKGEdges ++ extraKGEdges
    } yield TRAPIMessage(Some(queryGraph), Some(TRAPIKnowledgeGraph(trapiKGNodes.distinct, trapiKGEdges)), Some(results))

  // instances are not thread-safe; should be retrieved for every use
  private def messageDigest: MessageDigest = MessageDigest.getInstance("SHA-256")

  private def getTRAPINodes(
    queryGraph: TRAPIQueryGraph,
    querySolutions: List[QuerySolution],
    prefixes: Map[String, String]): RIO[ZConfig[AppConfig] with HttpClient with Has[PrefixesMap], List[TRAPINode]] = {
    val allOntClasses = querySolutions.flatMap { qs =>
      qs.varNames.asScala.filter(_.endsWith("_type")).map(v => qs.getResource(v).getURI)
    }
    for {
      nodeDetails <- getTRAPINodeDetails(allOntClasses.map(v => s"<$v>"))
      trapiNodes <- ZIO.foreach(querySolutions) { querySolution =>
        for {
          nodeMap <- Task.effect(queryGraph.nodes.map(n => (n.id, querySolution.get(s"${n.id}_type").toString)).toMap)
          nodes <- ZIO.foreach(queryGraph.nodes) { n =>
            for {
              nodeIRI <- ZIO.fromOption(nodeMap.get(n.id)).orElseFail(new Exception(s"Missing node IRI: ${n.id}"))
              nodeDetailTypes <-
                ZIO
                  .fromOption(
                    nodeDetails
                      .filter(a => nodeIRI.equals(a.subj))
                      .map(a => (a.pred, a.obj))
                      .groupBy(_._1)
                      .map({ case (k, v) => (k, v.map(a => a._2)) })
                      .headOption
                  )
                  .orElseFail(new Exception("failed to get details"))
              trapiNode = TRAPINode(applyPrefix(nodeIRI, prefixes), Some(nodeDetailTypes._1), nodeDetailTypes._2.sorted)
            } yield trapiNode
          }
        } yield nodes
      }
    } yield trapiNodes.flatten
  }

  private def getTRAPIEdges(queryGraph: TRAPIQueryGraph,
                            querySolutions: List[QuerySolution],
                            prefixes: Map[String, String]): RIO[ZConfig[AppConfig], List[TRAPIEdge]] =
    for {
      trapiEdges <- ZIO.foreach(querySolutions) { querySolution =>
        for {
          nodeMap <- Task.effect(queryGraph.nodes.map(n => (n.id, querySolution.get(s"${n.id}_type").toString)).toMap)
          edges <- ZIO.foreach(queryGraph.edges) { e =>
            for {
              sourceId <- ZIO.fromOption(nodeMap.get(e.source_id)).orElseFail(new Exception("could not get source id"))
              targetId <- ZIO.fromOption(nodeMap.get(e.target_id)).orElseFail(new Exception("could not get target id"))
              edgeKey = TRAPIEdgeKey(e.`type`, e.source_id, e.target_id).asJson.deepDropNullValues.noSpaces
              encodedTRAPIEdge = String.format("%064x", new BigInteger(1, messageDigest.digest(edgeKey.getBytes(StandardCharsets.UTF_8))))
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

  private def getTRAPIEdgeBindingsMany(queryGraph: TRAPIQueryGraph, querySolutions: List[QuerySolution])
    : ZIO[ZConfig[AppConfig] with HttpClient with Has[PrefixesMap], Throwable, Map[QuerySolution, List[TRAPIEdgeBinding]]] = {
    val queryTriples = queryGraph.edges.map(e => TripleString(e.source_id, e.id, e.target_id))
    val solutionTriples = querySolutions.flatMap { qs =>
      queryTriples.map { qt =>
        TripleString(qs.getResource(qt.subj).getURI, qs.getResource(qt.pred).getURI, qs.getResource(qt.obj).getURI)
      }
    }
    for {
      provs <- getProvenanceMany(solutionTriples.toSet)
      querySolutionsToEdgeBindings <- ZIO.foreach(querySolutions) { querySolution =>
        for {
          nodeMap <- Task.effect(queryGraph.nodes.map(n => (n.id, querySolution.get(s"${n.id}_type").toString)).toMap)
          edgeBindings <- ZIO.foreach(queryGraph.edges) { e =>
            for {
              predicateRDFNode <- Task.effect(querySolution.get(e.id).toString)
              sourceRDFNode <- Task.effect(querySolution.get(e.source_id).toString)
              targetRDFNode <- Task.effect(querySolution.get(e.target_id).toString)
              edgeKey = TRAPIEdgeKey(e.`type`, e.source_id, e.target_id).asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
              encodedTRAPIEdge = String.format("%064x", new BigInteger(1, messageDigest.digest(edgeKey)))
              prov <-
                ZIO
                  .fromOption(provs.get(TripleString(sourceRDFNode, predicateRDFNode, targetRDFNode)))
                  .orElseFail(new Exception("Unexpected triple string"))
              trapiEdgeBinding = TRAPIEdgeBinding(Some(e.id), encodedTRAPIEdge, Some(prov))
            } yield trapiEdgeBinding
          }
        } yield querySolution -> edgeBindings
      }
    } yield querySolutionsToEdgeBindings.toMap
  }

  private def getTRAPIEdgeBindings(
    queryGraph: TRAPIQueryGraph,
    querySolution: QuerySolution): RIO[ZConfig[AppConfig] with HttpClient with Has[PrefixesMap], List[TRAPIEdgeBinding]] =
    for {
      nodeMap <- Task.effect(queryGraph.nodes.map(n => (n.id, querySolution.get(s"${n.id}_type").toString)).toMap)
      edgeBindings <- ZIO.foreach(queryGraph.edges) { e =>
        for {
          predicateRDFNode <- Task.effect(querySolution.get(e.id).toString)
          sourceRDFNode <- Task.effect(querySolution.get(e.source_id).toString)
          targetRDFNode <- Task.effect(querySolution.get(e.target_id).toString)
          edgeKey = TRAPIEdgeKey(e.`type`, e.source_id, e.target_id).asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
          encodedTRAPIEdge = String.format("%064x", new BigInteger(1, messageDigest.digest(edgeKey)))
          prov <- getProvenance(sourceRDFNode, predicateRDFNode, targetRDFNode)
          trapiEdgeBinding = TRAPIEdgeBinding(Some(e.id), encodedTRAPIEdge, Some(prov))
        } yield trapiEdgeBinding
      }
    } yield edgeBindings

  private def getExtraKGNodes(camNodes: Set[String],
                              slotStuffNodeDetails: List[TripleString],
                              prefixes: Map[String, String]): Set[TRAPINode] =
    for {
      node <- camNodes
      slotStuffNodeDetailTypes <-
        slotStuffNodeDetails
          .filter(a => node.equals(a.subj))
          .map(a => (a.pred, a.obj))
          .groupBy(_._1)
          .map({ case (k, v) => (k, v.map(a => a._2)) })
          .headOption
      abbreviatedNodeType = applyPrefix(node, prefixes)
      kgNode = TRAPINode(abbreviatedNodeType, Some(slotStuffNodeDetailTypes._1), slotStuffNodeDetailTypes._2.sorted)
    } yield kgNode

  private def getProvenanceMany(
    edges: Set[TripleString]): ZIO[ZConfig[AppConfig] with HttpClient with ZConfig[PrefixesMap], Throwable, Map[TripleString, String]] =
    for {
      prefixMap <- biolinkPrefixes
      prefixes = prefixMap.prefixesMap.map { case (prefix, expansion) => s"PREFIX $prefix: <$expansion>" }.mkString("\n")
      values = edges.map(e => s"(<${e.subj}> <${e.pred}> <${e.obj}>)").mkString(" ")
      queryText = s"""$prefixes
                     |SELECT ?s ?p ?o ?g ?other WHERE {
                     |VALUES (?s ?p ?o) { $values }
                     |GRAPH ?g { ?s ?p ?o } OPTIONAL { ?g prov:wasDerivedFrom ?other . }
                     |}""".stripMargin
      query <- Task.effect(QueryFactory.create(queryText))
      bindings <- SPARQLQueryExecutor.runSelectQuery(query)
      triplesToGraphs <- ZIO.foreach(bindings.asScala.toList) { solution =>
        Task.effect {
          val graph = if (solution.contains("other")) solution.getResource("other").getURI else solution.getResource("g").getURI
          val triple = TripleString(solution.getResource("s").getURI, solution.getResource("p").getURI, solution.getResource("o").getURI)
          triple -> graph
        }
      }
    } yield triplesToGraphs.toMap

  private def getProvenance(source: String,
                            predicate: String,
                            target: String): RIO[ZConfig[AppConfig] with HttpClient with Has[PrefixesMap], String] =
    for {
      prefixMap <- biolinkPrefixes
      prefixes = prefixMap.prefixesMap.map { case (prefix, expansion) => s"PREFIX $prefix: <$expansion>" }.mkString("\n")
      queryText = s"""$prefixes
          |SELECT ?g ?other WHERE {
          |GRAPH ?g { <$source> <$predicate> <$target> } OPTIONAL { ?g prov:wasDerivedFrom ?other . }
          |}""".stripMargin
      query <- Task.effect(QueryFactory.create(queryText))
      bindings <- SPARQLQueryExecutor.runSelectQuery(query)
      nextSolution = bindings.nextSolution()
      prov <- Task.effect(nextSolution.get("other").toString).orElse(Task.effect(nextSolution.get("g").toString))
    } yield prov

  private def getTRAPINodeDetails(
    nodeIdList: List[String]): RIO[ZConfig[AppConfig] with HttpClient with Has[PrefixesMap], List[TripleString]] =
    for {
      prefixMap <- biolinkPrefixes
      prefixes = prefixMap.prefixesMap.map { case (prefix, expansion) => s"PREFIX $prefix: <$expansion>" }.mkString("\n")
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
          TripleString(qs.get("kid").toString,
                       qs.get("label").toString,
                       convertCase(blClass.substring(blClass.lastIndexOf("/") + 1, blClass.length)))
        }
      )
    } yield results

  private def getCAMStuff(prov: String): RIO[ZConfig[AppConfig] with HttpClient with Has[PrefixesMap], List[TripleString]] =
    for {
      prefixMap <- biolinkPrefixes
      prefixes = prefixMap.prefixesMap.map { case (prefix, expansion) => s"PREFIX $prefix: <$expansion>" }.mkString("\n")
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
      triples <- SPARQLQueryExecutor.runSelectQueryAs[Triple](query)
    } yield triples.map(t => TripleString(t.subj.toString, t.pred.toString, t.obj.toString))

  private def getSlotStuff(
    predicateMap: List[String]): RIO[ZConfig[AppConfig] with HttpClient with Has[PrefixesMap], List[(String, String, String, String)]] =
    for {
      prefixMap <- biolinkPrefixes
      prefixes = prefixMap.prefixesMap.map { case (prefix, expansion) => s"PREFIX $prefix: <$expansion>" }.mkString("\n")
      values =
        predicateMap.zipWithIndex
          .map(a => String.format(" ( <%s> \"e%s\" ) ", a._1, StringUtils.leftPad(a._2.toString, 4, '0')))
          .mkString(" ")
      queryText = s"""$prefixes
          |SELECT DISTINCT ?qid ?kid ?blslot ?label WHERE {
          |VALUES (?kid ?qid) { $values }
          |?kid <http://www.w3.org/2000/01/rdf-schema#subPropertyOf>+ ?blslot .
          |?blslot a <https://w3id.org/biolink/biolinkml/meta/types/SlotDefinition> .
          |FILTER NOT EXISTS {
          |?kid <http://www.w3.org/2000/01/rdf-schema#subPropertyOf>+ ?other .
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

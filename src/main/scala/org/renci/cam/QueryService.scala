package org.renci.cam

import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe.syntax._
import org.apache.commons.lang3.StringUtils
import org.apache.jena.query.QuerySolution
import org.phenoscape.sparql.SPARQLInterpolation._
import org.renci.cam.Biolink._
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.domain._
import zio.config.ZConfig
import zio.{Has, RIO, Task, ZIO, config => _}

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.jdk.CollectionConverters._

object QueryService extends LazyLogging {

  val ProvWasDerivedFrom: IRI = IRI("http://www.w3.org/ns/prov#wasDerivedFrom")

  val RDFSSubClassOf: IRI = IRI("http://www.w3.org/2000/01/rdf-schema#subClassOf")

  val RDFSLabel: IRI = IRI("http://www.w3.org/2000/01/rdf-schema#label")

  val RDFType: IRI = IRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")

  val OWLNamedIndividual: IRI = IRI("http://www.w3.org/2002/07/owl#NamedIndividual")

  val SesameDirectType: IRI = IRI("http://www.openrdf.org/schema/sesame#directType")

  val BiolinkMLSlotDefinition: IRI = IRI("https://w3id.org/biolink/biolinkml/meta/types/SlotDefinition")

  val BiolinkMLIsA: IRI = IRI("https://w3id.org/biolink/biolinkml/meta/is_a")

  val BiolinkNamedThing: IRI = IRI("https://w3id.org/biolink/vocab/NamedThing")

  val RDFSSubPropertyOf: IRI = IRI("http://www.w3.org/2000/01/rdf-schema#subPropertyOf")

  val SlotMapping = IRI("http://cam.renci.org/biolink_slot")

  final case class TRAPIEdgeKey(`type`: Option[BiolinkPredicate], source_id: String, target_id: String)

  final case class Triple(subj: IRI, pred: IRI, obj: IRI)

  final case class TripleString(subj: String, pred: String, obj: String)

  final case class SlotStuff(qid: String, kid: IRI, biolinkSlot: IRI, label: Option[String])

  final case class TermWithLabelAndBiolinkType(term: IRI, biolinkType: IRI, label: Option[String])

  final case class Predicate(predicate: IRI)

  // instances are not thread-safe; should be retrieved for every use
  private def messageDigest: MessageDigest = MessageDigest.getInstance("SHA-256")

  def getNodeTypes(nodes: Map[String, TRAPIQueryNode]): Map[String, IRI] =
    nodes
      .map(entry =>
        (entry._2.category, entry._2.id) match {
          case (_, Some(c))    => List(entry._1 -> c)
          case (Some(t), None) => List(entry._1 -> t.iri)
          case (None, None)    => Nil
        })
      .flatten
      .toMap

  def applyPrefix(value: String, prefixes: Map[String, String]): String =
    prefixes
      .filter(entry => value.startsWith(entry._2))
      .map(entry => StringUtils.prependIfMissing(value.substring(entry._2.length, value.length), s"${entry._1}:"))
      .headOption
      .getOrElse(value)

  def run(limit: Option[Int],
          queryGraph: TRAPIQueryGraph): RIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], List[QuerySolution]] = {
    val nodeTypes = getNodeTypes(queryGraph.nodes)
    for {
      predicates <- ZIO.foreachPar(queryGraph.edges.filter(edge => edge._2.predicate.nonEmpty)) { (k, v) =>
        for {
          foundPredicates <- getTRAPIQEdgePredicates(v)
          predicates = foundPredicates.map(a => sparql" $a ").fold(sparql"")(_ + _)
          edgeIDVar = Var(k)
          edgeSourceVar = Var(v.subject)
          edgeTargetVar = Var(v.`object`)
          sparqlChunk =
            sparql"""
                 VALUES $edgeIDVar { $predicates }
                 $edgeSourceVar $edgeIDVar $edgeTargetVar .
              """
        } yield (v, sparqlChunk)
      }
      (edges, sparqlLines) = predicates.unzip
      nodesToDirectTypes =
        queryGraph.nodes
          .map { node =>
            val nodeVar = Var(node._1)
            val nodeTypeVar = Var(s"${node._1}_type")
            sparql""" $nodeVar $SesameDirectType $nodeTypeVar .  """
          }
          .fold(sparql"")(_ + _)
      projectionVariableNames = queryGraph.edges.flatMap(entry => List(entry._1)) ++ edges.flatMap(e =>
        List(e.subject, e.`object`)) ++ queryGraph.nodes.map(entry => s"${entry._1}_type")
      projection = projectionVariableNames.map(Var(_)).map(v => sparql" $v ").fold(sparql"")(_ + _)
      moreLines = for {
        id <- edges.flatMap(a => List(a.subject, a.`object`))
        subjVar = Var(id)
        v <- nodeTypes.get(id)
      } yield sparql"$subjVar $RDFType $v ."
      valuesClause = (sparqlLines ++ moreLines).fold(sparql"")(_ + _)
      limitValue = limit.getOrElse(1000)
      limitSparql = if (limitValue > 0) sparql" LIMIT $limitValue" else sparql""
      queryString = sparql"""
                          SELECT DISTINCT $projection
                          WHERE {
                            $nodesToDirectTypes
                            $valuesClause
                          }
                          $limitSparql
                          """
      response <- SPARQLQueryExecutor.runSelectQuery(queryString.toQuery)
    } yield response
  }

  def parseResultSet(queryGraph: TRAPIQueryGraph,
                     querySolutions: List[QuerySolution]): RIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], TRAPIMessage] =
    for {
      biolinkData <- biolinkData
      initialKGNodes <- getTRAPINodes(queryGraph, querySolutions)
      _ = logger.warn("initialKGNodes: {}", initialKGNodes)
      initialKGEdges <- getTRAPIEdges(queryGraph, querySolutions)
      _ = logger.warn("initialKGEdges: {}", initialKGEdges)
      querySolutionsToEdgeBindings <- getTRAPIEdgeBindingsMany(queryGraph, querySolutions)
      _ = logger.warn("querySolutionsToEdgeBindings: {}", querySolutionsToEdgeBindings)
      trapiBindings <- ZIO.foreach(querySolutions) { querySolution =>
        getTRAPINodeBindings(queryGraph, querySolution, biolinkData.prefixes) zip Task.effect(querySolutionsToEdgeBindings(querySolution))
      }
      _ = logger.warn("trapiBindings: {}", trapiBindings)
      allEdgeBindings = trapiBindings.flatMap(_._2).toMap
      _ = logger.warn("allEdgeBindings: {}", allEdgeBindings)
      allCamIds = allEdgeBindings.to(Set).flatMap(_._2.provenance)
      _ = logger.warn("allCamIds: {}", allCamIds)
      prov2CAMStuffTripleMap <- ZIO.foreachPar(allCamIds)(prov => getCAMStuff(IRI(prov)).map(prov -> _)).map(_.toMap)
      _ = logger.warn("prov2CAMStuffTripleMap: {}", prov2CAMStuffTripleMap)
      allCAMTriples = prov2CAMStuffTripleMap.values.to(Set).flatten
      _ = logger.warn("allCAMTriples: {}", allCAMTriples)
      allTripleNodes = allCAMTriples.flatMap(t => Set(t.subj, t.obj))
      _ = logger.warn("allTripleNodes: {}", allTripleNodes)
      slotStuffNodeDetails <- getTRAPINodeDetails(allTripleNodes.to(List), biolinkData.classes)
      _ = logger.warn("slotStuffNodeDetails: {}", slotStuffNodeDetails)
      extraKGNodes = getExtraKGNodes(allTripleNodes, slotStuffNodeDetails, biolinkData)
      _ = logger.warn("extraKGNodes: {}", extraKGNodes)
      allPredicates = allCAMTriples.map(_.pred)
      _ = logger.warn("allPredicates: {}", allPredicates)
      slotStuffList <- getSlotStuff(allPredicates.to(List))
      _ = logger.warn("slotStuffList: {}", slotStuffList)
      extraKGEdges = allCAMTriples.flatMap { triple =>
        for {
          slotStuff <- slotStuffList.find(_.kid == triple.pred)
          predBLTermOpt = biolinkData.predicates.find(a => a.iri == slotStuff.biolinkSlot)
          edgeKey = TRAPIEdgeKey(predBLTermOpt, triple.subj.value, triple.obj.value).asJson.deepDropNullValues.noSpaces
          encodedEdgeKey = String.format("%064x", new BigInteger(1, messageDigest.digest(edgeKey.getBytes(StandardCharsets.UTF_8))))
        } yield encodedEdgeKey -> TRAPIEdge(triple.subj, triple.obj, None, predBLTermOpt, None)
      }.toMap
      _ = logger.warn("extraKGEdges: {}", extraKGEdges)
      results = trapiBindings.map { case (resultNodeBindings, resultEdgeBindings) =>
//        val provsAndCamTriples =
//          resultEdgeBindings.flatMap(_._2.provenance).map(prov => prov2CAMStuffTripleMap.get(prov).to(Set).flatten)
//        logger.warn("provsAndCamTriples: {}", provsAndCamTriples)
//        val nodes = provsAndCamTriples.flatten.to(Set).flatMap(t => Set(t.subj, t.obj))
//        val extraKGNodeBindings = nodes.map(n => TRAPINodeBinding(IRI(applyPrefix(n.value, biolinkData.prefixes))))
//        val extraKGEdgeBindings = provsAndCamTriples
//          .to(Set)
//          .flatMap { case (prov, triples) =>
//            triples.map { triple =>
//              val predBLTermOpt = biolinkData.predicates.find(a => a.iri == triple.pred)
////              val edgeKey = TRAPIEdgeKey(predBLTermOpt, triple.subj.value, triple.obj.value).asJson.deepDropNullValues.noSpaces
////              val kgId = String.format("%064x", new BigInteger(1, messageDigest.digest(edgeKey.getBytes(StandardCharsets.UTF_8))))
//              TRAPIEdgeBinding(triple.obj.value, Some(prov))
//            }
//          }
        TRAPIResult(resultNodeBindings, resultEdgeBindings /*, Some(extraKGNodeBindings.to(List)), Some(extraKGEdgeBindings.to(List))*/ )
      }
      trapiKGNodes = initialKGNodes ++ extraKGNodes
      trapiKGEdges = initialKGEdges ++ extraKGEdges
    } yield TRAPIMessage(Some(queryGraph), Some(TRAPIKnowledgeGraph(trapiKGNodes, trapiKGEdges)), Some(results))

  private def getTRAPIEdges(queryGraph: TRAPIQueryGraph,
                            querySolutions: List[QuerySolution]): RIO[ZConfig[AppConfig] with HttpClient, Map[String, TRAPIEdge]] =
    for {
      trapiEdges <- ZIO.foreach(querySolutions) { querySolution =>
        for {
          nodeMap <- Task.effect(queryGraph.nodes.map(entry => (entry._1, IRI(querySolution.getResource(s"${entry._1}_type").getURI))))
          edges <- ZIO.foreach(queryGraph.edges) { (k, v) =>
            for {
              source <- ZIO.fromOption(nodeMap.get(v.subject)).orElseFail(new Exception("could not get source id"))
              target <- ZIO.fromOption(nodeMap.get(v.`object`)).orElseFail(new Exception("could not get target id"))
              edgeKey = TRAPIEdgeKey(v.predicate, v.subject, v.`object`).asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
              encodedTRAPIEdge = String.format("%064x", new BigInteger(1, messageDigest.digest(edgeKey)))
            } yield encodedTRAPIEdge -> TRAPIEdge(source, target, None, v.predicate, None)
          }
        } yield edges.toList
      }
    } yield trapiEdges.flatten.toMap

  private def getTRAPINodes(
    queryGraph: TRAPIQueryGraph,
    querySolutions: List[QuerySolution]): RIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], Map[String, TRAPINode]] = {
    val allOntClassIRIsZ = ZIO
      .foreach(querySolutions) { qs =>
        ZIO.foreach(qs.varNames.asScala.filter(_.endsWith("_type")).to(Iterable)) { typeVar =>
          ZIO.effect(IRI(qs.getResource(typeVar).getURI)).mapError { e =>
            new Exception(s"Value of _type variable $typeVar is not a URI", e)
          }
        }
      }
      .map(_.flatten)
    for {
      allOntClassIRIs <- allOntClassIRIsZ
      biolinkData <- biolinkData
      nodeDetails <- getTRAPINodeDetails(allOntClassIRIs, biolinkData.classes)
      termToLabelAndTypes = nodeDetails.groupBy(_.term).map { case (term, termsAndTypes) =>
        val (labels, biolinkTypes) = termsAndTypes.map(t => t.label -> t.biolinkType).unzip
        term -> (labels.flatten.headOption, biolinkTypes)
      }
      trapiNodes <- ZIO.foreach(querySolutions) { querySolution =>
        for {
          nodeMap <- Task.effect(queryGraph.nodes.map(entry => (entry._1, querySolution.get(s"${entry._1}_type").toString)))
          nodes <- ZIO.foreach(queryGraph.nodes) { (k, v) =>
            for {
              nodeIRI <- ZIO.fromOption(nodeMap.get(k)).orElseFail(new Exception(s"Missing node IRI: $k"))
              labelAndTypes = termToLabelAndTypes.getOrElse(IRI(nodeIRI), (None, List(BiolinkNamedThing)))
              (labelOpt, biolinkTypes) = labelAndTypes
              biolinkTypesSet = biolinkTypes.to(Set)
              nodeBiolinkTypes = biolinkData.classes.filter(c => biolinkTypesSet(c.iri))
              prefixApplied = applyPrefix(nodeIRI, biolinkData.prefixes)
            } yield prefixApplied -> TRAPINode(labelOpt, Some(nodeBiolinkTypes), None)
          }
        } yield nodes.toList
      }
    } yield trapiNodes.flatten.toMap
  }

  private def getTRAPINodeDetails(
    nodeIdList: List[IRI],
    biolinkClasses: List[BiolinkClass]): RIO[ZConfig[AppConfig] with HttpClient, List[TermWithLabelAndBiolinkType]] = {
    val nodeIds = nodeIdList.map(n => sparql" $n ").fold(sparql"")(_ + _)
    for {
      namedThingBiolinkClass <- ZIO
        .fromOption(biolinkClasses.find(a => a.shorthand == "named_thing"))
        .orElseFail(new Exception("Could not find BiolinkClass:NamedThing"))
      // requiring biolinkType makes some terms not be found when these results are used elsewhere - must be handled there
      queryText = sparql"""
                       SELECT ?term ?biolinkType (MIN(?term_label) AS ?label)
                       WHERE {
                         VALUES ?term { $nodeIds }
                         ?term $RDFSSubClassOf ?biolinkType .
                         ?biolinkType $BiolinkMLIsA* ${namedThingBiolinkClass.iri} .
                         OPTIONAL { ?term $RDFSLabel ?term_label }
                       }
                       GROUP BY ?term ?biolinkType
                       """
      termsAndBiolinkTypes <- SPARQLQueryExecutor.runSelectQueryAs[TermWithLabelAndBiolinkType](queryText.toQuery)
    } yield termsAndBiolinkTypes
  }

  private def getTRAPINodeBindings(queryGraph: TRAPIQueryGraph,
                                   querySolution: QuerySolution,
                                   prefixes: Map[String, String]): RIO[ZConfig[AppConfig], Map[String, TRAPINodeBinding]] =
    for {
      nodeMap <- Task.effect(queryGraph.nodes.map(n => (n._1, querySolution.get(s"${n._1}_type").toString)))
      nodeBindings <- ZIO.foreach(queryGraph.nodes) { (k, v) =>
        for {
          nodeIRI <- ZIO.fromOption(nodeMap.get(k)).orElseFail(new Exception(s"Missing node IRI: $k"))
        } yield k -> TRAPINodeBinding(IRI(applyPrefix(nodeIRI, prefixes)))
      }
    } yield nodeBindings

  private def getTRAPIEdgeBindingsMany(queryGraph: TRAPIQueryGraph, querySolutions: List[QuerySolution])
    : ZIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], Throwable, Map[QuerySolution, Map[String, TRAPIEdgeBinding]]] = {
    val solutionTriples = for {
      queryEdge <- queryGraph.edges
      solution <- querySolutions
    } yield Triple(
      IRI(solution.getResource(queryEdge._2.subject).getURI),
      IRI(solution.getResource(queryEdge._1).getURI),
      IRI(solution.getResource(queryEdge._2.`object`).getURI)
    )
    for {
      provs <- getProvenance(solutionTriples.to(Set))
      querySolutionsToEdgeBindings <- ZIO.foreach(querySolutions) { querySolution =>
        for {
          edgeBindings <- ZIO.foreach(queryGraph.edges) { (k, v) =>
            for {
              predicateRDFNode <- Task.effect(querySolution.get(k).toString)
              sourceRDFNode <- Task.effect(querySolution.get(v.subject).toString)
              targetRDFNode <- Task.effect(querySolution.get(v.`object`).toString)
              edgeKey = TRAPIEdgeKey(v.predicate, v.subject, v.`object`).asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8)
              encodedTRAPIEdge = String.format("%064x", new BigInteger(1, messageDigest.digest(edgeKey)))
              prov <-
                ZIO
                  .fromOption(provs.get(TripleString(sourceRDFNode, predicateRDFNode, targetRDFNode)))
                  .orElseFail(new Exception("Unexpected triple string"))
            } yield encodedTRAPIEdge -> TRAPIEdgeBinding(predicateRDFNode, Some(prov))
          }
        } yield querySolution -> edgeBindings
      }
    } yield querySolutionsToEdgeBindings.toMap
  }

  private def getProvenance(edges: Set[Triple]): ZIO[ZConfig[AppConfig] with HttpClient, Throwable, Map[TripleString, String]] = {
    val values = edges.map(e => sparql"( ${e.subj} ${e.pred} ${e.obj} )").fold(sparql"")(_ + _)
    val queryText =
      sparql"""
                SELECT ?s ?p ?o ?g ?other
                WHERE {
                  VALUES (?s ?p ?o) { $values }
                  GRAPH ?g { ?s ?p ?o }
                  OPTIONAL { ?g $ProvWasDerivedFrom ?other . }
                }
              """
    for {
      bindings <- SPARQLQueryExecutor.runSelectQuery(queryText.toQuery)
      triplesToGraphs <- ZIO.foreach(bindings) { solution =>
        Task.effect {
          val graph = if (solution.contains("other")) solution.getResource("other").getURI else solution.getResource("g").getURI
          val triple = TripleString(solution.getResource("s").getURI, solution.getResource("p").getURI, solution.getResource("o").getURI)
          triple -> graph
        }
      }
    } yield triplesToGraphs.toMap
  }

  private def getCAMStuff(prov: IRI): RIO[ZConfig[AppConfig] with HttpClient, List[Triple]] =
    for {
      queryText <- Task.effect(
        sparql"""SELECT DISTINCT (?s_type AS ?subj) (?p AS ?pred) (?o_type AS ?obj) WHERE { GRAPH $prov {
                     ?s ?p ?o .
                     ?s $RDFType $OWLNamedIndividual .
                     ?o $RDFType $OWLNamedIndividual .
                   }
                   ?o $SesameDirectType ?o_type .
                   ?s $SesameDirectType ?s_type .
                   FILTER(isIRI(?o_type))
                   FILTER(isIRI(?s_type))
                 }"""
      )
      triples <- SPARQLQueryExecutor.runSelectQueryAs[Triple](queryText.toQuery)
    } yield triples

  private def getExtraKGNodes(camNodes: Set[IRI],
                              slotStuffNodeDetails: List[TermWithLabelAndBiolinkType],
                              biolinkData: BiolinkData): Map[String, TRAPINode] = {
    val termToLabelAndTypes = slotStuffNodeDetails.groupBy(_.term).map { case (term, termsAndTypes) =>
      val (labels, biolinkTypes) = termsAndTypes.map(t => t.label -> t.biolinkType).unzip
      term -> (labels.flatten.headOption, biolinkTypes)
    }
    val nodeMap = camNodes.map { node =>
      val (labelOpt, biolinkTypes) = termToLabelAndTypes.getOrElse(node, (None, List(BiolinkNamedThing)))
      val biolinkTypesSet = biolinkTypes.to(Set)
      val classes = biolinkData.classes.filter(c => biolinkTypesSet(c.iri))
      applyPrefix(node.value, biolinkData.prefixes) -> TRAPINode(labelOpt, Some(classes), None)
    }.toMap
    nodeMap
  }

  private def getSlotStuff(predicates: List[IRI]): RIO[ZConfig[AppConfig] with HttpClient, List[SlotStuff]] = {
    val values = predicates.zipWithIndex
      .map { case (p, i) =>
        val id = StringUtils.leftPad(i.toString, 4, '0')
        val qid = s"e$id"
        sparql" ( $p $qid ) "
      }
      .fold(sparql"")(_ + _)
    val queryText = sparql"""
                     SELECT DISTINCT ?qid ?kid ?biolinkSlot ?label
                     WHERE {
                     VALUES (?kid ?qid) { $values }
                     ?kid $SlotMapping ?biolinkSlot .
                     ?biolinkSlot a $BiolinkMLSlotDefinition .
                     OPTIONAL { ?kid $RDFSLabel ?label . }
                     FILTER NOT EXISTS {
                       ?kid $SlotMapping ?other .
                       ?other $BiolinkMLIsA+/<https://w3id.org/biolink/biolinkml/meta/mixins>* ?biolinkSlot .
                       }
                     }
                     """
    SPARQLQueryExecutor.runSelectQueryAs[SlotStuff](queryText.toQuery)
  }

  private def getTRAPIQEdgePredicates(edge: TRAPIQueryEdge): RIO[ZConfig[AppConfig] with HttpClient, List[IRI]] =
    for {
      edgeType <- ZIO.fromOption(edge.predicate).orElseFail(new Exception("failed to get edge type"))
      queryText = sparql"""
                   SELECT DISTINCT ?predicate
                   WHERE {
                     ?predicate $SlotMapping ${edgeType.iri} .
                   }
                   """
      predicates <- SPARQLQueryExecutor.runSelectQueryAs[Predicate](queryText.toQuery)
    } yield predicates.map(_.predicate)

}

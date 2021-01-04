package org.renci.cam

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe.syntax._
import org.apache.commons.lang3.StringUtils
import org.apache.jena.ext.com.google.common.base.CaseFormat
import org.apache.jena.query.QuerySolution
import org.phenoscape.sparql.SPARQLInterpolation._
import org.renci.cam.Biolink._
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.domain._
import zio.config.ZConfig
import zio.{Has, RIO, Task, ZIO, config => _}

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

  // instances are not thread-safe; should be retrieved for every use
  private def messageDigest: MessageDigest = MessageDigest.getInstance("SHA-256")

  def getNodeTypes(nodes: List[TRAPIQueryNode]): Map[String, IRI] =
    nodes.flatMap { node =>
      (node.`type`, node.curie) match {
        case (_, Some(c))    => List(node.id -> c)
        case (Some(t), None) => List(node.id -> t.iri)
        case (None, None)    => Nil
      }
    }.toMap

  def applyPrefix(value: String, prefixes: Map[String, String]): String =
    prefixes
      .filter(entry => value.startsWith(entry._2))
      .map(entry => StringUtils.prependIfMissing(value.substring(entry._2.length, value.length), s"${entry._1}:"))
      .headOption
      .getOrElse(value)

  def run(limit: Int, queryGraph: TRAPIQueryGraph): RIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], List[QuerySolution]] = {
    val nodeTypes = getNodeTypes(queryGraph.nodes)
    for {
      predicates <- ZIO.foreachPar(queryGraph.edges.filter(_.`type`.nonEmpty)) { edge =>
        for {
          foundPredicates <- getTRAPIQEdgePredicates(edge)
          predicates = foundPredicates.map(a => sparql" $a ").fold(sparql"")(_ + _)
          edgeIDVar = Var(edge.id)
          edgeSourceVar = Var(edge.source_id)
          edgeTargetVar = Var(edge.target_id)
          sparqlChunk =
            sparql"""
                 VALUES $edgeIDVar { $predicates }
                 $edgeSourceVar $edgeIDVar $edgeTargetVar .
              """
        } yield (edge, sparqlChunk)
      }
      (edges, sparqlLines) = predicates.unzip
      nodesToDirectTypes =
        queryGraph.nodes
          .map { node =>
            val nodeVar = Var(node.id)
            val nodeTypeVar = Var(s"${node.id}_type")
            sparql""" $nodeVar $SesameDirectType $nodeTypeVar .  """
          }
          .fold(sparql"")(_ + _)
      projectionVariableNames = edges.flatMap(a => List(a.id, a.source_id, a.target_id)) ++ queryGraph.nodes.map(n => s"${n.id}_type")
      projection = projectionVariableNames.map(Var(_)).map(v => sparql" $v ").fold(sparql"")(_ + _)
      moreLines = for {
        id <- edges.flatMap(a => List(a.source_id, a.target_id))
        subjVar = Var(id)
        v <- nodeTypes.get(id)
      } yield sparql"$subjVar $RDFType $v ."
      valuesClause = (sparqlLines ++ moreLines).fold(sparql"")(_ + _)
      limitSparql = if (limit > 0) sparql" LIMIT $limit" else sparql""
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
      initialKGEdges <- getTRAPIEdges(queryGraph, querySolutions)
      querySolutionsToEdgeBindings <- getTRAPIEdgeBindingsMany(queryGraph, querySolutions)
      trapiBindings <- ZIO.foreach(querySolutions) { querySolution =>
        getTRAPINodeBindings(queryGraph, querySolution, biolinkData.prefixes) zip Task.effect(querySolutionsToEdgeBindings(querySolution))
      }
      allEdgeBindings = trapiBindings.flatMap(_._2)
      allCamIds = allEdgeBindings.to(Set).flatMap(_.provenance)
      prov2CAMStuffTripleMap <- ZIO.foreachPar(allCamIds)(prov => getCAMStuff(IRI(prov)).map(prov -> _)).map(_.toMap)
      allCAMTriples = prov2CAMStuffTripleMap.values.to(Set).flatten
      allTripleNodes = allCAMTriples.flatMap(t => Set(t.subj, t.obj))
      slotStuffNodeDetails <- getTRAPINodeDetails(allTripleNodes.to(List), biolinkData.classes)
      extraKGNodes = getExtraKGNodes(allTripleNodes, slotStuffNodeDetails, biolinkData)
      allPredicates = allCAMTriples.map(_.pred)
      slotStuffList <- getSlotStuff(allPredicates.to(List))
      extraKGEdges = allCAMTriples.flatMap { triple =>
        for {
          slotStuff <- slotStuffList.find(_.kid == triple.pred)
          predBLTermOpt = biolinkData.predicates.find(a => a.iri == slotStuff.biolinkSlot)
        } yield {
          val edgeKey =
            TRAPIEdgeKey(predBLTermOpt, triple.subj.value, triple.obj.value).asJson.deepDropNullValues.noSpaces
          val knowledgeGraphId = String.format("%064x", new BigInteger(1, messageDigest.digest(edgeKey.getBytes(StandardCharsets.UTF_8))))
          TRAPIEdge(knowledgeGraphId, triple.subj, triple.obj, predBLTermOpt)
        }
      }
      results = trapiBindings.map { case (resultNodeBindings, resultEdgeBindings) =>
        val provsAndCamTriples =
          resultEdgeBindings.flatMap(_.provenance).map(prov => prov -> prov2CAMStuffTripleMap.get(prov).to(Set).flatten).toMap
        val nodes = provsAndCamTriples.values.flatten.to(Set).flatMap(t => Set(t.subj, t.obj))
        val extraKGNodeBindings = nodes.map(n => TRAPINodeBinding(None, applyPrefix(n.value, biolinkData.prefixes)))
        val extraKGEdgeBindings = provsAndCamTriples
          .to(Set)
          .flatMap { case (prov, triples) =>
            triples.map { triple =>
              val predBLTermOpt = biolinkData.predicates.find(a => a.iri == triple.pred)
              val edgeKey = TRAPIEdgeKey(predBLTermOpt, triple.subj.value, triple.obj.value).asJson.deepDropNullValues.noSpaces
              val kgId = String.format("%064x", new BigInteger(1, messageDigest.digest(edgeKey.getBytes(StandardCharsets.UTF_8))))
              TRAPIEdgeBinding(None, kgId, Some(prov))
            }
          }
        TRAPIResult(resultNodeBindings, resultEdgeBindings, Some(extraKGNodeBindings.to(List)), Some(extraKGEdgeBindings.to(List)))
      }
      trapiKGNodes = initialKGNodes ++ extraKGNodes
      trapiKGEdges = initialKGEdges ++ extraKGEdges
    } yield TRAPIMessage(Some(queryGraph), Some(TRAPIKnowledgeGraph(trapiKGNodes.distinct, trapiKGEdges)), Some(results))

  private def getTRAPINodes(
    queryGraph: TRAPIQueryGraph,
    querySolutions: List[QuerySolution]): RIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], List[TRAPINode]] = {
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
          nodeMap <- Task.effect(queryGraph.nodes.map(n => (n.id, querySolution.get(s"${n.id}_type").toString)).toMap)
          nodes <- ZIO.foreach(queryGraph.nodes) { n =>
            for {
              nodeIRI <- ZIO.fromOption(nodeMap.get(n.id)).orElseFail(new Exception(s"Missing node IRI: ${n.id}"))
              labelAndTypes = termToLabelAndTypes.getOrElse(IRI(nodeIRI), (None, List(BiolinkNamedThing)))
              (labelOpt, biolinkTypes) = labelAndTypes
              biolinkTypesSet = biolinkTypes.to(Set)
              nodeBiolinkTypes = biolinkData.classes.filter(c => biolinkTypesSet(c.iri))
            } yield TRAPINode(applyPrefix(nodeIRI, biolinkData.prefixes), labelOpt, nodeBiolinkTypes)
          }
        } yield nodes
      }
    } yield trapiNodes.flatten
  }

  private def getTRAPIEdges(queryGraph: TRAPIQueryGraph,
                            querySolutions: List[QuerySolution]): RIO[ZConfig[AppConfig] with HttpClient, List[TRAPIEdge]] =
    for {
      trapiEdges <- ZIO.foreach(querySolutions) { querySolution =>
        for {
          nodeMap <- Task.effect(queryGraph.nodes.map(n => (n.id, IRI(querySolution.getResource(s"${n.id}_type").getURI))).toMap)
          edges <- ZIO.foreach(queryGraph.edges) { e =>
            for {
              sourceId <- ZIO.fromOption(nodeMap.get(e.source_id)).orElseFail(new Exception("could not get source id"))
              targetId <- ZIO.fromOption(nodeMap.get(e.target_id)).orElseFail(new Exception("could not get target id"))
              edgeKey = TRAPIEdgeKey(e.`type`, e.source_id, e.target_id).asJson.deepDropNullValues.noSpaces
              encodedTRAPIEdge = String.format("%064x", new BigInteger(1, messageDigest.digest(edgeKey.getBytes(StandardCharsets.UTF_8))))
              trapiEdge = TRAPIEdge(encodedTRAPIEdge, sourceId, targetId, e.`type`)
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
    : ZIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], Throwable, Map[QuerySolution, List[TRAPIEdgeBinding]]] = {
    val solutionTriples = for {
      queryEdge <- queryGraph.edges
      solution <- querySolutions
    } yield Triple(
      IRI(solution.getResource(queryEdge.source_id).getURI),
      IRI(solution.getResource(queryEdge.id).getURI),
      IRI(solution.getResource(queryEdge.target_id).getURI)
    )
    for {
      provs <- getProvenance(solutionTriples.to(Set))
      querySolutionsToEdgeBindings <- ZIO.foreach(querySolutions) { querySolution =>
        for {
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

  private def getExtraKGNodes(camNodes: Set[IRI],
                              slotStuffNodeDetails: List[TermWithLabelAndBiolinkType],
                              biolinkData: BiolinkData): Set[TRAPINode] = {
    val termToLabelAndTypes = slotStuffNodeDetails.groupBy(_.term).map { case (term, termsAndTypes) =>
      val (labels, biolinkTypes) = termsAndTypes.map(t => t.label -> t.biolinkType).unzip
      term -> (labels.flatten.headOption, biolinkTypes)
    }
    camNodes.map { node =>
      val (labelOpt, biolinkTypes) = termToLabelAndTypes.getOrElse(node, (None, List(BiolinkNamedThing)))
      val biolinkTypesSet = biolinkTypes.to(Set)
      val abbreviatedNodeType = applyPrefix(node.value, biolinkData.prefixes)
      val classes = biolinkData.classes.filter(c => biolinkTypesSet(c.iri))
      TRAPINode(abbreviatedNodeType, labelOpt, classes)
    }
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

  final private case class TermWithLabelAndBiolinkType(term: IRI, biolinkType: IRI, label: Option[String])

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
                     ?kid $RDFSSubPropertyOf+ ?biolinkSlot .
                     ?biolinkSlot a $BiolinkMLSlotDefinition .
                     OPTIONAL { ?kid $RDFSLabel ?label . }
                     FILTER NOT EXISTS {
                       ?kid $RDFSSubPropertyOf+ ?other .
                       ?other <https://w3id.org/biolink/biolinkml/meta/is_a>+/<https://w3id.org/biolink/biolinkml/meta/mixins>* ?biolinkSlot .
                       } 
                     }
                     """
    SPARQLQueryExecutor.runSelectQueryAs[SlotStuff](queryText.toQuery)
  }

  final private case class Predicate(predicate: IRI)

  private def getTRAPIQEdgePredicates(edge: TRAPIQueryEdge): RIO[ZConfig[AppConfig] with HttpClient, List[IRI]] =
    for {
      edgeType <- ZIO.fromOption(edge.`type`).orElseFail(new Exception("failed to get edge type"))
      queryText = sparql"""
                   SELECT DISTINCT ?predicate
                   WHERE {
                     ?predicate $SlotMapping ${edgeType.iri} .
                   }
                   """
      predicates <- SPARQLQueryExecutor.runSelectQueryAs[Predicate](queryText.toQuery)
    } yield predicates.map(_.predicate)

}

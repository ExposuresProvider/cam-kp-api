package org.renci.cam

import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe.syntax._
import org.apache.commons.lang3.StringUtils
import org.apache.jena.query.QuerySolution
import org.phenoscape.sparql.SPARQLInterpolation._
import org.renci.cam.Biolink.{biolinkData, BiolinkData}
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

  val RDFSSubPropertyOf: IRI = IRI("http://www.w3.org/2000/01/rdf-schema#subPropertyOf")

  val SlotMapping: IRI = IRI("http://cam.renci.org/biolink_slot")

  final case class TRAPIEdgeKey(source_id: String, `type`: Option[BiolinkPredicate], target_id: String)

  final case class Triple(subj: IRI, pred: IRI, obj: IRI)

  final case class TripleString(subj: String, pred: String, obj: String)

  final case class SlotStuff(qid: String, kid: IRI, biolinkSlot: IRI, label: Option[String])

  final case class TermWithLabelAndBiolinkType(term: IRI, biolinkType: IRI, label: Option[String])

  final case class Predicate(biolinkPredicate: BiolinkPredicate, predicate: IRI)

  // instances are not thread-safe; should be retrieved for every use
  private def messageDigest: MessageDigest = MessageDigest.getInstance("SHA-256")

  def run(limit: Option[Int],
          submittedQueryGraph: TRAPIQueryGraph): RIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], TRAPIMessage] =
    for {
      biolinkData <- biolinkData
      queryGraph = enforceQueryEdgeTypes(submittedQueryGraph, biolinkData.predicates)
      namedThingBiolinkClass <- ZIO
        .fromOption(biolinkData.classes.find(a => a.shorthand == "NamedThing"))
        .orElseFail(new Exception("Could not find BiolinkClass:NamedThing"))
      predicatesMap <- ZIO.foreach(queryGraph.edges) { (k, v) =>
        for {
          preds <- getTRAPIQEdgePredicates(v)
        } yield k -> preds
      }
      predicates <- ZIO.foreachPar(queryGraph.edges) { (k, v) =>
        for {
          foundPredicates <- Task.effect(predicatesMap.values.flatMap(a => a.keys))
          predicatesQueryText <- Task.effect(foundPredicates.map(a => sparql" $a ").fold(sparql"")(_ + _))
          edgeIDVar = Var(k)
          edgeSourceVar = Var(v.subject)
          edgeTargetVar = Var(v.`object`)
          predicatesValuesClause = sparql""" VALUES $edgeIDVar { $predicatesQueryText } """

          subjectNode = queryGraph.nodes(v.subject)
          subjectNodeValuesClauses = (subjectNode.ids, subjectNode.categories) match {
            case (Some(c), _) =>
              val idList = c.map(a => sparql" $a ").reduce((a, b) => sparql"$a, $b")
              sparql" VALUES $edgeSourceVar { $idList } "
            case (None, Some(t)) =>
              val idList = t.map(a => sparql" ${a.iri} ").reduce((a, b) => sparql"$a, $b")
              sparql"$edgeSourceVar $RDFType $idList . "
            case (None, None) => sparql""
          }

          objectNode = queryGraph.nodes(v.`object`)
          objectNodeValuesClauses = (objectNode.ids, objectNode.categories) match {
            case (Some(c), _) =>
              val idList = c.map(a => sparql" $a ").reduce((a, b) => sparql"$a, $b")
              sparql" VALUES $edgeTargetVar { $idList } "
            case (None, Some(t)) =>
              val idList = t.map(a => sparql" ${a.iri} ").reduce((a, b) => sparql"$a, $b")
              sparql"$edgeTargetVar $RDFType $idList . "
            case (None, None) => sparql""
          }

          nodesValuesClauses = List(subjectNodeValuesClauses, objectNodeValuesClauses).fold(sparql"")(_ + _)
          ret = sparql"""
              $predicatesValuesClause
              $nodesValuesClauses
              $edgeSourceVar $edgeIDVar $edgeTargetVar .
            """

        } yield (v, ret)
      }
      (edges, sparqlLines) = predicates.unzip
      nodesToDirectTypes = getNodesToDirectTypes(queryGraph.nodes)
      projections = getProjections(queryGraph)
      valuesClause = sparqlLines.fold(sparql"")(_ + _)
      limitSparql = getLimit(limit)
      queryString =
        sparql"""SELECT DISTINCT $projections
          WHERE {
            $nodesToDirectTypes
            $valuesClause
          }
          $limitSparql
          """
      querySolutions <- SPARQLQueryExecutor.runSelectQuery(queryString.toQuery)
      solutionTriples = for {
        queryEdge <- queryGraph.edges
        solution <- querySolutions
      } yield Triple(
        IRI(solution.getResource(queryEdge._2.subject).getURI),
        IRI(solution.getResource(queryEdge._1).getURI),
        IRI(solution.getResource(queryEdge._2.`object`).getURI)
      )
      provs <- getProvenance(solutionTriples.to(Set))
      initialKGNodes <- getTRAPINodes(queryGraph, querySolutions, biolinkData.classes, namedThingBiolinkClass)
      initialKGEdges <- getTRAPIEdges(queryGraph, querySolutions, predicatesMap, provs)
      querySolutionsToEdgeBindings <- getTRAPIEdgeBindingsMany(queryGraph, querySolutions, predicatesMap)
      trapiBindings <- ZIO.foreach(querySolutions) { querySolution =>
        getTRAPINodeBindings(queryGraph, querySolution) zip Task.effect(querySolutionsToEdgeBindings(querySolution))
      }
      prov2CAMStuffTripleMap <- ZIO.foreachPar(provs.values)(prov => getCAMStuff(IRI(prov)).map(prov -> _)).map(_.toMap)
      allCAMTriples = prov2CAMStuffTripleMap.values.to(Set).flatten
      allTripleNodes = allCAMTriples.flatMap(t => Set(t.subj, t.obj))
      slotStuffNodeDetails <- getTRAPINodeDetails(allTripleNodes.to(List), namedThingBiolinkClass)
      extraKGNodes = getExtraKGNodes(allTripleNodes, slotStuffNodeDetails, biolinkData, namedThingBiolinkClass)
      allPredicates = allCAMTriples.map(_.pred)
      slotStuffList <- getSlotStuff(allPredicates.to(List))
      extraKGEdges = allCAMTriples.flatMap { triple =>
        for {
          slotStuff <- slotStuffList.find(_.kid == triple.pred)
          predBLTermOpt = biolinkData.predicates.find(a => a.iri == slotStuff.biolinkSlot)
          key = getTRAPIEdgeKey(triple.subj.value, predBLTermOpt, triple.obj.value)
          edge = TRAPIEdge(predBLTermOpt, None, triple.subj, triple.obj, None)
        } yield key -> edge
      }.toMap

      results = trapiBindings.map { case (resultNodeBindings, resultEdgeBindings) => TRAPIResult(resultNodeBindings, resultEdgeBindings) }
      finalResults = results
      trapiKGNodes = initialKGNodes ++ extraKGNodes
      trapiKGEdges = initialKGEdges ++ extraKGEdges

    } yield TRAPIMessage(Some(queryGraph), Some(TRAPIKnowledgeGraph(trapiKGNodes, trapiKGEdges)), Some(finalResults.distinct))

  def getNodesToDirectTypes(nodes: Map[String, TRAPIQueryNode]): QueryText =
    nodes
      .map { node =>
        val nodeVar = Var(node._1)
        val nodeTypeVar = Var(s"${node._1}_type")
        sparql""" $nodeVar $SesameDirectType $nodeTypeVar .  """
      }
      .fold(sparql"")(_ + _)

  def getLimit(limit: Option[Int]): QueryText = {
    val limitValue = limit.getOrElse(1000)
    if (limitValue > 0) sparql" LIMIT $limitValue" else sparql""
  }

  def getProjections(queryGraph: TRAPIQueryGraph): QueryText = {
    val projectionVariableNames =
      queryGraph.edges.flatMap(entry => List(entry._1)) ++ queryGraph.edges.flatMap(e =>
        List(e._2.subject, e._2.`object`)) ++ queryGraph.nodes.map(entry => s"${entry._1}_type")
    projectionVariableNames.map(Var(_)).map(v => sparql" $v ").fold(sparql"")(_ + _)
  }

  def enforceQueryEdgeTypes(queryGraph: TRAPIQueryGraph, biolinkPredicates: List[BiolinkPredicate]): TRAPIQueryGraph = {
    val improvedEdgeMap = queryGraph.edges.map { case (edgeID, edge) =>
      val newPredicate = edge.predicates match {
        case None          => Some(List(BiolinkPredicate("related_to")))
        case somePredicate => somePredicate
      }
      val filteredPredicates = newPredicate.get.filter(pred => biolinkPredicates.contains(pred))
      edgeID -> edge.copy(predicates = Some(filteredPredicates))
    }
    queryGraph.copy(edges = improvedEdgeMap)
  }

  def getTRAPIEdgeKey(sub: String, pred: Option[BiolinkPredicate], obj: String): String = {
    val edgeKey = TRAPIEdgeKey(sub, pred, obj).asJson.deepDropNullValues.noSpaces
    String.format("%064x", new BigInteger(1, messageDigest.digest(edgeKey.getBytes(StandardCharsets.UTF_8))))
  }

  def getTRAPIEdges(queryGraph: TRAPIQueryGraph,
                    querySolutions: List[QuerySolution],
                    predicatesMap: Map[String, Map[IRI, BiolinkPredicate]],
                    provs: Map[TripleString, String]): ZIO[Any, Throwable, Map[String, TRAPIEdge]] =
    for {
      trapiEdges <- ZIO.foreach(querySolutions) { querySolution =>
        for {
          nodeTypeMap <- Task.effect(queryGraph.nodes.map(entry => (entry._1, IRI(querySolution.getResource(s"${entry._1}_type").getURI))))
          edges <- ZIO.foreach(queryGraph.edges) { (k, v) =>
            for {
              sourceType <- ZIO.fromOption(nodeTypeMap.get(v.subject)).orElseFail(new Exception("could not get source id"))
              targetType <- ZIO.fromOption(nodeTypeMap.get(v.`object`)).orElseFail(new Exception("could not get target id"))
              source = querySolution.getResource(v.subject).getURI
              target = querySolution.getResource(v.`object`).getURI
              predicate = querySolution.getResource(k).getURI
              tripleString = TripleString(source, predicate, target)
              provValue <- ZIO.fromOption(provs.get(tripleString)).orElseFail(new Exception("no prov value"))
              attributes = List(TRAPIAttribute(IRI(source), Some("provenance"), List(provValue), sourceType, None, None, None))
              predicatesBLMapping <- ZIO.fromOption(predicatesMap.get(k)).orElseFail(new Exception("no biolink pred mapped value"))
              blPred = predicatesBLMapping.get(IRI(predicate))
              trapiEdgeKey = getTRAPIEdgeKey(sourceType.value, blPred, targetType.value)
              trapiEdge = TRAPIEdge(blPred, None, sourceType, targetType, Some(attributes))
            } yield trapiEdgeKey -> trapiEdge
          }
        } yield edges.toList
      }
    } yield trapiEdges.flatten.toMap

  def getTRAPINodes(
    queryGraph: TRAPIQueryGraph,
    querySolutions: List[QuerySolution],
    bionlinkClasses: List[BiolinkClass],
    namedThingBiolinkClass: BiolinkClass): RIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], Map[IRI, TRAPINode]] = {
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
      nodeDetails <- getTRAPINodeDetails(allOntClassIRIs, namedThingBiolinkClass)
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
              labelAndTypes = termToLabelAndTypes.getOrElse(IRI(nodeIRI), (None, List(namedThingBiolinkClass)))
              (labelOpt, biolinkTypes) = labelAndTypes
              biolinkTypesSet = biolinkTypes.to(Set)
              nodeBiolinkTypes = bionlinkClasses.filter(c => biolinkTypesSet(c.iri))
            } yield IRI(nodeIRI) -> TRAPINode(labelOpt, Some(nodeBiolinkTypes), None)
          }
        } yield nodes.toList
      }
    } yield trapiNodes.flatten.toMap
  }

  def getTRAPINodeDetailsQueryText(nodeIdList: List[IRI], namedThingBiolinkClass: BiolinkClass): QueryText = {
    // requiring biolinkType makes some terms not be found when these results are used elsewhere - must be handled there
    val nodeIds = nodeIdList.map(n => sparql" $n ").fold(sparql"")(_ + _)
    sparql"""SELECT ?term ?biolinkType (MIN(?term_label) AS ?label)
         WHERE {
           VALUES ?term { $nodeIds }
           ?term $RDFSSubClassOf ?biolinkType .
           ?biolinkType $BiolinkMLIsA* ${namedThingBiolinkClass.iri} .
           OPTIONAL { ?term $RDFSLabel ?term_label }
         }
         GROUP BY ?term ?biolinkType"""
  }

  def getTRAPINodeDetails(
    nodeIdList: List[IRI],
    namedThingBiolinkClass: BiolinkClass): RIO[ZConfig[AppConfig] with HttpClient, List[TermWithLabelAndBiolinkType]] =
    for {
      queryText <- Task.effect(getTRAPINodeDetailsQueryText(nodeIdList, namedThingBiolinkClass))
      termsAndBiolinkTypes <- SPARQLQueryExecutor.runSelectQueryAs[TermWithLabelAndBiolinkType](queryText.toQuery)
    } yield termsAndBiolinkTypes

  def getTRAPINodeBindings(queryGraph: TRAPIQueryGraph,
                           querySolution: QuerySolution): ZIO[Any, Throwable, Map[String, List[TRAPINodeBinding]]] =
    for {
      nodeMap <- Task.effect(queryGraph.nodes.map(n => (n._1, querySolution.get(s"${n._1}_type").toString)))
      nodeBindings <- ZIO.foreach(queryGraph.nodes) { (k, v) =>
        for {
          nodeIRI <- ZIO.fromOption(nodeMap.get(k)).orElseFail(new Exception(s"Missing node IRI: $k"))
        } yield k -> List(TRAPINodeBinding(IRI(nodeIRI)))
      }
    } yield nodeBindings

  def getTRAPIEdgeBindingsMany(queryGraph: TRAPIQueryGraph,
                               querySolutions: List[QuerySolution],
                               predicatesMap: Map[String, Map[IRI, BiolinkPredicate]])
    : ZIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], Throwable, Map[QuerySolution, Map[String, List[TRAPIEdgeBinding]]]] =
    for {
      querySolutionsToEdgeBindings <- ZIO.foreach(querySolutions) { querySolution =>
        for {
          edgeBindings <- ZIO.foreach(queryGraph.edges) { (k, v) =>
            for {
              sourceType <- Task.effect(querySolution.get(s"${v.subject}_type").toString)
              targetType <- Task.effect(querySolution.get(s"${v.`object`}_type").toString)
              predicate = querySolution.getResource(k).getURI
              predicatesBLMapping <- ZIO.fromOption(predicatesMap.get(k)).orElseFail(new Exception("no biolink pred mapped value"))
              blPred = predicatesBLMapping.get(IRI(predicate))
              trapiEdgeKey = getTRAPIEdgeKey(sourceType, blPred, targetType)
              trapiEdgeBinding = List(TRAPIEdgeBinding(trapiEdgeKey))
            } yield k -> trapiEdgeBinding
          }
        } yield querySolution -> edgeBindings
      }
    } yield querySolutionsToEdgeBindings.toMap

  def getProvenanceQueryText(edges: Set[Triple]): QueryText = {
    val values = edges.map(e => sparql"( ${e.subj} ${e.pred} ${e.obj} )").fold(sparql"")(_ + _)
    sparql"""SELECT ?s ?p ?o ?g ?other
        WHERE {
          VALUES (?s ?p ?o) { $values }
          GRAPH ?g { ?s ?p ?o }
          OPTIONAL { ?g $ProvWasDerivedFrom ?other . }
        }"""
  }

  def getProvenance(edges: Set[Triple]): ZIO[ZConfig[AppConfig] with HttpClient, Throwable, Map[TripleString, String]] =
    for {
      queryText <- Task.effect(getProvenanceQueryText(edges))
      querySolutions <- SPARQLQueryExecutor.runSelectQuery(queryText.toQuery)
      triplesToGraphs <- ZIO.foreach(querySolutions) { solution =>
        Task.effect {
          val graph = if (solution.contains("other")) solution.getResource("other").getURI else solution.getResource("g").getURI
          val triple = TripleString(solution.getResource("s").getURI, solution.getResource("p").getURI, solution.getResource("o").getURI)
          triple -> graph
        }
      }
    } yield triplesToGraphs.toMap

  def getCAMStuffQueryText(prov: IRI): QueryText =
    sparql"""SELECT DISTINCT (?s_type AS ?subj) (?p AS ?pred) (?o_type AS ?obj)
         WHERE { GRAPH $prov {
             ?s ?p ?o .
             ?s $RDFType $OWLNamedIndividual .
             ?o $RDFType $OWLNamedIndividual .
           }
         ?o $SesameDirectType ?o_type .
         ?s $SesameDirectType ?s_type .
         FILTER(isIRI(?o_type))
         FILTER(isIRI(?s_type))
       }"""

  def getCAMStuff(prov: IRI): RIO[ZConfig[AppConfig] with HttpClient, List[Triple]] =
    for {
      queryText <- Task.effect(getCAMStuffQueryText(prov))
      triples <- SPARQLQueryExecutor.runSelectQueryAs[Triple](queryText.toQuery)
    } yield triples

  def getExtraKGNodes(camNodes: Set[IRI],
                      slotStuffNodeDetails: List[TermWithLabelAndBiolinkType],
                      biolinkData: BiolinkData,
                      namedThingBiolinkClass: BiolinkClass): Map[IRI, TRAPINode] = {
    val termToLabelAndTypes = slotStuffNodeDetails.groupBy(_.term).map { case (term, termsAndTypes) =>
      val (labels, biolinkTypes) = termsAndTypes.map(t => t.label -> t.biolinkType).unzip
      term -> (labels.flatten.headOption, biolinkTypes)
    }
    val nodeMap = camNodes.map { node =>
      val (labelOpt, biolinkTypes) = termToLabelAndTypes.getOrElse(node, (None, List(namedThingBiolinkClass)))
      val biolinkTypesSet = biolinkTypes.to(Set)
      val classes = biolinkData.classes.filter(c => biolinkTypesSet(c.iri))
      node -> TRAPINode(labelOpt, Some(classes), None)
    }.toMap
    nodeMap
  }

  def getSlotStuffQueryText(predicates: List[IRI]): QueryText = {
    val values = predicates.zipWithIndex
      .map { case (p, i) =>
        val id = StringUtils.leftPad(i.toString, 4, '0')
        val qid = s"e$id"
        sparql" ( $p $qid ) "
      }
      .fold(sparql"")(_ + _)
    sparql"""SELECT DISTINCT ?qid ?kid ?biolinkSlot ?label
         WHERE {
           VALUES (?kid ?qid) { $values }
           ?kid $SlotMapping ?biolinkSlot .
           ?biolinkSlot a $BiolinkMLSlotDefinition .
           OPTIONAL { ?kid $RDFSLabel ?label . }
           FILTER NOT EXISTS {
             ?kid $SlotMapping ?other .
             ?other $BiolinkMLIsA+/<https://w3id.org/biolink/biolinkml/meta/mixins>* ?biolinkSlot .
           }
         }"""
  }

  def getSlotStuff(predicates: List[IRI]): RIO[ZConfig[AppConfig] with HttpClient, List[SlotStuff]] =
    for {
      queryText <- Task.effect(getSlotStuffQueryText(predicates))
      results <- SPARQLQueryExecutor.runSelectQueryAs[SlotStuff](queryText.toQuery)
    } yield results

  def getTRAPIQEdgePredicatesQueryText(predicates: QueryText): QueryText =
    sparql"""SELECT DISTINCT ?biolinkPredicate ?predicate WHERE {
        VALUES ?biolinkPredicate { $predicates }
        ?predicate $SlotMapping ?biolinkPredicate .
        FILTER EXISTS { ?s ?predicate ?o }
        <http://www.bigdata.com/queryHints#Query> <http://www.bigdata.com/queryHints#filterExists> "SubQueryLimitOne"
     }"""

  def getTRAPIQEdgePredicates(edge: TRAPIQueryEdge): RIO[ZConfig[AppConfig] with HttpClient, Map[IRI, BiolinkPredicate]] =
    for {
      edgePredicates <- ZIO.fromOption(edge.predicates).orElseFail(new Exception("failed to get edge type"))
      queryText <- Task.effect(getTRAPIQEdgePredicatesQueryText(edgePredicates.map(a => sparql" ${a.iri} ").fold(sparql"")(_ + _)))
      predicates <- SPARQLQueryExecutor.runSelectQueryAs[Predicate](queryText.toQuery)
      ret = predicates.map(p => p.predicate -> p.biolinkPredicate).toMap
    } yield ret

}

package org.renci.cam

import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe.syntax._
import org.apache.jena.query.QuerySolution
import org.phenoscape.sparql.SPARQLInterpolation._
import org.renci.cam.Biolink.{BiolinkData, biolinkData}
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.SPARQLQueryExecutor.SPARQLCache
import org.renci.cam.Util.IterableSPARQLOps
import org.renci.cam.domain._
import zio.config.{ZConfig, getConfig}
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

  val BiolinkMLSlotDefinition: IRI = IRI("https://w3id.org/linkml/SlotDefinition")

  val BiolinkMLIsA: IRI = IRI("https://w3id.org/linkml/is_a")

  val BiolinkMLMixins: IRI = IRI("https://w3id.org/linkml/mixins")

  val RDFSSubPropertyOf: IRI = IRI("http://www.w3.org/2000/01/rdf-schema#subPropertyOf")

  val SlotMapping: IRI = IRI("http://cam.renci.org/biolink_slot")

  val BiolinkNamedThing: BiolinkClass = BiolinkClass("NamedThing", IRI(s"${BiolinkTerm.namespace}NamedThing"))

  final case class TRAPIEdgeKey(source_id: String, `type`: Option[BiolinkPredicate], target_id: String)

  final case class Triple(subj: IRI, pred: IRI, obj: IRI)

  final case class TripleString(subj: String, pred: String, obj: String)

  final case class TermWithLabelAndBiolinkType(term: IRI, biolinkType: IRI, label: Option[String])

  // instances are not thread-safe; should be retrieved for every use
  private def messageDigest: MessageDigest = MessageDigest.getInstance("SHA-256")

  def run(limit: Int,
          includeExtraEdges: Boolean,
          submittedQueryGraph: TRAPIQueryGraph): RIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData] with Has[SPARQLCache], TRAPIMessage] =
    for {
      biolinkData <- biolinkData
      _ = logger.debug("limit: {}, includeExtraEdges: {}", limit, includeExtraEdges)
      queryGraph = enforceQueryEdgeTypes(submittedQueryGraph, biolinkData.predicates)
      allPredicatesInQuery = queryGraph.edges.values.flatMap(_.predicates.getOrElse(Nil)).to(Set)
      predicatesToRelations <- mapQueryBiolinkPredicatesToRelations(allPredicatesInQuery)
      allRelationsInQuery = predicatesToRelations.values.flatten.to(Set)
      relationsToLabelAndBiolinkPredicate <- mapRelationsToLabelAndBiolink(allRelationsInQuery)
      initialQuerySolutions <- findInitialQuerySolutions(queryGraph, predicatesToRelations, limit)
      solutionTriples = extractCoreTriples(initialQuerySolutions, queryGraph)
      provs <- getProvenance(solutionTriples)
      initialKGNodes <- getTRAPINodes(queryGraph, initialQuerySolutions, biolinkData.classes)
      initialKGEdges <- getTRAPIEdges(queryGraph, initialQuerySolutions, relationsToLabelAndBiolinkPredicate, provs)
      querySolutionsToEdgeBindings <- getTRAPIEdgeBindingsMany(queryGraph, initialQuerySolutions, relationsToLabelAndBiolinkPredicate)
      trapiBindings <- ZIO.foreach(initialQuerySolutions) { querySolution =>
        getTRAPINodeBindings(queryGraph, querySolution) zip Task.effect(querySolutionsToEdgeBindings(querySolution))
      }
      _ <- ZIO.when(includeExtraEdges)(
        for {
          prov2CAMStuffTripleMap <- ZIO.foreachPar(provs.values)(prov => getCAMStuff(IRI(prov)).map(prov -> _)).map(_.toMap)
          allCAMTriples = prov2CAMStuffTripleMap.values.to(Set).flatten
          allTripleNodes = allCAMTriples.flatMap(t => Set(t.subj, t.obj))
          slotStuffNodeDetails <- getTRAPINodeDetails(allTripleNodes.to(List))
          extraKGNodes = getExtraKGNodes(allTripleNodes, slotStuffNodeDetails, biolinkData)
          allPredicates = allCAMTriples.map(_.pred)
          relationsToInfo <- mapRelationsToLabelAndBiolink(allPredicates)
          extraKGEdges = allCAMTriples.flatMap { triple =>
            for {
              (relationLabelOpt, relationBiolinkPredicate) <- relationsToInfo.get(triple.pred)
              predBLTermOpt = biolinkData.predicates.find(a => a.iri == relationBiolinkPredicate)
              key = getTRAPIEdgeKey(triple.subj.value, predBLTermOpt, triple.obj.value)
              edge = TRAPIEdge(predBLTermOpt, triple.subj, triple.obj, None)
            } yield key -> edge
          }.toMap
          _ = initialKGNodes ++ extraKGNodes
          _ = initialKGEdges ++ extraKGEdges
        } yield ()
      )
      results = trapiBindings.map { case (resultNodeBindings, resultEdgeBindings) => TRAPIResult(resultNodeBindings, resultEdgeBindings) }
    } yield TRAPIMessage(Some(queryGraph), Some(TRAPIKnowledgeGraph(initialKGNodes, initialKGEdges)), Some(results.distinct))

  def findInitialQuerySolutions(queryGraph: TRAPIQueryGraph,
                                predicatesToRelations: Map[BiolinkPredicate, Set[IRI]],
                                limit: Int): ZIO[ZConfig[AppConfig] with HttpClient, Throwable, List[QuerySolution]] = {
    val queryEdgeSparql = queryGraph.edges.map { case (queryEdgeID, queryEdge) =>
      val relationsForEdge = queryEdge.predicates.getOrElse(Nil).flatMap(predicatesToRelations.getOrElse(_, Set.empty)).to(Set)
      val predicatesQueryText = relationsForEdge.asSPARQLList
      val edgeIDVar = Var(queryEdgeID)
      val edgeSourceVar = Var(queryEdge.subject)
      val edgeTargetVar = Var(queryEdge.`object`)
      val predicatesValuesClause = sparql""" FILTER( $edgeIDVar IN ( $predicatesQueryText ) )"""
      val subjectNode = queryGraph.nodes(queryEdge.subject)
      val subjectNodeValuesClauses = getNodeValuesClauses(subjectNode.ids, subjectNode.categories, edgeSourceVar)
      val objectNode = queryGraph.nodes(queryEdge.`object`)
      val objectNodeValuesClauses = getNodeValuesClauses(objectNode.ids, objectNode.categories, edgeTargetVar)
      val nodesValuesClauses = List(subjectNodeValuesClauses, objectNodeValuesClauses).fold(sparql"")(_ + _)
      sparql"""
              $predicatesValuesClause
              $nodesValuesClauses
              $edgeSourceVar $edgeIDVar $edgeTargetVar .
            """
    }
    val projections = getProjections(queryGraph)
    val nodesToDirectTypes = getNodesToDirectTypes(queryGraph.nodes.keySet)
    val edgePatterns = queryEdgeSparql.fold(sparql"")(_ + _)
    val limitSparql = if (limit > 0) sparql" LIMIT $limit" else sparql""
    val queryString =
      sparql"""SELECT DISTINCT $projections
          WHERE {
            $nodesToDirectTypes
            $edgePatterns
          }
          $limitSparql
          """
    SPARQLQueryExecutor.runSelectQuery(queryString.toQuery)
  }

  def getNodeValuesClauses(ids: Option[List[IRI]], categories: Option[List[BiolinkClass]], edgeVar: Var): QueryText = (ids, categories) match {
    case (Some(idsList), _) =>
      sparql""" VALUES ${edgeVar}_class { ${idsList.asValues} }
                      $edgeVar $RDFType ${edgeVar}_class .
                      """
    case (None, Some(biolinkTypes)) =>
      val irisList = biolinkTypes.map(_.iri)
      sparql""" VALUES ${edgeVar}_class { ${irisList.asValues} }
                      $edgeVar $RDFType ${edgeVar}_class .
                      """
    case (None, None) => sparql""
  }

  def extractCoreTriples(solutions: List[QuerySolution], queryGraph: TRAPIQueryGraph): Set[Triple] =
    (for {
      (queryEdgeID, queryEdge) <- queryGraph.edges
      solution <- solutions
    } yield Triple(
      IRI(solution.getResource(queryEdge.subject).getURI),
      IRI(solution.getResource(queryEdgeID).getURI),
      IRI(solution.getResource(queryEdge.`object`).getURI)
    )).to(Set)

  def getNodesToDirectTypes(nodeIDs: Set[String]): QueryText =
    nodeIDs
      .map { nodeID =>
        val nodeVar = Var(nodeID)
        val nodeTypeVar = Var(s"${nodeID}_type")
        val nodeClassVar = Var(s"${nodeID}_class")
        sparql""" $nodeVar $SesameDirectType $nodeTypeVar .
                  $nodeTypeVar $RDFSSubClassOf $nodeClassVar .
              """
      }
      .fold(sparql"")(_ + _)


  def getProjections(queryGraph: TRAPIQueryGraph): QueryText = {
    val projectionVariableNames =
      queryGraph.edges.keys ++
        queryGraph.edges.flatMap(e => List(e._2.subject, e._2.`object`)) ++
        queryGraph.nodes.keys.map(queryNodeID => s"${queryNodeID}_type")
    projectionVariableNames.map(Var(_)).map(v => sparql" $v ").fold(sparql"")(_ + _)
  }

  def enforceQueryEdgeTypes(queryGraph: TRAPIQueryGraph, biolinkPredicates: List[BiolinkPredicate]): TRAPIQueryGraph = {
    val improvedEdgeMap = queryGraph.edges.map { case (edgeID, edge) =>
      val newPredicates = edge.predicates match {
        case None       => Some(List(BiolinkPredicate("related_to")))
        case Some(Nil)  => Some(List(BiolinkPredicate("related_to")))
        case predicates => predicates
      }
      val filteredPredicates = newPredicates.map(_.filter(pred => biolinkPredicates.contains(pred)))
      edgeID -> edge.copy(predicates = filteredPredicates)
    }
    val improvedNodeMap = queryGraph.nodes.map { case (nodeID, node) =>
      val newCategories = node.categories match {
        case None       => Some(List(BiolinkNamedThing))
        case Some(Nil)  => Some(List(BiolinkNamedThing))
        case categories => categories
      }
      nodeID -> node.copy(categories = newCategories)
    }
    queryGraph.copy(edges = improvedEdgeMap, nodes = improvedNodeMap)
  }

  def getTRAPIEdgeKey(sub: String, pred: Option[BiolinkPredicate], obj: String): String = {
    val edgeKey = TRAPIEdgeKey(sub, pred, obj).asJson.deepDropNullValues.noSpaces
    String.format("%064x", new BigInteger(1, messageDigest.digest(edgeKey.getBytes(StandardCharsets.UTF_8))))
  }

  def getTRAPIEdges(queryGraph: TRAPIQueryGraph,
                    querySolutions: List[QuerySolution],
                    relationsMap: Map[IRI, (Option[String], IRI)],
                    provs: Map[TripleString, String]): ZIO[ZConfig[AppConfig] with Has[BiolinkData], Throwable, Map[String, TRAPIEdge]] =

    for {
      biolinkData <- biolinkData
      appConfig <- getConfig[AppConfig]
      trapiEdges <- ZIO.foreach(querySolutions) { querySolution =>
        for {
          nodeTypeMap <- Task.effect(queryGraph.nodes.map(entry => (entry._1, IRI(querySolution.getResource(s"${entry._1}_type").getURI))))
          edges <- ZIO.foreach(queryGraph.edges) { (queryEdgeID, queryEdge) =>
            for {
              sourceType <- ZIO.fromOption(nodeTypeMap.get(queryEdge.subject)).orElseFail(new Exception("could not get source id"))
              targetType <- ZIO.fromOption(nodeTypeMap.get(queryEdge.`object`)).orElseFail(new Exception("could not get target id"))
              source = querySolution.getResource(queryEdge.subject).getURI
              target = querySolution.getResource(queryEdge.`object`).getURI
              predicate = querySolution.getResource(queryEdgeID).getURI
              predicateIRI = IRI(predicate)
              tripleString = TripleString(source, predicate, target)
              originalKS <- ZIO.fromOption(biolinkData.predicates.find(p => p.shorthand == "original_knowledge_source")).orElseFail(new Exception("could not get biolink:original_knowledge_source"))
              aggregatorKS <- ZIO.fromOption(biolinkData.predicates.find(p => p.shorthand == "aggregator_knowledge_source")).orElseFail(new Exception("could not get biolink:aggregator_knowledge_source"))
              provValue <- ZIO.fromOption(provs.get(tripleString)).orElseFail(new Exception("no prov value"))
              infoResBiolinkClass <- ZIO.fromOption(biolinkData.classes.find(p => p.shorthand == "InformationResource")).orElseFail(new Exception("could not get biolink:InformationResource"))
              aggregatorKSAttribute = TRAPIAttribute(Some("infores:cam-kp"), aggregatorKS.iri, None, List("infores:cam-kp"), Some(infoResBiolinkClass.iri), Some(appConfig.location), None, None)
              originalKSAttribute = TRAPIAttribute(Some("infores:cam-kp"), originalKS.iri, None, List("infores:go-cam"), Some(infoResBiolinkClass.iri), Some(provValue), None, None)
              attributes = List(aggregatorKSAttribute, originalKSAttribute)
              relationLabelAndBiolinkPredicate <- ZIO
                .fromOption(relationsMap.get(predicateIRI))
                .orElseFail(new Exception("Unexpected edge relation"))
              (relationLabelOpt, biolinkPredicateIRI) = relationLabelAndBiolinkPredicate
              blPred = biolinkData.predicates.find(a => a.iri == biolinkPredicateIRI)
              trapiEdgeKey = getTRAPIEdgeKey(sourceType.value, blPred, targetType.value)
              //FIXME add relation CURIE here?
              trapiEdge = TRAPIEdge(blPred, sourceType, targetType, Some(attributes))
            } yield trapiEdgeKey -> trapiEdge
          }
        } yield edges.toList
      }
    } yield trapiEdges.flatten.toMap

  def getTRAPINodes(queryGraph: TRAPIQueryGraph, querySolutions: List[QuerySolution], bionlinkClasses: List[BiolinkClass])
    : RIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], Map[IRI, TRAPINode]] = {
    val allOntClassIRIsZ = ZIO
      .foreach(querySolutions) { qs =>
        ZIO.foreach(qs.varNames.asScala.filter(_.endsWith("_type")).to(Iterable)) { typeVar =>
          ZIO.effect(IRI(qs.getResource(typeVar).getURI)).mapError { e =>
            new Exception(s"Value of _type variable $typeVar is not a URI", e)
          }
        }
      }
      .map(_.flatten)
    val allQueryNodeIDs = queryGraph.nodes.keySet
    for {
      allOntClassIRIs <- allOntClassIRIsZ
      nodeDetails <- getTRAPINodeDetails(allOntClassIRIs)
      termToLabelAndTypes = nodeDetails.groupBy(_.term).map { case (term, termsAndTypes) =>
        val (labels, biolinkTypes) = termsAndTypes.map(t => t.label -> t.biolinkType).unzip
        term -> (labels.flatten.headOption, biolinkTypes)
      }
      trapiNodes <- ZIO.foreach(querySolutions) { querySolution =>
        for {
          nodes <- ZIO.foreach(allQueryNodeIDs) { queryNodeID =>
            for {
              nodeIRI <- ZIO
                .effect(querySolution.getResource(s"${queryNodeID}_type").getURI)
                .orElseFail(new Exception(s"Missing node IRI: $queryNodeID"))
              labelAndTypes = termToLabelAndTypes.getOrElse(IRI(nodeIRI), (None, List(BiolinkNamedThing)))
              (labelOpt, biolinkTypes) = labelAndTypes
              biolinkTypesSet = biolinkTypes.to(Set)
              nodeBiolinkTypes = bionlinkClasses.filter(c => biolinkTypesSet(c.iri))
            } yield IRI(nodeIRI) -> TRAPINode(labelOpt, Some(nodeBiolinkTypes), None)
          }
        } yield nodes.toList
      }
    } yield trapiNodes.flatten.toMap
  }

  def getTRAPINodeDetailsQueryText(nodeIdList: List[IRI]): QueryText = {
    // requiring biolinkType makes some terms not be found when these results are used elsewhere - must be handled there
    val nodeIds = nodeIdList.map(n => sparql" $n ").fold(sparql"")(_ + _)
    sparql"""SELECT ?term ?biolinkType (MIN(?term_label) AS ?label)
         WHERE {
           VALUES ?term { $nodeIds }
           ?term $RDFSSubClassOf ?biolinkType .
           ?biolinkType $BiolinkMLIsA* ${BiolinkNamedThing.iri} .
           OPTIONAL { ?term $RDFSLabel ?term_label }
         }
         GROUP BY ?term ?biolinkType"""
  }

  def getTRAPINodeDetails(nodeIdList: List[IRI]): RIO[ZConfig[AppConfig] with HttpClient, List[TermWithLabelAndBiolinkType]] =
    for {
      queryText <- Task.effect(getTRAPINodeDetailsQueryText(nodeIdList))
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
                               relationsMap: Map[IRI, (Option[String], IRI)])
    : ZIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], Throwable, Map[QuerySolution, Map[String, List[TRAPIEdgeBinding]]]] =
    for {
      biolinkData <- biolinkData
      querySolutionsToEdgeBindings <- ZIO.foreach(querySolutions) { querySolution =>
        for {
          edgeBindings <- ZIO.foreach(queryGraph.edges) { (queryEdgeID, queryEdge) =>
            for {
              sourceType <- Task.effect(querySolution.get(s"${queryEdge.subject}_type").toString)
              targetType <- Task.effect(querySolution.get(s"${queryEdge.`object`}_type").toString)
              relation = querySolution.getResource(queryEdgeID).getURI
              relationIRI = IRI(relation)
              relationLabelAndBiolinkPredicate <- ZIO
                .fromOption(relationsMap.get(relationIRI))
                .orElseFail(new Exception("Unexpected edge relation"))
              (relationLabelOpt, biolinkPredicateIRI) = relationLabelAndBiolinkPredicate
              blPred = biolinkData.predicates.find(a => a.iri == biolinkPredicateIRI)
              trapiEdgeKey = getTRAPIEdgeKey(sourceType, blPred, targetType)
              trapiEdgeBinding = List(TRAPIEdgeBinding(trapiEdgeKey))
            } yield queryEdgeID -> trapiEdgeBinding
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

  def getProvenance(triples: Set[Triple]): ZIO[ZConfig[AppConfig] with HttpClient, Throwable, Map[TripleString, String]] =
    for {
      queryText <- Task.effect(getProvenanceQueryText(triples))
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
                      biolinkData: BiolinkData): Map[IRI, TRAPINode] = {
    val termToLabelAndTypes = slotStuffNodeDetails.groupBy(_.term).map { case (term, termsAndTypes) =>
      val (labels, biolinkTypes) = termsAndTypes.map(t => t.label -> t.biolinkType).unzip
      term -> (labels.flatten.headOption, biolinkTypes)
    }
    val nodeMap = camNodes.map { node =>
      val (labelOpt, biolinkTypes) = termToLabelAndTypes.getOrElse(node, (None, List(BiolinkNamedThing)))
      val biolinkTypesSet = biolinkTypes.to(Set)
      val classes = biolinkData.classes.filter(c => biolinkTypesSet(c.iri))
      node -> TRAPINode(labelOpt, Some(classes), None)
    }.toMap
    nodeMap
  }

  def mapRelationsToLabelAndBiolink(relations: Set[IRI]): RIO[ZConfig[AppConfig] with HttpClient, Map[IRI, (Option[String], IRI)]] = {
    final case class RelationInfo(relation: IRI, biolinkSlot: IRI, label: Option[String])
    val queryText = sparql"""
         SELECT DISTINCT ?relation ?biolinkSlot ?label
         WHERE {
           VALUES ?relation { ${relations.asValues} }
           ?relation $SlotMapping ?biolinkSlot .
           ?biolinkSlot a $BiolinkMLSlotDefinition .
           OPTIONAL { ?relation $RDFSLabel ?label . }
           FILTER NOT EXISTS {
             ?relation $SlotMapping ?other .
             ?other $BiolinkMLIsA+/$BiolinkMLMixins* ?biolinkSlot .
           }
         }"""
    SPARQLQueryExecutor.runSelectQueryAs[RelationInfo](queryText.toQuery).map { res =>
      res.groupMap(_.relation)(info => (info.label, info.biolinkSlot)).map { case (relationIRI, infos) => relationIRI -> infos.head }
    }
  }

  def mapQueryBiolinkPredicatesToRelations(
    predicates: Set[BiolinkPredicate]): RIO[ZConfig[AppConfig] with HttpClient with Has[SPARQLCache], Map[BiolinkPredicate, Set[IRI]]] = {
    final case class Predicate(biolinkPredicate: BiolinkPredicate, predicate: IRI)
    val queryText = sparql"""
        SELECT DISTINCT ?biolinkPredicate ?predicate WHERE {
          VALUES ?biolinkPredicate { ${predicates.asValues} }
          ?predicate $SlotMapping ?biolinkPredicate .
          FILTER EXISTS { ?s ?predicate ?o }
          <http://www.bigdata.com/queryHints#Query> <http://www.bigdata.com/queryHints#filterExists> "SubQueryLimitOne"
        }"""
    for {
      predicates <- SPARQLQueryExecutor.runSelectQueryWithCacheAs[Predicate](queryText.toQuery)
    } yield predicates.to(Set).groupMap(_.biolinkPredicate)(_.predicate)
  }

}

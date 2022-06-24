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
import org.renci.cam.domain.{TRAPIAttribute, _}
import zio.config.{ZConfig, getConfig}
import zio.stream.ZStream
import zio.{Has, RIO, Task, UIO, ZIO, config => _}

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.UUID
import scala.collection.{immutable, mutable}
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

  /**
   * This case class is a way of storing the results we retrieved from the SPARQL query alongside
   * the information we need access to.
   *
   * @param queryGraph The query graph that we are attempting to respond to.
   * @param nodes The nodes returned from the query, with node labels as the key.
   * @param edges The edges returned from the query, with edge labels as the key.
   * @param graphs The RDF graphs in which these results are asserted.
   * @param derivedFrom The graphs that the graphs in which these results are asserted were derived.
   * @see org.renci.cam.QueryService#findInitialQuerySolutions
   */
  case class Result (
    index: Long,
    queryGraph: TRAPIQueryGraph,
    nodes: Map[String, IRI],
    edges: Map[String, IRI],
    graphs: Set[IRI],
    derivedFrom: Set[IRI]
  ) {
    /**
     * Return the edge key to be used when returning this result. Ideally, this would be hashed in some way so
     * that duplicate edges were merged, but for now we can rely on ARAs to do this downstream for us. So at the
     * moment, we do the simple thing of using the index of this result and adding the edge query key to it.
     *
     * @param queryEdgeKey The edge key used in the query (e.g. `e0`).
     * @return A unique edge key for this edge in this result.
     */
    def getEdgeKey(queryEdgeKey: String): String = s"result_${index}_edge_${queryEdgeKey}"
  }

  object Result {
    /**
     * Convert a QuerySolution to a Result.
     *
     * @param qs The QuerySolution to convert.
     * @param index The index of this QuerySolution (this is stored in the Result object and used to generate the edge keys).
     * @param queryGraph The query graph used to generate this QuerySolution.
     * @return The Result this QuerySolution was converted to.
     */
    def fromQuerySolution(qs: QuerySolution, index: Long, queryGraph: TRAPIQueryGraph): Result = {
      val nodes = queryGraph.nodes.keySet.map(n => (n, IRI(qs.getResource(f"${n}_type").getURI))).toMap
      val edges = queryGraph.edges.keySet.map(e => (e, IRI(qs.getResource(e).getURI))).toMap
      val graphs = qs.getLiteral("graphs").getString.split("\\|").map(IRI(_)).toSet
      val derivedFrom = qs.getLiteral("graphs").getString.split("\\|").map(IRI(_)).toSet

      Result(
        index,
        queryGraph,
        nodes,
        edges,
        graphs,
        derivedFrom
      )
    }
  }

  /**
   * Generate the TRAPI node map (IRI -> TRAPINode) from a list of Results.
   *
   * @param results The list of Results retrieved from a query.
   * @return A ZIO that generates a Map of IRI to TRAPINode.
   * @see org.renci.cam.QueryService#getTRAPINodes
   */
  def generateTRAPINodes(results: List[Result]): RIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], Map[IRI, TRAPINode]] = {
    for {
      biolinkData <- biolinkData
      // We need to get TRAPI Node Details for these nodes so that we can fill in label and category information.
      allNodeDetails <- getTRAPINodeDetails(results.map(_.nodes).flatMap(_.values).toSet.toList)
      // Generate the nodes.
      nodes <- ZStream.fromIterable(results)
        .mapConcat(result => for {
          key <- result.nodes.keys

          // Generate the pieces of the TRAPINode.
          iri = result.nodes(key)
          nodeDetails = allNodeDetails.filter(_.term == iri)
          name = nodeDetails.flatMap(_.label).headOption
          categories = nodeDetails.map(_.biolinkType).flatMap(iri => biolinkData.classes.filter(_.iri == iri))
          categoriesAsOption = if (categories.isEmpty) None else Some(categories)
          attributes = List()

          // Generate the TRAPINode.
          trapiNode = TRAPINode(name, categoriesAsOption, Some(attributes))
        } yield (iri, trapiNode))
        .runCollect
    } yield {
      nodes.toMap
    }
  }

  /**
   * Generate TRAPI Edges from a list of results along with some additional data.
   *
   * @param results A list of Results.
   * @param relationsToLabelAndBiolinkPredicate Information on Biolink relations.
   *  TODO: generate this in this method if nobody else needs it.
   * @return A ZIO that generates a map of edge keys and TRAPIEdges.
   * @see org.renci.cam.QueryService#getTRAPIEdges
   */
  def generateTRAPIEdges(results: List[Result], relationsToLabelAndBiolinkPredicate: Map[IRI, (Option[String], IRI)]): RIO[ZConfig[AppConfig] with ZConfig[BiolinkData], Map[String, TRAPIEdge]] = {
    for {
      // Get some data we need to generate the TRAPI edges.
      biolinkData <- biolinkData
      appConfig <- getConfig[AppConfig]

      // Generate the attributes we will need to produce the edge output.
      originalKS <- ZIO.fromOption(biolinkData.predicates.find(p => p.shorthand == "original_knowledge_source")).orElseFail(new Exception("could not get biolink:original_knowledge_source"))
      aggregatorKS <- ZIO.fromOption(biolinkData.predicates.find(p => p.shorthand == "aggregator_knowledge_source")).orElseFail(new Exception("could not get biolink:aggregator_knowledge_source"))
      infoResBiolinkClass <- ZIO.fromOption(biolinkData.classes.find(p => p.shorthand == "InformationResource")).orElseFail(new Exception("could not get biolink:InformationResource"))

      // Generate the edges.
      edges <- ZStream.fromIterable(results)
        .mapConcat(result => for {
          key <- result.edges.keys

          // Generate the pieces of this TRAPIEdge.
          iri = result.edges(key)
          labelAndBiolinkPredicate = relationsToLabelAndBiolinkPredicate.get(iri)
          biolinkPredicateIRI = labelAndBiolinkPredicate.map(_._2)
          biolinkPred = biolinkData.predicates.find(a => biolinkPredicateIRI.forall(_.equals(a.iri)))
          queryEdge = result.queryGraph.edges(key)
          subjectIRI = result.nodes(queryEdge.subject)
          objectIRI = result.nodes(queryEdge.`object`)
          derivedFroms = result.derivedFrom

          // Generate the attributes for this edge.
          aggregatorKSAttribute = TRAPIAttribute(Some("infores:cam-kp"), aggregatorKS.iri, None, List("infores:cam-kp"), Some(infoResBiolinkClass.iri), Some(appConfig.location), None, None)
          originalKnowledgeSources = derivedFroms.map {
            case df@ctd if ctd.value.contains("ctdbase.org") => (df, "infores:ctd")
            case df => (df, "infores:go-cam")
          }
          originalKSAttributes = originalKnowledgeSources.map({
            case (derivedFrom, inforesKS) =>
              TRAPIAttribute(Some("infores:cam-kp"), originalKS.iri, None, List(inforesKS), Some(infoResBiolinkClass.iri), Some(derivedFrom.value), None, None)
          })
          attributes = aggregatorKSAttribute +: originalKSAttributes.toList

          // Generate the TRAPIEdge and its edge key.
          trapiEdge = TRAPIEdge(biolinkPred, subjectIRI, objectIRI, Some(attributes))
          // edgeKey = getTRAPIEdgeKey(queryEdge.subject, biolinkPred, queryEdge.`object`)
          edgeKey = result.getEdgeKey(key)
        } yield {
          (edgeKey, trapiEdge)
        }).runCollect
    } yield {
      edges.toMap
    }
  }

  /**
   * Generate TRAPI results from a list of Results.
   *
   * This is fairly straightforward: each result contains node IRIs as well as edge IRIs, but we need to generate the
   * edge key via Result.getEdgeKey()
   *
   * @param results The Results to convert into TRAPI Results.
   * @return An UIO that generates the list of TRAPI Results.
   */
  def generateTRAPIResults(results: List[Result]): UIO[List[TRAPIResult]] = {
    val stream = for {
      result <- ZStream.fromIterable(results)

      nodeBindings = result.nodes
        .groupMap(_._1)(p => TRAPINodeBinding(p._2))
        .map(p => (p._1, p._2.toList))
      edgeBindings = result.edges.keys
        .map(key => (key, List(TRAPIEdgeBinding(result.getEdgeKey(key)))))
        .toMap
    } yield {
      TRAPIResult(node_bindings = nodeBindings, edge_bindings = edgeBindings)
    }

    stream.runCollect.map(_.toList)
  }

  /**
   * Query the triplestore with a TRAPIQuery and return a TRAPIMessage with the result.
   *
   * @param limit The maximum number of results to return.
   * @param includeExtraEdges Include additional information about the returned nodes.
   * @param submittedQueryGraph The query graph to search the triplestore with.
   * @return A TRAPIMessage displaying the results.
   */
  def run(limit: Int,
          includeExtraEdges: Boolean,
          submittedQueryGraph: TRAPIQueryGraph): RIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData] with Has[SPARQLCache], TRAPIMessage] =
    for {
      // Get the Biolink data.
      biolinkData <- biolinkData
      _ = logger.debug("limit: {}, includeExtraEdges: {}", limit, includeExtraEdges)

      // Prepare the query graph for processing.
      queryGraph = enforceQueryEdgeTypes(submittedQueryGraph, biolinkData.predicates)

      // Generate the relationsToLabelAndBiolinkPredicate.
      allPredicatesInQuery = queryGraph.edges.values.flatMap(_.predicates.getOrElse(Nil)).to(Set)
      predicatesToRelations <- mapQueryBiolinkPredicatesToRelations(allPredicatesInQuery)
      allRelationsInQuery = predicatesToRelations.values.flatten.to(Set)
      relationsToLabelAndBiolinkPredicate: Map[IRI, (Option[String], IRI)] <- mapRelationsToLabelAndBiolink(allRelationsInQuery)

      // Generate query solutions.
      _ = logger.debug(s"findInitialQuerySolutions(${queryGraph}, ${predicatesToRelations}, ${limit})")
      initialQuerySolutions <- findInitialQuerySolutions(queryGraph, predicatesToRelations, limit)
      results = initialQuerySolutions.zipWithIndex.map({
        case (qs, index) => Result.fromQuerySolution(qs, index, queryGraph)
      })
      _ = logger.debug(s"Results: ${results}")

      // From the results, generate the TRAPI nodes, edges and results.
      nodes <- generateTRAPINodes(results)
      _ = logger.debug(s"Nodes: ${nodes}")
      edges <- generateTRAPIEdges(results, relationsToLabelAndBiolinkPredicate)
      _ = logger.debug(s"Edges: ${edges}")
      trapiResults <- generateTRAPIResults(results)
      _ = logger.debug(s"Results: ${trapiResults}")
    } yield {
      TRAPIMessage(Some(queryGraph), Some(TRAPIKnowledgeGraph(nodes, edges)), Some(trapiResults.distinct))
    }

  def oldRun(limit: Int,
          includeExtraEdges: Boolean,
          submittedQueryGraph: TRAPIQueryGraph): RIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData] with Has[SPARQLCache], TRAPIMessage] =
    for {
      biolinkData <- biolinkData
      _ = logger.warn("limit: {}, includeExtraEdges: {}", limit, includeExtraEdges)
      queryGraph = enforceQueryEdgeTypes(submittedQueryGraph, biolinkData.predicates)
      allPredicatesInQuery = queryGraph.edges.values.flatMap(_.predicates.getOrElse(Nil)).to(Set)
      predicatesToRelations <- mapQueryBiolinkPredicatesToRelations(allPredicatesInQuery)
      allRelationsInQuery = predicatesToRelations.values.flatten.to(Set)
      relationsToLabelAndBiolinkPredicate: Map[IRI, (Option[String], IRI)] <- mapRelationsToLabelAndBiolink(allRelationsInQuery)
      _ = logger.warn(s"findInitialQuerySolutions(${queryGraph}, ${predicatesToRelations}, ${limit})")
      initialQuerySolutions <- findInitialQuerySolutions(queryGraph, predicatesToRelations, limit)
      _ = logger.warn(s"Initial query solutions: ${initialQuerySolutions.length} (limit: ${limit}): ${initialQuerySolutions}")
      _ = logger.warn(s"extractCoreTriples(${initialQuerySolutions}, ${queryGraph})")
      solutionTriples = extractCoreTriples(initialQuerySolutions, queryGraph)
      _ = logger.warn(s"Solution triples: ${solutionTriples.size} (limit: ${limit}): ${solutionTriples}")
      provs <- getProvenance(solutionTriples)
      _ = logger.warn(s"provs: ${provs.size} (limit: ${limit}): ${provs}")
      _ = logger.warn(s"getTRAPINodes(${queryGraph}, ${initialQuerySolutions}, ${biolinkData.classes})")
      initialKGNodes <- getTRAPINodes(queryGraph, initialQuerySolutions, biolinkData.classes)
      _ = logger.warn(s"initialKGNodes: ${initialKGNodes.size} (limit: ${limit}): ${initialKGNodes}")
      _ = logger.warn(s"getTRAPIEdges(${queryGraph}, ${initialQuerySolutions}, ${relationsToLabelAndBiolinkPredicate}, ${provs})")
      initialKGEdges <- getTRAPIEdges(queryGraph, initialQuerySolutions, relationsToLabelAndBiolinkPredicate, provs)
      _ = logger.warn(s"initialKGEdges: ${initialKGEdges.size} (limit: ${limit}): ${initialKGEdges}")
      _ = logger.warn(s"getTRAPIEdgeBindingsMany(${queryGraph}, ${initialQuerySolutions}, ${relationsToLabelAndBiolinkPredicate})")
      querySolutionsToEdgeBindings <- getTRAPIEdgeBindingsMany(queryGraph, initialQuerySolutions, relationsToLabelAndBiolinkPredicate)
      _ = logger.warn(s"querySolutionsToEdgeBindings: ${querySolutionsToEdgeBindings} (length: ${querySolutionsToEdgeBindings.size}, limit: ${limit})")
      trapiBindings <- ZIO.foreach(initialQuerySolutions) { querySolution =>
        getTRAPINodeBindings(queryGraph, querySolution) zip Task.effect(querySolutionsToEdgeBindings(querySolution))
      }
      _ = logger.warn(s"trapiBindings: ${trapiBindings.length} (limit: ${limit})")
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
      _ = logger.warn(s"results: ${results} (length: ${results.length}, limit: ${limit})")
      _ = logger.warn(s"results distinct: ${results.distinct} (length: ${results.distinct.length}, limit: ${limit})")
    } yield TRAPIMessage(Some(queryGraph), Some(TRAPIKnowledgeGraph(initialKGNodes, initialKGEdges)), Some(results.distinct))

  /**
   * Construct and execute a SPARQL query based on the provided query graph in the triplestore.
   *
   * @param queryGraph The query graph describing the
   * @param predicatesToRelations Mappings between Biolink predicates and their corresponding IRIs in the triplestore.
   * @param limit The number of results to return.
   * @return A ZIO that resolves to a list of QuerySolutions
   * @see org.renci.cam.QueryService#Result for a more human-readable version of the query solution.
   */
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
              $nodesValuesClauses
              GRAPH ?g {
                $predicatesValuesClause
                $edgeSourceVar $edgeIDVar $edgeTargetVar .
              }
            """
    }
    val node_projections = getProjections(queryGraph)
    val type_projections = getProjections(queryGraph, true)
    val nodesToDirectTypes = getNodesToDirectTypes(queryGraph.nodes.keySet)
    val edgePatterns = queryEdgeSparql.fold(sparql"")(_ + _)
    val limitSparql = if (limit > 0) sparql" LIMIT $limit" else sparql""
    val queryString =
      sparql"""PREFIX hint: <http://www.bigdata.com/queryHints#>

          SELECT DISTINCT $type_projections
               (GROUP_CONCAT(DISTINCT ?g; SEPARATOR='|') AS ?graphs)
               (GROUP_CONCAT(DISTINCT ?d; SEPARATOR='|') AS ?derivedFrom)
          WHERE {
            $nodesToDirectTypes
            OPTIONAL { ?g $ProvWasDerivedFrom ?d }
            {
              SELECT $node_projections ?g
              WHERE {
                $edgePatterns
              }
            }
            hint:Prior hint:runFirst true .
          }
          GROUP BY $type_projections
          $limitSparql
          """
    logger.debug(s"Executing query: ${queryString}")
    SPARQLQueryExecutor.runSelectQuery(queryString.toQuery)
  }

  def getNodeValuesClauses(ids: Option[List[IRI]], categories: Option[List[BiolinkClass]], edgeVar: Var): QueryText = (ids, categories) match {
    case (Some(idsList), _) =>
      sparql""" VALUES ${edgeVar}_class { ${idsList.asValues} }
                      $edgeVar $RDFType ${edgeVar}_class .
                      """
    case (None, Some(List(BiolinkNamedThing))) =>
      // Since everything is a Biolink Named Thing, we don't actually need to filter to it.
      sparql""
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


  /**
   * There are two "projections" that we need to generate for use in SPARQL:
   *  - If typesInsteadOfNodes is false (the default), we should generate ?n0 ?e0 ?n1 for a one-hop.
   *  - If typesInsteadOfNodes is true, we should generate ?n0_type ?e0 ?n1_type for a one-hop.
   *
   * @param queryGraph The query graph to generate projections for.
   * @param typesInsteadOfNodes Use `_type` projections instead of just the raw node projections.
   * @return A string listing all the projects for use in a SPARQL query.
   */
  def getProjections(queryGraph: TRAPIQueryGraph, typesInsteadOfNodes: Boolean = false): QueryText = {
    val projectionVariableNames =
      queryGraph.edges.keys ++
        (if(typesInsteadOfNodes) queryGraph.nodes.keys.map(queryNodeID => s"${queryNodeID}_type")
        else queryGraph.edges.flatMap(e => List(e._2.subject, e._2.`object`)))
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
              originalKSstr = provValue match {
                case ctd if provValue.contains("ctdbase.org") => "infores:ctd"
                case _ => "infores:go-cam"
              }
              originalKSAttribute = TRAPIAttribute(Some("infores:cam-kp"), originalKS.iri, None, List(originalKSstr), Some(infoResBiolinkClass.iri), Some(provValue), None, None)
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

  // TODO: This would be a useful method to cache.
  def getTRAPINodeDetails(nodeIdList: List[IRI]): RIO[ZConfig[AppConfig] with HttpClient, List[TermWithLabelAndBiolinkType]] =
    for {
      queryText <- Task.effect(getTRAPINodeDetailsQueryText(nodeIdList))
      termsAndBiolinkTypes <- SPARQLQueryExecutor.runSelectQueryAs[TermWithLabelAndBiolinkType](queryText.toQuery)
      _ = logger.debug(s"getTRAPINodeDetails(${nodeIdList}) = ${termsAndBiolinkTypes}")
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
    : ZIO[Has[BiolinkData], Throwable, Map[QuerySolution, Map[String, List[TRAPIEdgeBinding]]]] =
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

  // TODO:
  // - Change this to cached queries (see mapQueryBiolinkPredicatesToRelations for example)
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

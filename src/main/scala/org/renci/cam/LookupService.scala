package org.renci.cam

import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe._
import org.apache.jena.query.QuerySolution
import org.http4s.{HttpRoutes, Uri}
import org.phenoscape.sparql.SPARQLInterpolation._
import org.renci.cam.Biolink.{biolinkData, BiolinkData}
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.Server.EndpointEnv
import org.renci.cam.Server.LocalTapirJsonCirce.jsonBody
import org.renci.cam.Util.IterableSPARQLOps
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, IRI, PredicateMappings, QualifiedBiolinkPredicate}
import sttp.tapir.Endpoint
import sttp.tapir.generic.auto._
import sttp.tapir.server.http4s.ztapir.ZHttp4sServerInterpreter
import sttp.tapir.ztapir.{endpoint, query}
import zio.blocking.{effectBlockingIO, Blocking}
import zio.config.ZConfig
import zio.{Has, RIO, URIO, ZIO}

import scala.io.Source

/** The LookupService can be used to look up concepts within CAM-KP-API without needing to use either high-level TRAPI queries or low-level
  * SPARQL queries. It is intended to provide a middle path to interrogate the database and to identify cases where the TRAPI interface
  * (QueryService) doesn't provide the correct results.
  */
object LookupService extends LazyLogging {

  /* DATA STRUCTURES */

  /** This case class represents a single triple from our database.
    *
    * @param s
    *   Subject
    * @param p
    *   Predicate
    * @param o
    *   Object
    * @param g
    *   Graph
    */
  case class ResultTriple(
    s: String,
    p: String,
    o: String,
    g: String
  )

  /** A helper method for converting a QuerySolution with ?s, ?p, ?o, ?g into a ResultTriple.
    *
    * If we were concerned about correctly formatting different kinds of RDF nodes (e.g. literals, IRIs, etc.), we would match their types
    * and handle them differently here. However, a ResultTriple is just a series of strings to be sent back to the user as a JSON object, so
    * it's a lot easier to just convert it into a string here.
    */
  def fromQuerySolution(result: QuerySolution): ResultTriple = ResultTriple(
    result.get("s").toString,
    result.get("p").toString,
    result.get("o").toString,
    result.get("g").toString
  )

  /** A labeled IRI is an IRI with an optional label.
    *
    * @param iri
    *   An IRI.
    * @param label
    *   An optional label for this IRI.
    */
  case class LabeledIRI(
    iri: String,
    label: Set[String]
  )

  /** This case class represents a relation between the subject object and other objects in CAM-KP.
    *
    * This should really be a TRAPI object, but since there isn't a TRAPI object that includes both a node and an edge, we make one up here.
    *
    * @param subj
    *   The subject of this relation.
    * @param preds
    *   The predicates connecting this subject to this object.
    * @param biolinkPredicates
    *   The predicates in `preds` translated into Biolink predicates. The predicate IRIs are used as the keys.
    * @param obj
    *   The object of this relation.
    * @param objRelations
    *   The relations connected to the object of this relation. CAM-KP is designed so that this list is not necessary, since `subj` should
    *   be connected to every other entity in its graph (even if only by a `biolink:related_to` predicate); however, it is intended to test
    *   whether this is in fact the case. TODO: not yet implemented.
    * @param g
    *   The graph that this relation comes from.
    */
  case class Relation(
    subj: Set[LabeledIRI],
    preds: Set[LabeledIRI],
    biolinkQualifiedPredicates: Map[String, Set[QualifiedBiolinkPredicate]],
    obj: Set[LabeledIRI],
    objRelations: Seq[Relation],
    g: Set[String]
  )

  /** The result of calling `/lookup` on an identifier.
    *
    * @param queryId
    *   The identifier queried with lookup.
    * @param normalizedIds
    *   The list of normalized identifiers for the queryId with their labels. If the node could not be normalized, this will be a single
    *   entry, which consists of queryId without any labels. All of these identifiers will be used in the query.
    * @param biolinkPredicates
    *   All predicates connected to the identifier being looked up.
    * @param relations
    *   All relations connected to the identifier being looked up.
    * @param subjectTriples
    *   All subject triples for the identifier being looked up (i.e. triples in the triplestore in the shape `normalizedId ?p ?o`).
    * @param objectTriples
    *   All object triples for the identifier being looked up (i.e. triples in the triplestore in the shape `?s ?p normalizedId`)
    */
  case class Result(
    queryId: String,
    normalizedIds: List[LabeledIRI],
    biolinkQualifiedPredicates: Set[QualifiedBiolinkPredicate],
    relations: Seq[Relation],
    subjectTriples: Seq[ResultTriple],
    objectTriples: Seq[ResultTriple]
  )

  /** An error returned by calling `/lookup`. */
  case class Error(
    code: String,
    message: String
  )

  /* CONTROLLER */
  type LookupEndpointParams = (String, Int, Option[String], String)
  type LookupEndpoint = Endpoint[LookupEndpointParams, Error, Result, Any]

  /* Helper methods */

  /** Returns a list of Relations for a particular subject.
    *
    * TODO: in a future version, this will call itself recursively so we can fill in objRelations.
    *
    * @param qualifiedIds
    *   The subject IRIs to find relations for.
    */
  def getRelations(
    qualifiedIds: Set[String]): ZIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], Throwable, Iterable[Relation]] =
    for {
      biolinkData <- biolinkData

      subjectIRIs = {
        import biolinkData.implicits._

        qualifiedIds.map(qualifiedId => iriKeyDecoder(qualifiedId).get)
      }

      relationsQuery =
        sparql"""SELECT DISTINCT ?subj ?subjLabel ?p ?pLabel ?obj ?objLabel ?g {
                  ?s ${QueryService.SesameDirectType} ?subj .
                  ?subj ${QueryService.RDFSSubClassOf} ?subjClass .
                  VALUES ?subjClass { ${subjectIRIs.asValues} } .
                  
                  ?o ${QueryService.SesameDirectType} ?obj .
                  ?obj ${QueryService.RDFSSubClassOf} ${QueryService.BiolinkNamedThing.iri} .

                  GRAPH ?g {
                    ?s ?p ?o
                  }

                  OPTIONAL { ?subjClass ${QueryService.RDFSLabel} ?subjLabel }
                  OPTIONAL { ?p ${QueryService.RDFSLabel} ?pLabel }
                  OPTIONAL { ?obj ${QueryService.RDFSLabel} ?objLabel }
               }"""
      relationsResults <- SPARQLQueryExecutor.runSelectQuery(relationsQuery.toQuery)
      preds = relationsResults.map(_.getResource("p").getURI).map(IRI(_))

      // We want to group these by object, so we don't return a gazillion predicates for each result.
      objectMap = relationsResults.groupBy(_.getResource("obj").getURI)

      relations = objectMap.map { case (obj, results) =>
        val predResults = results.flatMap { res =>
          val predIRI = res.getResource("p").getURI

          val pred = res.getLiteral("pLabel") match {
            case null => LabeledIRI(predIRI, Set())
            case lit  => LabeledIRI(predIRI, Set(lit.getString))
          }

          val biolinkQualifiedPreds = PredicateMappings.getBiolinkQualifiedPredicates(IRI(predIRI))

          // TODO: add support for qualified predicates.
          biolinkQualifiedPreds.map(bp => (pred, bp.biolinkPredicate, bp.qualifierList))
        }

        val objLabeled = results
          .map(
            { res =>
              res.getLiteral("objLabel") match {
                case null => LabeledIRI(res.getResource("obj").getURI, Set())
                case lit  => LabeledIRI(res.getResource("obj").getURI, Set(lit.getString))
              }
            }
          )
          .toSet

        val subjLabeled = results
          .map(
            { res =>
              res.getLiteral("subjLabel") match {
                case null => LabeledIRI(res.getResource("subj").getURI, Set())
                case lit  => LabeledIRI(res.getResource("subj").getURI, Set(lit.getString))
              }
            }
          )
          .toSet

        Relation(
          subjLabeled,
          predResults.map(_._1).toSet,
          predResults.map(kv => (kv._1.iri, QualifiedBiolinkPredicate(kv._2, kv._3))).groupMapReduce(_._1)(p => Set(p._2))(_ ++ _),
          objLabeled,
          Seq[Relation](), // TODO: recurse!
          results.map(_.getResource("g").getURI).toSet
        )
      }
    } yield relations

  /* Node Norm structures and functions */

  /* Node Norm result structures */

  /** Node Norm identifier */
  case class NodeNormIdentifier(
    identifier: String,
    label: Option[String]
  ) {
    val asLabeledIRI = LabeledIRI(identifier, label.toSet)
  }

  /** Node Norm response */
  case class NodeNormResponse(
    id: Option[NodeNormIdentifier],
    equivalent_identifiers: List[NodeNormIdentifier],
    `type`: List[BiolinkClass],
    information_content: Option[Float]
  )

  /** Retrieve a list of all equivalent identifiers for a queryId.
    *
    * @param queryId
    *   The identifier being queried.
    * @param nodeNormURL
    *   The NodeNorm URL (ending with `/get_normalized_nodes`) to query.
    * @param conflate
    *   What to set in the `conflate` param to NodeNorm. Currently, may be `true` or `false`.
    */
  def getQualifiedIdsFromNodeNorm(queryId: String,
                                  nodeNormURL: String,
                                  conflate: String): ZIO[Blocking with Has[BiolinkData], Exception, List[LabeledIRI]] =
    for {
      biolinkData <- biolinkData
      nnUri <- ZIO.fromEither(Uri.fromString(nodeNormURL))
      uri = nnUri.withQueryParam("curie", queryId).withQueryParam("conflate", conflate).toString()
      strResult <- effectBlockingIO(Source.fromURL(uri)).bracketAuto { src =>
        val lines = src.getLines().mkString

        logger.info(s"Queried ${uri}, got result: ${lines}.")

        ZIO.succeed(lines)
      }
      jsonResult <- ZIO.fromEither(parser.parse(strResult))
      qualifiedIds = (jsonResult \\ queryId).flatMap { res =>
        // Decode IRIs from NodeNorm into Biolink entities and IRIs.
        import biolinkData.implicits._

        res
          .as[NodeNormResponse]
          .fold(
            cause => {
              logger.warn(s"Could not parse server response as a NodeNormResponse (cause: ${cause}): ${strResult}.")
              Set(LabeledIRI(queryId, Set()))
            },
            nnr => nnr.equivalent_identifiers.map(_.asLabeledIRI)
          )
      }
    } yield qualifiedIds

  /** Lookup a particular subject.
    *
    * @param queryId
    *   The IRI of a subject to investigate.
    * @param nodeNormURL
    *   The URL of NodeNorm to call for normalization (should end with '/get_normalized_nodes'), or None if we should not normalize.
    * @param hopLimit
    *   The maximum number of hops to follow. (TODO: implement this)
    * @return
    *   A ZIO returning a Result to the user.
    */
  def lookup(queryId: String, hopLimit: Int, nodeNormURL: Option[String], conflation: String): ZIO[EndpointEnv, Throwable, Result] = for {
    biolinkData <- biolinkData
    _ = logger.debug(s"lookup(${queryId}, ${hopLimit}, ${nodeNormURL}, ${conflation})")

    // Retrieve id_prefixes we support.
    defaultQualifiedIds: Set[LabeledIRI] = Set(LabeledIRI(queryId, Set()))
    qualifiedIds <- nodeNormURL match {
      case None      => ZIO.succeed(defaultQualifiedIds)
      case Some(nnu) => getQualifiedIdsFromNodeNorm(queryId, nnu, conflation)
    }

    // Normalize subjectIRI using NodeNorm.
    subjectIRIs = {
      import biolinkData.implicits._
      logger.debug(s"Normalized identifier to ${qualifiedIds} using ${nodeNormURL}")

      qualifiedIds.map(qualifiedId => iriKeyDecoder(qualifiedId.iri).get)
    }

    // Get every triple that has the subject as a subject.
    subjectTriplesQueryString = sparql"""SELECT DISTINCT ?s ?p ?o ?g { GRAPH ?g { ?s ?p ?o } . VALUES ?s { ${subjectIRIs.asValues} }}"""
    subjectTriples <- SPARQLQueryExecutor.runSelectQuery(subjectTriplesQueryString.toQuery)
    _ = logger.debug(s"SPARQL query for subjectTriples: ${subjectTriplesQueryString.toQuery}")
    _ = logger.debug(s"Results for subjectTriples: ${subjectTriples}")

    // Get every triple that has the subject as an object.
    objectTriplesQueryString = sparql"""SELECT DISTINCT ?s ?p ?o ?g { GRAPH ?g { ?s ?p ?o } . VALUES ?o { ${subjectIRIs.asValues} }}"""
    objectTriples <- SPARQLQueryExecutor.runSelectQuery(objectTriplesQueryString.toQuery)
    _ = logger.debug(s"SPARQL query for objectTriples: ${objectTriplesQueryString.toQuery}")
    _ = logger.debug(s"Results for objectTriples: ${objectTriples}")

    // Get every relation from this subject.
    relations <- getRelations(qualifiedIds.map(_.iri).toSet)
    biolinkPredicates = relations.flatMap(_.biolinkQualifiedPredicates.values).flatten
  } yield Result(
    queryId,
    qualifiedIds.toList,
    biolinkPredicates.toSet,
    relations.toSeq,
    subjectTriples.map(fromQuerySolution),
    objectTriples.map(fromQuerySolution)
  )

  /* HTTP4S INTERFACE */
  val lookupEndpointZ: URIO[Has[BiolinkData], LookupEndpoint] =
    for {
      biolinkData <- biolinkData
    } yield {
      import biolinkData.implicits._

      // The encoded example asks what biological process or activities positively regulate GO:0004707
      // (MAP kinase activity, see http://purl.obolibrary.org/obo/GO_0004707)
      endpoint.get
        .in("lookup")
        .in(
          query[String]("subject")
            .default("NCBIGene:478")
        )
        .in(
          query[Int]("hopLimit")
            .description("The number of hops to recurse in objRelations")
            .default(10)
            .example(10)
        )
        .in(
          query[Option[String]]("nodeNormURL")
            .description(
              "The URL where the Translator NodeNormalizer can be found -- if blank or missing, no node normalization will occur")
            .default(None)
            .example(Some("https://nodenorm.transltr.io/1.3/get_normalized_nodes"))
        )
        .in(
          query[String]("conflate")
            .description("Whether or not to use conflation (should be 'true' or 'false')")
            .default("true")
            .example("true")
        )
        .out(jsonBody[Result])
        .errorOut(jsonBody[Error])
        .summary("Look up relationships within CAM-KP-API by identifier.")
    }

  def lookupRouteR(lookupEndpoint: LookupEndpoint): HttpRoutes[RIO[EndpointEnv, *]] =
    ZHttp4sServerInterpreter[EndpointEnv]()
      .from(lookupEndpoint) { case (subj, hopLimit, nodeNormURL, conflation) =>
        lookup(subj, hopLimit, nodeNormURL, conflation)
          .mapError(ex => Error("interp_error", ex.getMessage))
      }
      .toRoutes

}

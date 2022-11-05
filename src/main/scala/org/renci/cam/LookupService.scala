package org.renci.cam

import com.typesafe.scalalogging.LazyLogging
import io.circe
import sttp.tapir.generic.auto._
import org.phenoscape.sparql.SPARQLInterpolation._
import io.circe.generic.semiauto._
import io.circe.generic.auto._
import io.circe.{parser, Decoder, DecodingFailure, Encoder, Json, KeyDecoder, KeyEncoder}
import org.apache.jena.query.QuerySolution
import org.apache.jena.rdf.model.{RDFNode, Resource}
import org.http4s.{HttpRoutes, InvalidBodyException, Uri}
import org.renci.cam.Biolink.{biolinkData, BiolinkData}
import org.renci.cam.SPARQLQueryExecutor.SPARQLCache
import org.renci.cam.Server.EndpointEnv
import org.renci.cam.Server.LocalTapirJsonCirce.jsonBody
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, IRI, LogEntry, TRAPIMessage, TRAPINode, TRAPIQuery, TRAPIQueryEdge, TRAPIQueryGraph, TRAPIQueryNode, TRAPIResponse}
import sttp.tapir.Endpoint
import sttp.tapir.server.http4s.ztapir.ZHttp4sServerInterpreter
import sttp.tapir.ztapir.{endpoint, query, stringBody}
import zio.{Has, RIO, Task, UIO, URIO, ZIO}
import org.http4s.implicits._
import org.renci.cam.HttpClient.HttpClient
import zio.blocking.{effectBlockingIO, Blocking}
import zio.config.ZConfig
import org.renci.cam.Util.IterableSPARQLOps

import java.io.IOException
import java.net.URL
import scala.collection.immutable
import scala.io.{BufferedSource, Source}
import scala.language.implicitConversions
import scala.util.Try

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

  def fromQuerySolution(result: QuerySolution): ResultTriple = ResultTriple(
    result.get("s").toString,
    result.get("p").toString,
    result.get("o").toString,
    result.get("g").toString
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
    *   The predicates in `pred` translated into Biolink predicates. The predicate IRIs are used as the keys.
    * @param obj
    *   The object of this relation.
    * @param objRelations
    *   The relations connected to the object of this relation. CAM-KP is designed so that this list is not necessary, since `subj` should
    *   be connected to every other entity in its graph (even if only by a `biolink:related_to` predicate); however, it is intended to test
    *   whether this is in fact the case.
    * @param g
    *   The graph that this relation comes from.
    */
  case class Relation(
    subj: Set[LabeledIRI],
    preds: Set[LabeledIRI],
    biolinkPredicates: Map[String, Set[LabeledIRI]],
    obj: Set[LabeledIRI],
    objRelations: Seq[Relation],
    g: Set[String]
  )

  /** The result of calling `/lookup` on an identifier.
    *
    * @param queryId
    *   The identifier queried with lookup.
    * @param normalizedId
    *   The identifier actually used for the query. This will be identical to queryId if normalization was not requested or
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
    normalizedIds: Set[LabeledIRI],
    biolinkPredicates: Map[String, Set[LabeledIRI]],
    relations: Seq[Relation],
    subjectTriples: Seq[ResultTriple],
    objectTriples: Seq[ResultTriple]
  )

  /** An error returned by calling `/lookup`. */
  case class Error(
    code: String,
    message: String
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

  /* CONTROLLER */
  type LookupEndpointParams = (String, Int, Option[String], String)
  type LookupEndpoint = Endpoint[LookupEndpointParams, Error, Result, Any]

  /* Helper methods */

  /** Returns a list of Relations for a particular subject.
    *
    * This function is intended to be called recursively: it will call itself to get all the relations for every subject of a relation.
    *
    * @param subject
    *   The subject IRI to find relations for.
    * @param hopLimit
    *   The number of hops to find relations for. If equal to zero, this will return an empty Seq.
    * @param subjectsPreviouslyQueried
    *   Subject IRIs that have already been queried. We will not recurse into these subject IRIs.
    * @return
    *   A ZIO that returns a Relation, possibly containing multiple Relations in its objRelations field.
    */
  def getRelations(
    qualifiedIds: Set[String],
    hopLimit: Int,
    subjectsPreviouslyQueried: Set[String]): ZIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], Throwable, Iterable[Relation]] =
    for {
      biolinkData <- biolinkData

      subjectIRIs = {
        implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)

        qualifiedIds.map(qualifiedId => iriKeyDecoder(qualifiedId).get)
      }

      relationsQuery =
        sparql"""PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
               PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

               SELECT DISTINCT ?subj ?subjLabel ?p ?pLabel ?obj ?objLabel ?g {
                  ?s <http://www.openrdf.org/schema/sesame#directType> ?subj .
                  ?subj <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?subjClass .
                  VALUES ?subjClass { ${subjectIRIs.asValues} } .
                  
                  ?o <http://www.openrdf.org/schema/sesame#directType> ?obj .
                  ?obj <http://www.w3.org/2000/01/rdf-schema#subClassOf> <https://w3id.org/biolink/vocab/NamedThing> .

                  GRAPH ?g {
                    ?s ?p ?o
                  }

                  OPTIONAL { ?subjClass rdfs:label ?subjLabel }
                  OPTIONAL { ?p rdfs:label ?pLabel }
                  OPTIONAL { ?obj rdfs:label ?objLabel }
               }"""
      relationsResults <- SPARQLQueryExecutor.runSelectQuery(relationsQuery.toQuery)
      preds = relationsResults.map(_.getResource("p").getURI).map(IRI(_))
      biolinkRelationMap <- QueryService.mapRelationsToLabelAndBiolink(preds.toSet)

      // We want to group these by object, so we don't return a gazillion predicates for each result.
      objectMap = relationsResults.groupBy(_.getResource("obj").getURI)

      relations = objectMap map { case (obj, results) =>
        val predResults = results map { res =>
          val predIRI = res.getResource("p").getURI

          val pred = res.getLiteral("pLabel") match {
            case null => LabeledIRI(predIRI, Set())
            case lit  => LabeledIRI(predIRI, Set(lit.getString))
          }

          val biolinkRes = biolinkRelationMap.get(IRI(predIRI))
          val biolink = biolinkRes match {
            case None                     => Set[LabeledIRI]()
            case Some((None, iri))        => Set(LabeledIRI(iri.value, Set()))
            case Some((Some(label), iri)) => Set(LabeledIRI(iri.value, Set(label)))
          }

          (pred, biolink)
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
          predResults.map(kv => (kv._1.iri, kv._2)).toMap,
          objLabeled,
          Seq[Relation](), // TODO: recurse!
          results.map(_.getResource("g").getURI).toSet
        )
      }
    } yield relations

  /* Node Norm result structures */
  case class NodeNormIdentifier(
    identifier: String,
    label: Option[String]
  ) {
    val asLabeledIRI = LabeledIRI(identifier, label.toSet)
  }

  case class NodeNormResponse(
    id: Option[NodeNormIdentifier],
    equivalent_identifiers: List[NodeNormIdentifier],
    `type`: List[BiolinkClass],
    information_content: Option[Float]
  )

  def getQualifiedIdsFromNodeNorm(queryId: String, nodeNormURL: String, conflation: String) =
    for {
      nnUri <- ZIO.fromEither(Uri.fromString(nodeNormURL))
      // TODO: convert this into ZIO, I guess.
      strResult = {
        val uri = nnUri.+?("curie", queryId).+?("conflate", conflation).toString()
        val s = Source.fromURL(uri)
        val result = s.mkString
        s.close()

        logger.warn(s"Queried ${uri}, got result: ${result}.")

        result
      }
      jsonResult <- ZIO.fromEither(parser.parse(strResult))
      qualifiedIds = jsonResult
        .as[NodeNormResponse]
        .fold(
          cause => {
            logger.warn(s"Could not parse server response as a NodeNormResponse (cause: ${cause}): ${strResult}.")
            Set(LabeledIRI(queryId, Set()))
          },
          nnr => nnr.equivalent_identifiers.map(_.asLabeledIRI).toSet
        )
    } yield qualifiedIds

  /** Lookup a particular subject.
    *
    * @param queryId
    *   The IRI of a subject to investigate.
    * @param nodeNormURL
    *   The URL of NodeNorm to call for normalization (should end with '/get_normalized_nodes'), or None if no normalization is required.
    * @param hopLimit
    *   The maximum number of hops to follow.
    * @return
    *   A ZIO returning a Result to the user.
    */
  def lookup(queryId: String, hopLimit: Int, nodeNormURL: Option[String], conflation: String): ZIO[EndpointEnv, Throwable, Result] = for {
    biolinkData <- biolinkData

    // Retrieve id_prefixes we support.
    defaultQualifiedIds: Set[LabeledIRI] = Set(LabeledIRI(queryId, Set()))
    qualifiedIds <- nodeNormURL match {
      case None      => ZIO.succeed(defaultQualifiedIds)
      case Some(nnu) => getQualifiedIdsFromNodeNorm(queryId, nnu, conflation)
    }

    // Normalize subjectIRI using NodeNorm.
    subjectIRIs = {
      implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)

      logger.debug(s"Normalized identifier to ${qualifiedIds} using ${nodeNormURL}")

      qualifiedIds map { qualifiedId => iriKeyDecoder(qualifiedId.iri).get }
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
    relations <- getRelations(qualifiedIds.map(_.iri), hopLimit - 1, Set(queryId))
    biolinkPredicates: Map[String, Set[LabeledIRI]] = relations.flatMap(_.biolinkPredicates).foldLeft(Map[String, Set[LabeledIRI]]()) {
      case (map, entry) => map.updated(entry._1, map.getOrElse(entry._1, Set()) ++ entry._2)
    }
  } yield Result(
    queryId,
    qualifiedIds,
    biolinkPredicates,
    relations.toSeq,
    subjectTriples.map(fromQuerySolution),
    objectTriples.map(fromQuerySolution)
  )

  /* HTTP4S INTERFACE */
  val lookupEndpointZ: URIO[Has[BiolinkData], LookupEndpoint] =
    for {
      biolinkData <- biolinkData
    } yield {
      implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
      implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)

      implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
      implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)

      implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder
      implicit val biolinkClassDecoder: Decoder[BiolinkClass] = Implicits.biolinkClassDecoder(biolinkData.classes)

      implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Implicits.biolinkPredicateEncoder(biolinkData.prefixes)
      implicit val biolinkPredicateDecoder: Decoder[List[BiolinkPredicate]] =
        Implicits.predicateOrPredicateListDecoder(biolinkData.predicates)

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
            .description("The URL where the Translator NodeNormalizer can be found")
            .default(Some("https://nodenorm.transltr.io/1.3/get_normalized_nodes"))
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

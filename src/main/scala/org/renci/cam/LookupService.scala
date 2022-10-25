package org.renci.cam

import com.typesafe.scalalogging.LazyLogging
import sttp.tapir.generic.auto._
import org.phenoscape.sparql.SPARQLInterpolation._
import io.circe.generic.semiauto._
import io.circe.generic.auto._
import io.circe.{Decoder, Encoder, Json, KeyDecoder, KeyEncoder}
import org.apache.jena.query.QuerySolution
import org.apache.jena.rdf.model.{RDFNode, Resource}
import org.http4s.{HttpRoutes, InvalidBodyException}
import org.renci.cam.Biolink.{biolinkData, BiolinkData}
import org.renci.cam.SPARQLQueryExecutor.SPARQLCache
import org.renci.cam.Server.EndpointEnv
import org.renci.cam.Server.LocalTapirJsonCirce.jsonBody
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, IRI, LogEntry, TRAPIMessage, TRAPINode, TRAPIQuery, TRAPIQueryEdge, TRAPIQueryGraph, TRAPIQueryNode, TRAPIResponse}
import sttp.tapir.Endpoint
import sttp.tapir.server.http4s.ztapir.ZHttp4sServerInterpreter
import sttp.tapir.ztapir.{endpoint, query, stringBody}
import zio.{Has, RIO, URIO, ZIO}
import org.http4s.implicits._

/** The LookupService can be used to look up concepts within CAM-KP-API without needing to use either high-level TRAPI queries or low-level
  * SPARQL queries. It is intended to provide a middle path to interrogate the database and to identify cases where the TRAPI interface
  * (QueryService) doesn't provide the correct results.
  */
object LookupService extends LazyLogging {

  /* DATA STRUCTURES */
  case class ResultTriplesIRIs(
    s: String,
    p: String,
    o: String,
    g: String
  )

  case class Result(
    predicates: Set[BiolinkPredicate],
    subjectTriples: Seq[ResultTriplesIRIs],
    objectTriples: Seq[ResultTriplesIRIs]
  )

  case class Error(
    code: String,
    message: String
  )

  /* CONTROLLER */
  type LookupEndpointParams = (String, Option[String], Option[String])
  type LookupEndpoint = Endpoint[LookupEndpointParams, Error, Result, Any]

  def convertSPOToResultTriplesIRIs(result: QuerySolution): ResultTriplesIRIs =
    // Note that either ?s or ?o may be bnodes!
    ResultTriplesIRIs(
      result.get("s").toString,
      result.get("p").toString,
      result.get("o").toString,
      result.get("g").toString
    )

  def lookup(subject: String, predicateOpt: Option[String], objOpt: Option[String]): ZIO[EndpointEnv, Throwable, Result] = for {
    biolinkData <- biolinkData

    subjectIRI = {
      implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)

      // TODO: throw an error here.
      iriKeyDecoder(subject).getOrElse(IRI("http://identifiers.org/ncbigene/478"))
    }

    subjectTriplesQueryString = sparql"""SELECT DISTINCT ($subjectIRI AS ?s) ?p ?o ?g { GRAPH ?g { $subjectIRI ?p ?o }}"""
    subjectTriples <- SPARQLQueryExecutor.runSelectQuery(subjectTriplesQueryString.toQuery)
    _ = logger.debug(s"SPARQL query for subjectTriples: ${subjectTriplesQueryString.toQuery}")
    _ = logger.debug(s"Results for subjectTriples: ${subjectTriples}")

    objectTriplesQueryString = sparql"""SELECT DISTINCT ?s ?p ($subjectIRI AS ?o) ?g { GRAPH ?g { ?s ?p $subjectIRI }}"""
    objectTriples <- SPARQLQueryExecutor.runSelectQuery(objectTriplesQueryString.toQuery)
    _ = logger.debug(s"SPARQL query for objectTriples: ${objectTriplesQueryString.toQuery}")
    _ = logger.debug(s"Results for objectTriples: ${objectTriples}")
  } yield Result(
    Set(),
    subjectTriples.map(convertSPOToResultTriplesIRIs),
    objectTriples.map(convertSPOToResultTriplesIRIs)
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
          query[Option[String]]("predicate")
            .default(None)
            .example(None)
        )
        .in(
          query[Option[String]]("object")
            .default(None)
            .example(None)
        )
        .out(jsonBody[Result])
        .errorOut(jsonBody[Error])
        .summary("Look up relationships within CAM-KP-API by identifier.")
    }

  def lookupRouteR(lookupEndpoint: LookupEndpoint): HttpRoutes[RIO[EndpointEnv, *]] =
    ZHttp4sServerInterpreter[EndpointEnv]()
      .from(lookupEndpoint) { case (s, p, o) => lookup(s, p, o).mapError(ex => Error("interp_error", ex.getMessage)) }
      .toRoutes

}

package org.renci.cam

import sttp.tapir.generic.auto._
import io.circe.generic.semiauto._
import io.circe.generic.auto._
import io.circe.{Decoder, Encoder, Json, KeyDecoder, KeyEncoder}
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
object LookupService {

  /* DATA STRUCTURES */
  case class ResultTriples(
    s: Option[TRAPINode],
    p: Option[BiolinkPredicate],
    o: Option[TRAPINode]
  )

  case class Result(
    predicates: Set[BiolinkPredicate],
    subjectTriples: Seq[ResultTriples],
    objectTriples: Seq[ResultTriples]
  )

  case class Error(
    code: String,
    message: String
  )

  /* CONTROLLER */
  type LookupEndpointParams = (Option[String], Option[String], Option[String])
  type LookupEndpoint = Endpoint[LookupEndpointParams, Error, Result, Any]

  def lookup(subject: Option[String], predicate: Option[String], obj: Option[String]): ZIO[EndpointEnv, Error, Result] =
    ZIO.succeed(
      Result(
        Set(),
        Seq(),
        Seq()
      ))

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
          query[Option[String]]("subject")
            .default(None)
            .example(Some("biolink:BiologicalProcessOrActivity"))
        )
        .in(
          query[Option[String]]("predicate")
            .default(None)
            .example(Some("biolink:positively_regulates"))
        )
        .in(
          query[Option[String]]("object")
            .default(None)
            .example(Some("GO:0004707"))
        )
        .out(jsonBody[Result])
        .errorOut(jsonBody[Error])
        .summary("Look up relationships within CAM-KP-API by identifier.")
    }

  def lookupRouteR(lookupEndpoint: LookupEndpoint): HttpRoutes[RIO[EndpointEnv, *]] =
    ZHttp4sServerInterpreter[EndpointEnv]()
      .from(lookupEndpoint) { case (s, p, o) => lookup(s, p, o) }
      .toRoutes

}

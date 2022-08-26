package org.renci.cam

import sttp.tapir.generic.auto._
import io.circe.generic.semiauto._
import io.circe.generic.auto._
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import org.http4s.{HttpRoutes, InvalidBodyException}
import org.renci.cam.Biolink.{BiolinkData, biolinkData}
import org.renci.cam.SPARQLQueryExecutor.SPARQLCache
import org.renci.cam.Server.EndpointEnv
import org.renci.cam.Server.LocalTapirJsonCirce.jsonBody
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, IRI, LogEntry, TRAPIMessage, TRAPINode, TRAPIQuery, TRAPIQueryEdge, TRAPIQueryGraph, TRAPIQueryNode, TRAPIResponse}
import sttp.tapir.Endpoint
import sttp.tapir.server.http4s.ztapir.ZHttp4sServerInterpreter
import sttp.tapir.ztapir.{endpoint, query, stringBody}
import zio.{Has, RIO, URIO, ZIO}

/**
 * The LookupService can be used to look up concepts within CAM-KP-API without needing to use either high-level TRAPI
 * queries or low-level SPARQL queries. It is intended to provide a middle path to interrogate the database and to
 * identify cases where the TRAPI interface (QueryService) doesn't provide the correct results.
 */
object LookupService {
  /* DATA STRUCTURES */
  case class Result (
    s: Option[TRAPINode],
    p: Option[BiolinkPredicate],
    o: Option[TRAPINode]
                    )


  /* HTTP4S INTERFACE */
  type LookupEndpoint = Endpoint[(Option[Int], Option[String], Option[String], Option[String]), List[LogEntry], List[Result], Any]
  val lookupEndpointZ: URIO[Has[BiolinkData], LookupEndpoint] = {

    // This example asks what biological process or activities positively regulate GO:0004707
    // (MAP kinase activity, see http://purl.obolibrary.org/obo/GO_0004707)
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

      endpoint.get
        .in("lookup")
        .in(
          query[Option[Int]]("limit")
            .default(Some(100))
            .example(Some(100))
        )
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
        .out(jsonBody[List[Result]])
        .errorOut(jsonBody[List[LogEntry]])
        .summary("Look up relationships within CAM-KP-API by identifier.")
    }
  }


  def lookupRouteR(queryEndpoint: Endpoint[(Int, String), String, TRAPIResponse, Any]): HttpRoutes[RIO[EndpointEnv, *]] =
    ZHttp4sServerInterpreter[EndpointEnv]()
      .from(queryEndpoint) { case (limit, body) =>
        val program: ZIO[EndpointEnv, Throwable, TRAPIResponse] = for {
          queryGraph <-
            ZIO
              .fromOption(body.message.query_graph)
              .orElseFail(new InvalidBodyException("A query graph is required, but hasn't been provided."))
          limitValue <- ZIO.fromOption(limit).orElse(ZIO.effect(1000))
          message <- QueryService.run(limitValue, queryGraph)
        } yield TRAPIResponse(message, Some("Success"), None, None)
        program.mapError(error => error.getMessage)
      }
      .toRoutes
}

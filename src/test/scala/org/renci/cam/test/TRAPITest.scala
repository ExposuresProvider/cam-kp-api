package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import org.http4s.headers.{`Content-Type`, Accept}
import org.http4s.{EntityDecoder, MediaType, Method, Request, Uri}
import org.renci.cam.Biolink.biolinkData
import org.renci.cam._
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, IRI, TRAPIAttribute, TRAPIResponse}
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.interop.catz.concurrentInstance
import zio.test._
import zio.{Layer, ZIO}

object TRAPITest extends DefaultRunnableSpec with LazyLogging {

  /** This query contains two unexpected properties:
    *   - unknown_qnode_property on the "n0" QNode
    *   - unknown_qedge_property on the "e0" QEdge As per https://github.com/NCATSTranslator/ReasonerAPI/pull/322, a server receiving an
    *     unknown property SHOULD generate a warning and MAY continue processing.
    */
  val testUnexpectedPropertiesOnQNodeAndQEdge = suite("testUnexpectedPropertiesOnQNodeAndQEdge") {
    //
    val query = """{
                     "message": {
                       "query_graph": {
                         "nodes": {
                           "n0": {
                             "categories": ["biolink:GeneOrGeneProduct"],
                             "unknown_qnode_property": "A1"
                           },
                           "n1": { "categories": ["biolink:AnatomicalEntity"], "ids": ["GO:0005634"] }
                         },
                         "edges": {
                           "e0": {
                             "subject": "n0",
                             "object": "n1",
                             "predicates": ["biolink:part_of"],
                             "unknown_qedge_property": "B2"
                           }
                         }
                       }
                     }
                   }"""

    testM("test unexpected properties on QNode and QEdge") {
      for {
        biolinkData <- biolinkData

        server <- Server.httpApp
        response <- server(
          Request(
            method = Method.POST,
            uri = Uri.unsafeFromString("/query")
          )
            .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
            .withEntity(query)
        )
        content <- EntityDecoder.decodeText(response)
        trapiResponseJson <- ZIO.fromEither(io.circe.parser.parse(content))

        trapiResponse <- ZIO.fromEither(
          {
            implicit val decoderIRI: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
            implicit val keyDecoderIRI: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
            implicit val decoderBiolinkClass: Decoder[BiolinkClass] = Implicits.biolinkClassDecoder(biolinkData.classes)
            implicit val decoderBiolinkPredicate: Decoder[BiolinkPredicate] =
              Implicits.biolinkPredicateDecoder(biolinkData.predicates)
            implicit lazy val decoderTRAPIAttribute: Decoder[TRAPIAttribute] = deriveDecoder[TRAPIAttribute]

            trapiResponseJson.as[TRAPIResponse]
          }
        )

        logs = trapiResponse.logs.getOrElse(List())
        logCodes = logs.map(_.code.getOrElse("")).toSet
        logMessages = logs.map(_.message.getOrElse(""))
      } yield
      // Expect a 400 Bad Request
      assert(response.status.code)(Assertion.equalTo(400)) &&
        assert(response.status.reason)(Assertion.equalTo("Bad Request")) &&
        // Nevertheless, the content should be some content with logs.
        assert(content)(Assertion.isNonEmptyString) &&
        // Make sure we have two ERRORs for both validation errors.
        assert(logCodes)(Assertion.equalTo(Set("ERROR"))) &&
        assert(logMessages)(Assertion.hasSize(Assertion.equalTo(2))) &&
        assert(logMessages.head)(Assertion.startsWithString("Hello"))
    }
  }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val testLayer = HttpClient.makeHttpClientLayer ++ Biolink.makeUtilitiesLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer

  def spec = suite("TRAPI tests")(
    testUnexpectedPropertiesOnQNodeAndQEdge
  ).provideCustomLayer(testLayer.mapError(TestFailure.die))

}

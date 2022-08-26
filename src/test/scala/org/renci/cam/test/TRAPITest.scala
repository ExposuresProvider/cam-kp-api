package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import org.http4s.headers.{`Content-Type`, Accept}
import org.http4s.{EntityDecoder, MediaType, Method, Request, Uri}
import org.renci.cam.Biolink.biolinkData
import org.renci.cam._
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, IRI, LogEntry, TRAPIAttribute, TRAPIResponse}
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.interop.catz.concurrentInstance
import zio.test._
import zio.{Layer, ZIO}

object TRAPITest extends DefaultRunnableSpec with LazyLogging {

  /** If CAM-KP-API receives a attribute constraint that it does not support, it MUST respond with an error Code
    * "UnsupportedAttributeConstraint".
    */
  val testUnsupportedAttributeConstraints = suite("testUnsupportedAttributeConstraints") {
    // Examples taken from https://github.com/NCATSTranslator/ReasonerAPI/blob/1e0795a1c4ff5bcac3ccd5f188fdc09ec6bd27c3/ImplementationRules.md#specifying-permitted-and-excluded-kps-to-an-ara
    val query = """{
                     "message": {
                       "query_graph": {
                         "nodes": {
                           "n0": {
                             "categories": ["biolink:GeneOrGeneProduct"]
                           },
                           "n1": { "categories": ["biolink:AnatomicalEntity"], "ids": ["GO:0005634"] }
                         },
                         "edges": {
                           "e0": {
                             "subject": "n0",
                             "object": "n1",
                             "predicates": ["biolink:part_of"],
                             "attribute_constraints": [{
                               "id": "biolink:knowledge_source",
                               "name": "knowledge source",
                               "value": "infores:semmeddb",
                               "not": true,
                               "operator": "=="
                             }]
                           }
                         }
                       }
                     }
                   }"""

    testM("test unsupported attribute constraint") {
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

        logs = trapiResponse.logs
        logsWithUnsupportedAttributeConstraint = logs.getOrElse(List()).filter {
          case LogEntry(_, Some("Error"), Some("UnsupportedAttributeConstraint"), _) => true
          case _                                                                     => false
        }
      } yield assert(response.status)(Assertion.hasField("isSuccess", _.isSuccess, Assertion.isTrue)) &&
        assert(content)(Assertion.isNonEmptyString) &&
        // Should return an UnsupportedAttributeConstraint as the status ...
        assert(trapiResponse.status)(Assertion.isSome(Assertion.equalTo("UnsupportedAttributeConstraint"))) &&
        // ... and in the logs.
        assert(logs)(Assertion.isSome(Assertion.isNonEmpty)) &&
        assert(logsWithUnsupportedAttributeConstraint)(Assertion.isNonEmpty)
    }
  }

  /** If CAM-KP-API receives a qualifier constraint that it does not support, it MUST return an empty response (since no edges meet the
    * constraint).
    *
    * (This is still in development at https://github.com/NCATSTranslator/ReasonerAPI/pull/364)
    */
  val testUnsupportedQualifierConstraints = suite("testUnsupportedConstraints") {
    // Examples taken from https://github.com/NCATSTranslator/ReasonerAPI/blob/7520ac564e63289dffe092d4c7affd6db4ba22f1/examples/Message/subject_and_object_qualifiers.json
    val query = """{
                     "message": {
                       "query_graph": {
                         "nodes": {
                           "n0": {
                             "categories": ["biolink:GeneOrGeneProduct"]
                           },
                           "n1": { "categories": ["biolink:AnatomicalEntity"], "ids": ["GO:0005634"] }
                         },
                         "edges": {
                           "e0": {
                             "subject": "n0",
                             "object": "n1",
                             "predicates": ["biolink:part_of"],
                             "qualifier_constraints": [{
                               "qualifier_set": [{
                                 "qualifier_type_id": "biolink:subject_aspect_qualifier",
                                 "qualifier_value": "abundance"
                               }, {
                                 "qualifier_type_id": "biolink:subject_direction_qualifier",
                                 "qualifier_value": "decreased"
                               }]
                             }]
                           }
                         }
                       }
                     }
                   }"""

    testM("test unsupported qualifier constraint on QEdge") {
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

        logs = trapiResponse.logs
        logWarningOfQualifierConstraints = logs.getOrElse(List()).filter {
          // We've made this up ourselves.
          case LogEntry(_, Some("Warning"), Some("UnsupportedQualifierConstraint"), _) => true
          case _                                                                       => false
        }
      } yield assert(response.status)(Assertion.hasField("isSuccess", _.isSuccess, Assertion.isTrue)) &&
        assert(content)(Assertion.isNonEmptyString) &&
        // Should return an overall status of Success
        assert(trapiResponse.status)(Assertion.isSome(Assertion.equalTo("Success"))) &&
        // ... and in the logs
        assert(logs)(Assertion.isSome(Assertion.isNonEmpty)) &&
        assert(logWarningOfQualifierConstraints)(Assertion.isNonEmpty) &&
        // ... and with no results.
        assert(trapiResponse.message.results)(Assertion.isSome(Assertion.isEmpty))
    }
  }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val testLayer = HttpClient.makeHttpClientLayer ++ Biolink.makeUtilitiesLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer

  def spec = suite("TRAPI tests")(
    testUnsupportedAttributeConstraints,
    testUnsupportedQualifierConstraints
  ).provideCustomLayer(testLayer.mapError(TestFailure.die))

}

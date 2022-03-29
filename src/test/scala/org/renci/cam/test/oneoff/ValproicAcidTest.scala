package org.renci.cam.test.oneoff

import com.typesafe.scalalogging.LazyLogging

import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.apache.jena.query.{Query, QuerySolution}
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.SPARQLQueryExecutor.SPARQLCache
import org.renci.cam.{AppConfig, Biolink, HttpClient, QueryService, SPARQLQueryExecutor}
import org.renci.cam.domain._

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.http4s._
import org.http4s.headers._
import org.http4s.implicits._
import org.renci.cam.Biolink.BiolinkData
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam._
import org.renci.cam.domain._
import zio.blocking.Blocking
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.interop.catz._
import zio.test._
import zio.test.environment.testEnvironment
import zio.{Has, Layer, RIO, Task, ZIO}

import java.nio.file.{Files, Paths}

/**
 * Tests relating to Question of the Month #1: ALAS1 and Valproic Acid
 *
 * See https://github.com/NCATSTranslator/testing/issues/176 for the question and background
 * information.
 */
object ValproicAcidTest extends DefaultRunnableSpec with LazyLogging {
  /**
   * Find out everything we know about Valproic Acid (http://purl.obolibrary.org/obo/CHEBI_39867).
   */
  val testEverythingOnValproicAcid = {
    val testQueryGraph = TRAPIQueryGraph(
      Map(
        "n0" -> TRAPIQueryNode(Some(List(IRI("http://purl.obolibrary.org/obo/CHEBI_39867"))), None, None),
        "n1" -> TRAPIQueryNode(None, Some(List(BiolinkClass("NamedThing"))), None)
      ),
      Map(
        "e0" -> TRAPIQueryEdge(Some(List(BiolinkPredicate("related_to"))), "n0", "n1", None)
      )
    )

    suite("testEverythingOnValproicAcid")(
      testM(s"Find everything related to valproic acid") {
        for {
          message <- QueryService.run(100, false, testQueryGraph)

          biolinkData <- Biolink.biolinkData
          encoded = {
            implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
            implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
            implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder
            implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Implicits.biolinkPredicateEncoder(biolinkData.prefixes)
            message.asJson.deepDropNullValues.spaces2
          }

          _ = Files.writeString(Paths.get("src/test/resources/test-valproic-acid.json"), encoded)
          results = message.results.get
        } yield {
          assert(results.length)(Assertion.isGreaterThan(0)) // Expect more than one result
        }
      }
    )
  }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val testLayer = HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer

  def spec = suite("Limit tests")(
    testEverythingOnValproicAcid
  ).provideCustomLayer(testLayer.mapError(TestFailure.die))
}

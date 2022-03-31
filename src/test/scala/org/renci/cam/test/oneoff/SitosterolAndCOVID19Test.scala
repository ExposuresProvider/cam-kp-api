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
 * Tests relating to Question of the Month #2: β-sitosterol and COVID-19
 *
 * See https://github.com/NCATSTranslator/testing/issues/185 for the question and background
 * information.
 */
object SitosterolAndCOVID19Test extends DefaultRunnableSpec with LazyLogging {
  /**
   * Find out everything we know about beta-Sitosterol (http://purl.obolibrary.org/obo/CHEBI_39867).
   */
  val testEverythingOnBetaSitosterol = {
    val testQueryGraph = TRAPIQueryGraph(
      Map(
        "n0" -> TRAPIQueryNode(Some(List(IRI("http://purl.obolibrary.org/obo/CHEBI_27693"))), None, None),
        "n1" -> TRAPIQueryNode(None, Some(List(BiolinkClass("NamedThing"))), None)
      ),
      Map(
        "e0" -> TRAPIQueryEdge(Some(List(BiolinkPredicate("related_to"))), "n0", "n1", None)
      )
    )
    val expectedResultCount = 43536 // As of 2022mar29

    suite("testEverythingOnBetaSitosterol")(
      testM(s"Find everything related to β-sitosterol") {
        for {
          message <- QueryService.run(5000, false, testQueryGraph)

          biolinkData <- Biolink.biolinkData
          encoded = {
            implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
            implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
            implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder
            implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Implicits.biolinkPredicateEncoder(biolinkData.prefixes)
            message.asJson.deepDropNullValues.spaces2
          }

          _ = Files.writeString(Paths.get("src/test/resources/test-beta-sitosterol-acid.json"), encoded)
          results = message.results.get
        } yield {
          assert(results.length)(Assertion.isGreaterThanEqualTo(7)) // This is a little less than 5000, probably because of
                                                                    // https://github.com/NCATS-Tangerine/cam-kp-api/pull/499
        }
      }
    )
  }

  /**
   * Find out everything we know about COVID-19.
   */
  val testEverythingOnCOVID19 = {
    val testQueryGraph = TRAPIQueryGraph(
      Map(
        "n0" -> TRAPIQueryNode(Some(List(IRI("http://purl.obolibrary.org/obo/MONDO_0100096"))), None, None),
        "n1" -> TRAPIQueryNode(None, Some(List(BiolinkClass("NamedThing"))), None)
      ),
      Map(
        "e0" -> TRAPIQueryEdge(Some(List(BiolinkPredicate("related_to"))), "n0", "n1", None)
      )
    )
    val expectedResultCount = 43536 // As of 2022mar29

    suite("testEverythingOnCOVID19")(
      testM(s"Find everything related to COVID19") {
        for {
          message <- QueryService.run(5000, false, testQueryGraph)

          biolinkData <- Biolink.biolinkData
          encoded = {
            implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
            implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
            implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder
            implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Implicits.biolinkPredicateEncoder(biolinkData.prefixes)
            message.asJson.deepDropNullValues.spaces2
          }

          _ = Files.writeString(Paths.get("src/test/resources/test-covid-19.json"), encoded)
          results = message.results.get
        } yield {
          assert(results.length)(Assertion.isGreaterThanEqualTo(7)) // This is a little less than 5000, probably because of
          // https://github.com/NCATS-Tangerine/cam-kp-api/pull/499
        }
      }
    )
  }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val testLayer = HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer

  def spec = suite("β-sitosterol and COVID-19 one-off tests")(
    // testEverythingOnBetaSitosterol,
    testEverythingOnCOVID19
  ).provideCustomLayer(testLayer.mapError(TestFailure.die))
}

package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import io.circe.syntax.EncoderOps
import org.apache.jena.query.{Query, QuerySolution}
import org.http4s.Method.GET
import org.http4s.client.dsl.io._
import org.http4s.{EntityDecoder, Request, Uri}
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam._
import org.renci.cam.domain._
import zio.blocking.effectBlockingIO
import zio.cache.Cache
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.interop.catz.concurrentInstance
import zio.test.{assert, _}
import zio.{Layer, ZIO, ZLayer}

import java.io.{File, FileWriter, PrintWriter}

/** Test the `/lookup` endpoint (implemented by LookupService)
  */
object LookupServiceTest extends DefaultRunnableSpec with LazyLogging {

  /** We should be able to get some default results just by hitting the `/lookup` endpoint. */
  val testDefault =
    suite("testDefault") {
      testM("Check the default lookup") {
        for {
          // Retrieve /docs/docs.yaml from the server.
          server <- Server.httpApp
          response <- server(Request(uri = Uri.unsafeFromString("/lookup")))
          content <- EntityDecoder.decodeText(response)
          resultOrErrorJson <- ZIO.fromEither(io.circe.parser.parse(content))

          result <- ZIO.fromEither(resultOrErrorJson.as[LookupService.Result])
          _ = logger.debug(s"Results: ${result.asJson.deepDropNullValues.spaces2SortKeys}")
        } yield assert(content)(Assertion.isNonEmptyString) &&
          assert(result.subjectTriples)(Assertion.isNonEmpty) &&
          assert(result.objectTriples)(Assertion.isNonEmpty) &&
          assert(result.relations)(Assertion.isNonEmpty) &&
          assert(result.biolinkPredicates)(Assertion.isEmpty)
      }
    }

  def testIdentifier(id: String) = suite(s"Test identifier ${id}") {
    testM("Test whether we can retrieve results for this identifier") {
      val idFilenameSafe = id.replaceAll("\\W", "_")
      val outputFile = new File(s"./src/test/resources/lookup-service-results/${idFilenameSafe}.json")

      for {

        server <- Server.httpApp
        response <- server(Request(GET, Uri.unsafeFromString("/lookup").withQueryParam("subject", id)))
        content <- EntityDecoder.decodeText(response)
        resultOrErrorJson <- ZIO.fromEither(io.circe.parser.parse(content))
        result <- ZIO.fromEither(resultOrErrorJson.as[LookupService.Result])
        _ = logger.debug(s"Results for identifier ${id}: ${result.asJson.deepDropNullValues.spaces2SortKeys}")
        _ <- effectBlockingIO(new PrintWriter(new FileWriter(outputFile)))
          .bracketAuto { pw =>
            pw.println(result.asJson.deepDropNullValues.spaces2SortKeys)
            ZIO.unit
          }
      } yield assert(content)(Assertion.isNonEmptyString) &&
        assert(result.subjectTriples)(Assertion.isNonEmpty) &&
        assert(result.objectTriples)(Assertion.isNonEmpty) &&
        assert(result.relations)(Assertion.isNonEmpty)
    }
  }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)

  val testLayer: ZLayer[
    Any,
    Throwable,
    HttpClient with ZConfig[Biolink.BiolinkData] with ZConfig[AppConfig] with ZConfig[Cache[Query, Throwable, List[QuerySolution]]]] =
    HttpClient.makeHttpClientLayer ++ Biolink.makeUtilitiesLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer

  def spec: Spec[environment.TestEnvironment, TestFailure[Throwable], TestSuccess] = suite("Limit tests")(
    testDefault,
    testIdentifier("CHEBI:17685")
  ).provideCustomLayer(testLayer.mapError(TestFailure.die))

}

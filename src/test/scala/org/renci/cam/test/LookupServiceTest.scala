package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import io.circe.syntax.EncoderOps
import org.apache.jena.query.{Query, QuerySolution}
import org.http4s.{EntityDecoder, Request, Uri}
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam._
import org.renci.cam.domain._
import zio.cache.Cache
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.interop.catz.concurrentInstance
import zio.test.{assert, _}
import zio.{Layer, ZIO, ZLayer}

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
          _ = logger.info(s"Results: ${result.asJson.deepDropNullValues.spaces2SortKeys}")
        } yield assert(content)(Assertion.isNonEmptyString) &&
          assert(result.subjectTriples)(Assertion.isNonEmpty) &&
          assert(result.objectTriples)(Assertion.isNonEmpty) &&
          assert(result.snaks)(Assertion.isNonEmpty) &&
          assert(result.biolinkPredicates)(Assertion.isEmpty)
      }
    }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)

  val testLayer: ZLayer[
    Any,
    Throwable,
    HttpClient with ZConfig[Biolink.BiolinkData] with ZConfig[AppConfig] with ZConfig[Cache[Query, Throwable, List[QuerySolution]]]] =
    HttpClient.makeHttpClientLayer ++ Biolink.makeUtilitiesLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer

  def spec: Spec[environment.TestEnvironment, TestFailure[Throwable], TestSuccess] = suite("Limit tests")(
    testDefault
  ).provideCustomLayer(testLayer.mapError(TestFailure.die))

}

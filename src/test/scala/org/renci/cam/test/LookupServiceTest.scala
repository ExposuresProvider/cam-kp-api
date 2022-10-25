package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import org.apache.jena.query.{Query, QuerySolution}
import org.http4s.{EntityDecoder, Request, Uri}
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam._
import org.renci.cam.domain._
import zio.cache.Cache
import zio.config.typesafe.TypesafeConfig
import zio.config.{getConfig, ZConfig}
import zio.interop.catz.concurrentInstance
import zio.test._
import zio.{Layer, ZLayer}

/** Test the `/lookup` endpoint (implemented by LookupService)
  */
object LookupServiceTest extends DefaultRunnableSpec with LazyLogging {

  /** We should be able to get some default results just by hitting the `/lookup` endpoint. */
  val testDefault =
    suite("testDefault") {
      testM("Check the default lookup") {
        for {
          appConfig <- getConfig[AppConfig]

          // Retrieve /docs/docs.yaml from the server.
          server <- Server.httpApp
          response <- server(Request(uri = Uri.unsafeFromString("/lookup")))
          content <- EntityDecoder.decodeText(response)
        } yield assert(content)(Assertion.isNonEmptyString)
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

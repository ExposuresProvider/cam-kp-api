package org.renci.cam.it

import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.http4s._
import org.http4s.headers._
import org.renci.cam.Biolink.BiolinkData
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam._
import org.renci.cam.domain._
import org.renci.cam.it.ProdQueryServiceTest.{suite, testSimpleQuery}
import zio.blocking.Blocking
import zio.config.typesafe.TypesafeConfig
import zio.config.{getConfig, ZConfig}
import zio.interop.catz._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.testEnvironment
import zio.{Has, Layer, RIO, Task, ZIO}

object ServerTest extends DefaultRunnableSpec {

  def runQuery(trapiQuery: TRAPIQuery, limit: Int = 1, include_extra_edges: Boolean = false)
    : RIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], (Map[IRI, TRAPINode], Map[String, TRAPIEdge])] =
    for {
      appConfig <- getConfig[AppConfig]
      httpClient <- HttpClient.client
      biolinkData <- Biolink.biolinkData
      encoded = {
        implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
        implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
        implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder
        implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Implicits.biolinkPredicateEncoder(biolinkData.prefixes)
        trapiQuery.asJson.deepDropNullValues.noSpaces
      }
      _ = println("encoded: " + encoded)
      // TODO: this should probably be in the AppConfig somewhere.
      uri = Uri.fromString(s"https://${appConfig.location}/${appConfig.trapiVersion}/query").toOption.get
      _ = println("uri: " + uri)
      uriWithQueryParams = uri.withQueryParam("limit", limit).withQueryParam("include_extra_edges", include_extra_edges)
      request = Request[Task](Method.POST, uriWithQueryParams)
        .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
        .withEntity(encoded)
      response <- httpClient.expect[String](request)
    } yield response

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val camkpapiLayer = Blocking.live >>> HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer
  val testLayer = (configLayer ++ testEnvironment ++ camkpapiLayer).mapError(TestFailure.die)

  def spec = suite("QueryService tests")(
    testSimpleQuery
  ).provideLayerShared(testLayer) @@ TestAspect.sequential

}

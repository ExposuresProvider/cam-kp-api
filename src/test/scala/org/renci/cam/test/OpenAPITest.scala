package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import io.circe.yaml.parser
import org.http4s.{EntityDecoder, Request, Status, Uri}
import org.renci.cam._
import zio.config.typesafe.TypesafeConfig
import zio.config.{getConfig, ZConfig}
import zio.interop.catz.concurrentInstance
import zio.test._
import zio.{Layer, ZIO}

object OpenAPITest extends DefaultRunnableSpec with LazyLogging {

  val testOpenAPISpecification = suite("testOpenAPISpecification") {
    testM("Validate and check Open API specification") {
      for {
        appConfig <- getConfig[AppConfig]

        // Retrieve /docs/docs.yaml from the server.
        server <- Server.httpApp
        response <- server(Request(uri = Uri.unsafeFromString("/docs/docs.yaml")))
        content <- EntityDecoder.decodeText(response)
        openApiDoc <- ZIO.fromEither(parser.parse(content))

        // Look up info.version.
        infoVersionOpt = openApiDoc.hcursor.downField("info").downField("version").as[String].toOption
      } yield assert(response.status)(Assertion.equalTo(Status.Ok)) &&
        assert(content)(Assertion.isNonEmptyString) &&
        // Check the info.version value.
        assert(infoVersionOpt)(Assertion.isSome(Assertion.isNonEmptyString)) &&
        assert(infoVersionOpt)(Assertion.equalTo(Some(appConfig.version)))
    }
  }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val testLayer = HttpClient.makeHttpClientLayer ++ Biolink.makeUtilitiesLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer

  def spec = suite("OpenAPI tests")(
    testOpenAPISpecification
  ).provideCustomLayer(testLayer.mapError(TestFailure.die))

}

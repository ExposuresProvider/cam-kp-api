package org.renci.cam.test

import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import io.circe.yaml.parser
import org.http4s.{EntityDecoder, Request, Status, Uri}
import org.renci.cam._
import org.renci.cam.domain._
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.interop.catz.concurrentInstance
import zio.test._
import zio.{Layer, ZIO}

object OpenAPITest extends DefaultRunnableSpec with LazyLogging {
  // implicit val F = Concurrent[IO]

  val testOpenAPISpecification = suite("testOpenAPISpecification") {
    testM("Validate and check Open API specification") {
      for {
        server <- Server.httpApp
        response <- server(Request(uri = Uri.unsafeFromString("/docs/docs.yaml")))
        content <- EntityDecoder.decodeText(response)
        openApiDoc <- ZIO.fromEither(parser.parse(content))
      } yield assert(response.status)(Assertion.equalTo(Status.Ok)) &&
        assert(content)(Assertion.isNonEmptyString)
    }
  }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val testLayer = HttpClient.makeHttpClientLayer ++ Biolink.makeUtilitiesLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer

  def spec = suite("OpenAPI tests")(
    testOpenAPISpecification
  ).provideCustomLayer(testLayer.mapError(TestFailure.die))

}

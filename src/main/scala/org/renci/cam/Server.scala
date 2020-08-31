package org.renci.cam

import cats.implicits._
import io.circe.Printer
import io.circe.generic.auto._
import org.http4s._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.{Logger, _}
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.domain._
import sttp.tapir.docs.openapi._
import sttp.tapir.json.circe._
import sttp.tapir.openapi.circe.yaml._
import sttp.tapir.server.http4s.ztapir._
import sttp.tapir.swagger.http4s.SwaggerHttp4s
import sttp.tapir.ztapir._
import zio.config.typesafe.TypesafeConfig
import zio.config.{ZConfig, _}
import zio.interop.catz._
import zio.interop.catz.implicits._
import zio.{config => _, _}

object Server extends App {

  object LocalTapirJsonCirce extends TapirJsonCirce {
    override def jsonPrinter: Printer = Printer.noSpaces.copy(dropNullValues = true)
  }

  import LocalTapirJsonCirce._

  val predicatesEndpoint: ZEndpoint[Unit, String, String] = endpoint.get.in("predicates").errorOut(stringBody).out(jsonBody[String])

  val predicatesRouteR: URIO[ZConfig[AppConfig], HttpRoutes[Task]] = predicatesEndpoint.toRoutesR {
    case () =>
      val program = for {
        response <- Task.effect("")
      } yield response
      program.mapError(error => error.getMessage)
  }

  val queryEndpoint: ZEndpoint[(Int, TRAPIQueryRequestBody), String, TRAPIMessage] =
    endpoint.post
      .in("query")
      .in(query[Int]("limit"))
      .in(jsonBody[TRAPIQueryRequestBody])
      .errorOut(stringBody)
      .out(jsonBody[TRAPIMessage])

  val queryRouteR: URIO[ZConfig[AppConfig] with HttpClient, HttpRoutes[Task]] = queryEndpoint.toRoutesR {
    case (limit, body) =>
      val program = for {
        queryGraph <-
          ZIO
            .fromOption(body.message.query_graph)
            .orElseFail(new InvalidBodyException("A query graph is required, but hasn't been provided."))
        resultSet <- QueryService.run(limit, queryGraph)
        message <- QueryService.parseResultSet(queryGraph, resultSet)
      } yield message
      program.mapError(error => error.getMessage)
  }

  // will be available at /docs
  val openAPI: String = List(queryEndpoint, predicatesEndpoint).toOpenAPI("CAM-KP API", "0.1").toYaml

  val server: RIO[ZConfig[AppConfig] with HttpClient, Unit] =
    ZIO.runtime[Any].flatMap { implicit runtime =>
      for {
        appConfig <- config[AppConfig]
        predicatesRoute <- predicatesRouteR
        queryRoute <- queryRouteR
        routes = queryRoute <+> predicatesRoute
        docsRoute = new SwaggerHttp4s(openAPI).routes[Task]
        httpApp = Router("/" -> (routes <+> docsRoute)).orNotFound
        httpAppWithLogging = Logger.httpApp(true, true)(httpApp)
        result <-
          BlazeServerBuilder[Task](runtime.platform.executor.asEC)
            .bindHttp(appConfig.port, appConfig.host)
            .withHttpApp(CORS(httpAppWithLogging))
            .serve
            .compile
            .drain
      } yield result
    }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for {
      httpClientLayer <- HttpClient.makeHttpClientLayer
      appLayer = httpClientLayer ++ configLayer
      out <- server.provideLayer(appLayer)
    } yield out).exitCode

}

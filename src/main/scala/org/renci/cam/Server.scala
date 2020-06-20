package org.renci.cam

import cats.implicits._
import io.circe.generic.auto._
import org.http4s._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.syntax.kleisli._
import sttp.tapir.docs.openapi._
import sttp.tapir.json.circe._
import sttp.tapir.openapi.circe.yaml._
import sttp.tapir.server.http4s.ztapir._
import sttp.tapir.swagger.http4s.SwaggerHttp4s
import sttp.tapir.ztapir._
import zio.config.{Config, _}
import zio.interop.catz._
import zio.interop.catz.implicits._
import zio.{config => _, _}

object Server extends App {

  val queryEndpoint: ZEndpoint[(Int, KGSQueryGraph), String, String] =
    endpoint.post
      .in("query")
      .in(query[Int]("limit"))
      .in(jsonBody[KGSQueryGraph])
      .errorOut(stringBody)
      .out(jsonBody[String])

  val routesR: URIO[Config[AppConfig], HttpRoutes[Task]] = queryEndpoint.toRoutesR {
    case (limit, queryGraph) =>
      //do stuff with queryGraph
      for {
        appConfig <- config[AppConfig]
      } yield queryGraph.toString
  }

  // will be available at /docs
  val openAPI: String = List(queryEndpoint).toOpenAPI("CAM-KP API", "0.1").toYaml

  val server: ZIO[Config[AppConfig], Throwable, Unit] =
    ZIO.runtime[Any].flatMap { implicit runtime =>
      for {
        routes <- routesR
        appConfig <- config[AppConfig]
        servr <-
          BlazeServerBuilder[Task](runtime.platform.executor.asEC)
            .bindHttp(appConfig.port, appConfig.host)
            .withHttpApp(Router("/" -> (routes <+> new SwaggerHttp4s(openAPI).routes[Task])).orNotFound)
            .serve
            .compile
            .drain
      } yield servr
    }

  // this is a temporary map-based config; it can be replaced with a property- or file-based config
  val configLayer =
    Config.fromMap(Map("host" -> "localhost", "port" -> "8080", "sparqlEndpoint" -> "http://example.org/sparql"),
                   AppConfig.config)

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = server.provideLayer(configLayer).exitCode

}

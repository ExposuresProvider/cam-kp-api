package org.renci.cam

import cats.implicits._
import io.circe.generic.auto._
import org.apache.jena.riot.ResultSetMgr
import org.apache.jena.riot.resultset.ResultSetLang
import org.http4s._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.{Logger, _}
import org.renci.cam.domain._
import sttp.tapir.docs.openapi._
import sttp.tapir.json.circe._
import sttp.tapir.openapi.circe.yaml._
import sttp.tapir.server.http4s.ztapir._
import sttp.tapir.swagger.http4s.SwaggerHttp4s
import sttp.tapir.ztapir._
import zio.config.typesafe.TypesafeConfig
import zio.config.{Config, _}
import zio.interop.catz._
import zio.interop.catz.implicits._
import zio.{config => _, _}

object Server extends App {

  val queryEndpoint: ZEndpoint[(Int, KGSQueryRequestBody), String, String] =
    endpoint.post
      .in("query")
      .in(query[Int]("limit"))
      .in(jsonBody[KGSQueryRequestBody])
      .errorOut(stringBody)
      .out(jsonBody[String])

  val routesR: URIO[Config[AppConfig], HttpRoutes[Task]] = queryEndpoint.toRoutesR {
    case (limit, body) =>
      //do stuff with queryGraph
      val queryGraph: KGSQueryGraph = body.message.query_graph
      val program = for {
        resultSet <- QueryService.run(limit, queryGraph)
        response <- Task.effect(ResultSetMgr.asString(resultSet, ResultSetLang.SPARQLResultSetJSON))
      } yield response
      program.mapError(error => error.getMessage)
  }

  // will be available at /docs
  val openAPI: String = List(queryEndpoint).toOpenAPI("CAM-KP API", "0.1").toYaml

  val server: RIO[Config[AppConfig], Unit] =
    ZIO.runtime[Any].flatMap { implicit runtime =>
      for {
        routes <- routesR
        appConfig <- config[AppConfig]
        httpApp = Router("/" -> (routes <+> new SwaggerHttp4s(openAPI).routes[Task])).orNotFound
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

  val configLayer: Layer[Throwable, Config[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = server.provideLayer(configLayer).exitCode

}

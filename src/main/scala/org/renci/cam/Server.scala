package org.renci.cam

import cats.implicits._
import io.circe.generic.auto._
import org.http4s._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import org.renci.cam.domain._
import sttp.tapir.docs.openapi._
import sttp.tapir.json.circe._
import sttp.tapir.openapi.circe.yaml._
import sttp.tapir.server.http4s.ztapir._
import sttp.tapir.swagger.http4s.SwaggerHttp4s
import sttp.tapir.ztapir._
import zio.interop.catz._
import zio.interop.catz.implicits._
import zio.{App, ExitCode, Runtime, Task, ZEnv, ZIO}

object Server extends App {

  implicit val runtime: Runtime[ZEnv] = Runtime.default

  val queryEndpoint: ZEndpoint[(Int, KGSQueryRequestBody), String, String] =
    endpoint.post
      .in("query")
      .in(query[Int]("limit"))
      .in(jsonBody[KGSQueryRequestBody])
      .errorOut(stringBody)
      .out(jsonBody[String])

  val queryRoute: HttpRoutes[Task] = queryEndpoint.toRoutes {
    case (limit, body) =>
      val queryGraph: KGSQueryGraph = body.message.query_graph
      val queryResponse = QueryService.run(limit, queryGraph)
      queryResponse.mapError(error => error.getMessage)
  }

  // will be available at /docs
  val openAPI: String = List(queryEndpoint).toOpenAPI("CAM-KP API", "0.1").toYaml

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = {
    val httpApp = Router("/" -> (queryRoute <+> new SwaggerHttp4s(openAPI).routes[Task])).orNotFound
    val finalHttpApp = Logger.httpApp(true, true)(httpApp)
    BlazeServerBuilder[Task](runtime.platform.executor.asEC)
      .bindHttp(8080, "localhost")
      .withHttpApp(finalHttpApp)
      .serve
      .compile
      .drain
      .exitCode
  }

}

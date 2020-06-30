package org.renci.cam

import cats.implicits._
import io.circe.generic.auto._
import org.http4s._
import org.http4s.server.Router
import org.http4s.server.middleware._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.syntax.kleisli._
import sttp.tapir.docs.openapi._
import sttp.tapir.json.circe._
import sttp.tapir.openapi.circe.yaml._
import sttp.tapir.server.http4s.ztapir._
import sttp.tapir.swagger.http4s.SwaggerHttp4s
import sttp.tapir.ztapir._
import zio.interop.catz._
import zio.interop.catz.implicits._
import zio.{App, ExitCode, Runtime, Task, UIO, ZEnv, ZIO}

object Server extends App {

  implicit val runtime: Runtime[ZEnv] = Runtime.default

  val queryEndpoint: ZEndpoint[(Int, KGSQueryGraph), String, String] =
    endpoint.post
      .in("query")
      .in(query[Int]("limit"))
      .in(jsonBody[KGSQueryGraph])
      .errorOut(stringBody)
      .out(jsonBody[String])

  val routes: HttpRoutes[Task] = queryEndpoint.toRoutes {
    case (limit, queryGraph) =>
      //do stuff with queryGraph
      ZIO.succeed(queryGraph.toString)
  }

  // will be available at /docs
  val openAPI: String = List(queryEndpoint).toOpenAPI("CAM-KP API", "0.1").toYaml

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    BlazeServerBuilder[Task](runtime.platform.executor.asEC)
      .bindHttp(8080, "localhost")
      .withHttpApp(CORS.httpApp(Router("/" -> (routes <+> new SwaggerHttp4s(openAPI).routes[Task])).orNotFound))
      .serve
      .compile
      .drain
      .as(ExitCode.success)
      .catchAllCause(cause => UIO(println(cause.prettyPrint)))
      .as(ExitCode.failure)

}

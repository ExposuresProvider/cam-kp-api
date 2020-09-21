package org.renci.cam

import java.util.Properties

import cats.effect.Blocker
import cats.implicits._
import io.circe.Printer
import io.circe.generic.auto._
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Location
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.{Logger, _}
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.Utilities._
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

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object Server extends App {

  object LocalTapirJsonCirce extends TapirJsonCirce {
    override def jsonPrinter: Printer = Printer.noSpaces.copy(dropNullValues = true)
  }

  import LocalTapirJsonCirce._

  val predicatesEndpoint: ZEndpoint[Unit, String, String] = endpoint.get.in("predicates").errorOut(stringBody).out(jsonBody[String])

  val predicatesRouteR: URIO[ZConfig[AppConfig], HttpRoutes[Task]] = predicatesEndpoint.toRoutesR { case () =>
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

  val queryRouteR: URIO[ZConfig[AppConfig] with HttpClient with Has[PrefixesMap], HttpRoutes[Task]] = queryEndpoint.toRoutesR {
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

  val server: RIO[ZConfig[AppConfig] with HttpClient with Has[PrefixesMap], Unit] =
    ZIO.runtime[Any].flatMap { implicit runtime =>
      for {
        appConfig <- config[AppConfig]
        predicatesRoute <- predicatesRouteR
        queryRoute <- queryRouteR
        routes = queryRoute <+> predicatesRoute
        openAPI: String = List(queryEndpoint, predicatesEndpoint)
          .toOpenAPI("CAM-KP API", "0.1")
          .servers(List(sttp.tapir.openapi.Server(appConfig.location)))
          .toYaml
        docsRoute = swaggerRoutes(openAPI)
        httpApp = Router("/" -> (routes <+> docsRoute)).orNotFound
        httpAppWithLogging = Logger.httpApp(true, false)(httpApp)
        result <-
          BlazeServerBuilder[Task](runtime.platform.executor.asEC)
            .bindHttp(appConfig.port, appConfig.host)
            .withHttpApp(CORS(httpAppWithLogging))
            .withResponseHeaderTimeout(120.seconds)
            .withIdleTimeout(180.seconds)
            .serve
            .compile
            .drain
      } yield result
    }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)

  val prefixesLayer: ZLayer[HttpClient, Throwable, Has[PrefixesMap]] = Utilities.makePrefixesLayer

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
    (for {
      httpClientLayer <- HttpClient.makeHttpClientLayer
      appLayer = httpClientLayer ++ (httpClientLayer >>> prefixesLayer) ++ configLayer
      out <- server.provideLayer(appLayer)
    } yield out).exitCode

  // hack using SwaggerHttp4s code to handle running in subdirectory
  private def swaggerRoutes(yaml: String): HttpRoutes[Task] = {
    val dsl = Http4sDsl[Task]
    import dsl._
    val contextPath = "docs"
    val yamlName = "docs.yaml"
    HttpRoutes.of[Task] {
      case path @ GET -> Root / `contextPath` =>
        val queryParameters = Map("url" -> Seq(s"$yamlName"))
        Uri
          .fromString(s"$contextPath/index.html")
          .map(uri => uri.setQueryParams(queryParameters))
          .map(uri => PermanentRedirect(Location(uri)))
          .getOrElse(NotFound())
      case GET -> Root / `contextPath` / `yamlName` =>
        Ok(yaml)
      case GET -> Root / `contextPath` / swaggerResource =>
        StaticFile
          .fromResource[Task](
            s"/META-INF/resources/webjars/swagger-ui/$swaggerVersion/$swaggerResource",
            Blocker.liftExecutionContext(ExecutionContext.global)
          )
          .getOrElseF(NotFound())
    }
  }

  private val swaggerVersion = {
    val p = new Properties()
    val pomProperties = getClass.getResourceAsStream("/META-INF/maven/org.webjars/swagger-ui/pom.properties")
    try p.load(pomProperties)
    finally pomProperties.close()
    p.getProperty("version")
  }

}

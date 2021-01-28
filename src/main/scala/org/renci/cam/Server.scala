package org.renci.cam

import java.util.Properties

import cats.effect.Blocker
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe.{Json, _}
import io.circe.yaml.syntax._
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Location
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.{Logger, _}
import org.renci.cam.Biolink._
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.domain._
import sttp.tapir.docs.openapi._
import sttp.tapir.json.circe._
import sttp.tapir.openapi.circe.yaml._
import sttp.tapir.openapi.{Contact, Info, License}
import sttp.tapir.server.http4s.ztapir._
import sttp.tapir.ztapir._
import zio._
import zio.blocking.{blocking, Blocking}
import zio.config.typesafe.TypesafeConfig
import zio.config.{getConfig, ZConfig}
import zio.interop.catz._
import zio.interop.catz.implicits._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object Server extends App with LazyLogging {

  object LocalTapirJsonCirce extends TapirJsonCirce {
    override def jsonPrinter: Printer = Printer.noSpaces.copy(dropNullValues = true)
  }

  import LocalTapirJsonCirce._

  val predicatesEndpointZ: URIO[Has[BiolinkData], ZEndpoint[Unit, String, Map[BiolinkClass, Map[BiolinkClass, List[BiolinkPredicate]]]]] =
    for {
      biolinkData <- biolinkData
    } yield endpoint.get
      .in("predicates")
      .errorOut(stringBody)
      .out(
        {
          implicit val blClassKeyDecoder: KeyDecoder[BiolinkClass] = (blClass: String) => Some(BiolinkClass(blClass))
          implicit val blPredicateEncoder: Encoder[BiolinkPredicate] =
            Encoder.encodeString.contramap(predicate => s"biolink:${predicate.shorthand}")
          implicit val blClassKeyEncoder: KeyEncoder[BiolinkClass] = (blClass: BiolinkClass) => s"biolink:${blClass.shorthand}"
          jsonBody[Map[BiolinkClass, Map[BiolinkClass, List[BiolinkPredicate]]]]
        }
      )
      .summary("Get predicates used at this service")

  def predicatesRouteR(predicatesEndpoint: ZEndpoint[Unit, String, Map[BiolinkClass, Map[BiolinkClass, List[BiolinkPredicate]]]])
    : URIO[ZConfig[AppConfig] with Blocking with HttpClient with Has[BiolinkData], HttpRoutes[Task]] =
    predicatesEndpoint.toRoutesR { case () =>
      val program = for {
        response <- PredicatesService.run
      } yield response
      program.mapError(error => error.getMessage)
    }

  val queryEndpointZ: URIO[Has[BiolinkData], ZEndpoint[(Option[Int], TRAPIQuery), String, TRAPIResponse]] =
    for {
      biolinkData <- biolinkData
    } yield endpoint.post
      .in("query")
      .in(query[Option[Int]]("limit"))
      .in(
        {
          implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
          implicit val biolinkClassDecoder: Decoder[BiolinkClass] = Implicits.biolinkClassDecoder(biolinkData.classes)
          implicit val biolinkPredicateDecoder: Decoder[BiolinkPredicate] = Implicits.biolinkPredicateDecoder(biolinkData.predicates)
          jsonBody[TRAPIQuery]
        }
      )
      .errorOut(stringBody)
      .out(
        {
          implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoderOut(biolinkData.prefixes)
//          implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder
          implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] =
            Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          jsonBody[TRAPIResponse]
        }
      )
      .summary("Submit a TRAPI question graph and retrieve matching solutions")

  def queryRouteR(queryEndpoint: ZEndpoint[(Option[Int], TRAPIQuery), String, TRAPIResponse])
    : URIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], HttpRoutes[Task]] =
    queryEndpoint.toRoutesR { case (limit, body) =>
      val program = for {
        queryGraph <-
          ZIO
            .fromOption(body.message.query_graph)
            .orElseFail(new InvalidBodyException("A query graph is required, but hasn't been provided."))
        results <- QueryService.run(limit, queryGraph)
        message <- QueryService.parseResultSet(queryGraph, results)
      } yield TRAPIResponse(message, Some("Success"), None, None)
      program.mapError(error => error.getMessage)
    }

  val openAPIInfo: Info = Info(
    "CAM-KP API",
    "0.1",
    Some("TRAPI interface to database of Causal Activity Models"),
    Some("https://opensource.org/licenses/MIT"),
    Some(Contact(Some("Jim Balhoff"), Some("balhoff@renci.org"), None)),
    Some(License("MIT License", Some("https://opensource.org/licenses/MIT")))
  )

  val server: RIO[ZConfig[AppConfig] with Blocking with HttpClient with Has[BiolinkData], Unit] =
    ZIO.runtime[Any].flatMap { implicit runtime =>
      for {
        appConfig <- getConfig[AppConfig]
        predicatesEndpoint <- predicatesEndpointZ
        predicatesRoute <- predicatesRouteR(predicatesEndpoint)
        queryEndpoint <- queryEndpointZ
        queryRoute <- queryRouteR(queryEndpoint)
        routes = queryRoute <+> predicatesRoute
        openAPI: String = List(queryEndpoint, predicatesEndpoint)
          .toOpenAPI("CAM-KP API", "0.1")
          .copy(info = openAPIInfo)
          .copy(tags = List(sttp.tapir.openapi.Tag("translator")))
          .servers(List(sttp.tapir.openapi.Server(appConfig.location)))
          .toYaml
        openAPIJson <- ZIO.fromEither(io.circe.yaml.parser.parse(openAPI))
        info: String =
          """
             {
                "info": {
                  "x-translator": {
                    "component": "KP",
                    "team": "Exposures Provider"
                  }
                }
             }
          """
        infoJson <- ZIO.fromEither(io.circe.parser.parse(info))
        openAPIinfo = infoJson.deepMerge(openAPIJson).asYaml.spaces2
        docsRoute = swaggerRoutes(openAPIinfo)
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

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = {
    val appLayer = HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer ++ configLayer ++ Blocking.live
    server.provideLayer(appLayer).exitCode
  }

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

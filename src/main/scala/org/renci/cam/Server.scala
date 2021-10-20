package org.renci.cam

import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.generic.auto._
import io.circe.yaml.syntax._
import org.http4s._
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.middleware._
import org.renci.cam.Biolink._
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.SPARQLQueryExecutor.SPARQLCache
import org.renci.cam.domain._
import sttp.tapir.Endpoint
import sttp.tapir.docs.openapi._
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe._
import sttp.tapir.openapi.circe.yaml._
import sttp.tapir.openapi.{Contact, Info, License}
import sttp.tapir.server.http4s.ztapir.ZHttp4sServerInterpreter
import sttp.tapir.swagger.SwaggerUI
import sttp.tapir.ztapir._
import zio._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.config.typesafe.TypesafeConfig
import zio.config.{getConfig, ZConfig}
import zio.interop.catz._

import java.util.Properties
import scala.concurrent.duration._

object Server extends App with LazyLogging {

  type EndpointEnv = ZConfig[AppConfig] with Clock with Blocking with HttpClient with Has[BiolinkData] with Has[SPARQLCache]

  object LocalTapirJsonCirce extends TapirJsonCirce {
    override def jsonPrinter: Printer = Printer.noSpaces.copy(dropNullValues = true)
  }

  import LocalTapirJsonCirce._

  val metaKnowledgeGraphEndpointZ: URIO[Has[BiolinkData], Endpoint[Unit, String, MetaKnowledgeGraph, Any]] =
    for {
      biolinkData <- biolinkData
    } yield endpoint.get
      .in("meta_knowledge_graph")
      .errorOut(stringBody)
      .out(
        {

          implicit val bcKeyDecoder: KeyDecoder[BiolinkClass] = Implicits.biolinkClassKeyDecoder(biolinkData.classes)
          implicit val bcKeyEncoder: KeyEncoder[BiolinkClass] = Implicits.biolinkClassKeyEncoder

          implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
          implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)

          implicit val blClassDecoder: Decoder[BiolinkClass] = Implicits.biolinkClassDecoder(biolinkData.classes)
          implicit val blClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder

          implicit val blPredicateDecoder: Decoder[BiolinkPredicate] = Implicits.biolinkPredicateDecoder(biolinkData.predicates)
          implicit val blPredicateEncoder: Encoder[BiolinkPredicate] = Implicits.biolinkPredicateEncoder(biolinkData.prefixes)

          jsonBody[MetaKnowledgeGraph]
        }
      )
      .summary("Get MetaKnowledgeGraph used at this service")

  def metaKnowledgeGraphRouteR(
    metaKnowledgeGraphEndpoint: Endpoint[Unit, String, MetaKnowledgeGraph, Any]): HttpRoutes[RIO[EndpointEnv, *]] =
    ZHttp4sServerInterpreter[EndpointEnv]()
      .from(metaKnowledgeGraphEndpoint) { case () =>
        val program = for {
          response <- MetaKnowledgeGraphService.run
        } yield response
        program.mapError(error => error.getMessage)
      }
      .toRoutes

  val queryEndpointZ: URIO[Has[BiolinkData], Endpoint[(Option[Int], Option[Boolean], TRAPIQuery), String, TRAPIResponse, Any]] = {
    for {
      biolinkData <- biolinkData
    } yield {
      implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
      implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)

      implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
      implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)

      implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder
      implicit val biolinkClassDecoder: Decoder[BiolinkClass] = Implicits.biolinkClassDecoder(biolinkData.classes)

      implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Implicits.biolinkPredicateEncoder(biolinkData.prefixes)
      implicit val biolinkPredicateDecoder: Decoder[List[BiolinkPredicate]] =
        Implicits.predicateOrPredicateListDecoder(biolinkData.predicates)

      val example = {
        val n0Node = TRAPIQueryNode(None, Some(List(BiolinkClass("GeneOrGeneProduct"))), None)
        val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("BiologicalProcess"))), None)
        val e0Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("has_participant"))), "n1", "n0", None)
        val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
        val message = TRAPIMessage(Some(queryGraph), None, None)
        TRAPIQuery(message, None)
      }

      val defaultAndExampleLimit = Some(10)
      val defaultAndExampleIncludeExtraEdges = Some(false)

      endpoint.post
        .in("query")
        .in(
          query[Option[Int]]("limit")
            .default(defaultAndExampleLimit)
            .example(defaultAndExampleLimit)
            .and(query[Option[Boolean]]("include_extra_edges")
              .default(defaultAndExampleIncludeExtraEdges)
              .example(defaultAndExampleIncludeExtraEdges))
        )
        .in(jsonBody[TRAPIQuery].default(example).example(example))
        .errorOut(stringBody)
        .out(jsonBody[TRAPIResponse])
        .summary("Submit a TRAPI question graph and retrieve matching solutions")
    }
  }

  def queryRouteR(
    queryEndpoint: Endpoint[(Option[Int], Option[Boolean], TRAPIQuery), String, TRAPIResponse, Any]): HttpRoutes[RIO[EndpointEnv, *]] =
    ZHttp4sServerInterpreter[EndpointEnv]()
      .from(queryEndpoint) { case (limit, includeExtraEdges, body) =>
        val program: ZIO[EndpointEnv, Throwable, TRAPIResponse] = for {
          queryGraph <-
            ZIO
              .fromOption(body.message.query_graph)
              .orElseFail(new InvalidBodyException("A query graph is required, but hasn't been provided."))
          limitValue <- ZIO.fromOption(limit).orElse(ZIO.effect(1000))
          includeExtraEdgesValue <- ZIO.fromOption(includeExtraEdges).orElse(ZIO.effect(false))
          message <- QueryService.run(limitValue, includeExtraEdgesValue, queryGraph)
        } yield TRAPIResponse(message, Some("Success"), None, None)
        program.mapError(error => error.getMessage)
      }
      .toRoutes

  val openAPIInfo: Info = Info(
    "CAM-KP API",
    "0.1",
    Some("TRAPI interface to database of Causal Activity Models"),
    Some("https://opensource.org/licenses/MIT"),
    Some(Contact(Some("Jim Balhoff"), Some("balhoff@renci.org"), None)),
    Some(License("MIT License", Some("https://opensource.org/licenses/MIT")))
  )

  val server: RIO[ZEnv with EndpointEnv, Unit] =
    ZIO.runtime[ZEnv with EndpointEnv].flatMap { implicit runtime =>
      for {
        appConfig <- getConfig[AppConfig]
        biolinkData <- biolinkData
        metaKnowledgeGraphEndpoint <- metaKnowledgeGraphEndpointZ
        metaKnowledgeGraphRoute = metaKnowledgeGraphRouteR(metaKnowledgeGraphEndpoint)
        queryEndpoint <- queryEndpointZ
        queryRoute = queryRouteR(queryEndpoint)
        routes = queryRoute <+> metaKnowledgeGraphRoute
        openAPI: String = OpenAPIDocsInterpreter()
          .toOpenAPI(List(queryEndpoint, metaKnowledgeGraphEndpoint), "CAM-KP API", "0.1")
          .copy(info = openAPIInfo)
          .copy(tags = List(sttp.tapir.apispec.Tag("translator"), sttp.tapir.apispec.Tag("trapi")))
          .servers(List(sttp.tapir.openapi.Server(s"${appConfig.location}/${appConfig.trapiVersion}")))
          .toYaml
        openAPIJson <- ZIO.fromEither(io.circe.yaml.parser.parse(openAPI))
        info: String =
          s"""
             {
                "info": {
                  "x-translator": {
                    "component": "KP",
                    "team": ["Exposures Provider"],
                    "biolink-version": "${biolinkData.version}"
                  },
                  "x-trapi": {
                    "version": "${appConfig.trapiVersion}",
                    "operations": [ "lookup" ]
                  }
                }
             }
          """
        infoJson <- ZIO.fromEither(io.circe.parser.parse(info))
        openAPIinfo = infoJson.deepMerge(openAPIJson).asYaml.spaces2
        docsRoute = ZHttp4sServerInterpreter().from(SwaggerUI[RIO[EndpointEnv, *]](openAPIinfo)).toRoutes
        httpApp = Router("/" -> (routes <+> docsRoute)).orNotFound
        httpAppWithLogging = Logger.httpApp(true, false)(httpApp)
        result <-
          BlazeServerBuilder[RIO[EndpointEnv, *]](runtime.platform.executor.asEC)
            .bindHttp(appConfig.port, appConfig.host)
            .withHttpApp(CORS.policy.withAllowOriginAll
              .withAllowCredentials(false)
              .apply(httpAppWithLogging))
            .withResponseHeaderTimeout(120.seconds)
            .withIdleTimeout(180.seconds)
            .serve
            .compile
            .drain
      } yield result
    }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = {
    val appLayer = HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer
    server.provideCustomLayer(appLayer).exitCode
  }

  // hack using SwaggerHttp4s code to handle running in subdirectory
//  private def swaggerRoutes(yaml: String): HttpRoutes[Task] = {
//    val dsl = Http4sDsl[Task]
//    import dsl._
//    val contextPath = "docs"
//    val yamlName = "docs.yaml"
//    HttpRoutes.of[Task] {
//      case path @ GET -> Root / `contextPath` =>
//        val queryParameters = Map("url" -> Seq(s"$yamlName"))
//        Uri
//          .fromString(s"$contextPath/index.html")
//          .map(uri => uri.setQueryParams(queryParameters))
//          .map(uri => PermanentRedirect(Location(uri)))
//          .getOrElse(NotFound())
//      case GET -> Root / `contextPath` / `yamlName` =>
//        Ok(yaml)
//      case GET -> Root / `contextPath` / swaggerResource =>
//        StaticFile
//          .fromResource[Task](
//            s"/META-INF/resources/webjars/swagger-ui/$swaggerVersion/$swaggerResource",
//            Blocker.liftExecutionContext(ExecutionContext.global)
//          )
//          .getOrElseF(NotFound())
//    }
//  }

  private val swaggerVersion = {
    val p = new Properties()
    val pomProperties = getClass.getResourceAsStream("/META-INF/maven/org.webjars/swagger-ui/pom.properties")
    try p.load(pomProperties)
    finally pomProperties.close()
    p.getProperty("version")
  }

}

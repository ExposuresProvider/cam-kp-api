package org.renci.cam

import org.renci.cam.domain._
import cats.implicits._
import io.circe.generic.auto._
import org.http4s._
import org.http4s.client._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.server.Router
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
import scala.concurrent.ExecutionContext
import org.http4s.headers._
import org.apache.jena.query.ResultSetFactory
import org.apache.commons.io.IOUtils
import org.apache.commons.text.CaseUtils
import java.nio.charset.StandardCharsets
// import org.apache.jena.graph.Node;
// import org.apache.jena.query.ResultSet;
// import org.apache.jena.query.ResultSetFactory;
// import org.apache.jena.sparql.core.Var;
// import org.apache.jena.sparql.engine.binding.Binding;
import org.http4s.server.middleware.Logger
import scala.collection.JavaConverters._

object Server extends App {

  implicit val runtime: Runtime[ZEnv] = Runtime.default

  val queryEndpoint: ZEndpoint[(Int, KGSQueryGraph), String, String] =
    endpoint.post
      .in("query")
      .in(query[Int]("limit"))
      .in(jsonBody[KGSQueryGraph])
      .errorOut(stringBody)
      .out(jsonBody[String])

  val queryRoute: HttpRoutes[Task] = queryEndpoint.toRoutes {
    case (limit, queryGraph) =>
      print(limit)
      val query = StringBuilder.newBuilder
      val nodeTypes = queryGraph.nodes.collect {
        case (node) if node.`type`.nonEmpty => (node.id, "bl:" + CaseUtils.toCamelCase(node.`type`, true, '_'))
      }.toMap

      nodeTypes ++ queryGraph.nodes.collect { case (node) if node.curie.nonEmpty => (node.id, node.curie.get) }.toMap

      queryGraph.nodes.foreach {
        case node => query.append(String.format("  ?%1$s sesame:directType ?%1$s_type .%n", node.`type`))
      }

      var instanceVars: Set[String] = Set()
      var instanceVarsToTypes: Map[String, String] = Map()

      for ((edge, idx) <- queryGraph.edges.view.zipWithIndex)
        if (edge.`type`.nonEmpty) {
          val predicates = for {
            httpClientManaged <- QueryService.makeHttpClient
            predicate_query = s"""PREFIX bl: <https://w3id.org/biolink/vocab/>
              SELECT DISTINCT ?predicate WHERE { bl:${edge.`type`} <http://reasoner.renci.org/vocab/slot_mapping> ?predicate . }"""
            request = Request[Task](Method.POST, Uri.uri("http://152.54.9.207:9999/blazegraph/sparql"))
              .withHeaders(
                Accept(MediaType.application.json),
                `Content-Type`(MediaType.application.`sparql-query`)
              )
              .withEntity(predicate_query)
            response <- httpClientManaged.use(_.expect[String](request))
            resultSet <- {
              val is = IOUtils.toInputStream(response, StandardCharsets.UTF_8)
              val rs = ResultSetFactory.fromJSON(is)
              is.close()
              ZIO.effect(rs)
            }
            predicates = (for {
                solution <- resultSet.asScala
                v <- solution.varNames.asScala
                node = solution.get(v)
              } yield s"<$node>").mkString(" ")
          } yield predicates

          query.append(s"VALUES ?${edge.`type`} { $predicates }\n")
          query.append(s"  ?${edge.source_id} ?${edge.id} ?${edge.target_id} .\n)")

          instanceVars += (edge.source_id, edge.target_id)
          instanceVarsToTypes += (edge.source_id -> edge.source_id, edge.target_id -> edge.target_id)

        }

      for ((key, value) <- instanceVarsToTypes) {
        val varType = nodeTypes.get(value)
        query.append(s"?$key rdf:type $varType .\n")
      }

      query.append("}")

      if (limit > 0) query.append(s" LIMIT $limit")

      val prequel = StringBuilder.newBuilder
      for ((key, value) <- QueryService.PREFIXES)
        prequel.append(s"PREFIX $key: <$value>\n")

      val ids = instanceVars.map(a => s"?$a").toList :::
        queryGraph.nodes.map(a => s"?${a.id}_type") :::
        queryGraph.edges.map(a => s"?${a.id}")

      prequel.append(s"\nSELECT DISTINCT ${ids.mkString(" ")} WHERE {\n")

      val full_query = prequel.toString() + query.toString()
      // for {
      //   request <- ZIO.effect(
      //     Request[Task](Method.POST, Uri.uri("http://152.54.9.207:9999/blazegraph/sparql"))
      //       .withHeaders(Accept.parse("application/json").toOption.get,
      //                    `Content-Type`.parse("application/sparql-query").toOption.get)
      //       .withEntity(full_query)
      //   )
      //   serviceResults <- BlazeClientBuilder[Task](ExecutionContext.global).resource.use { httpClient =>
      //     httpClient.expect[String](request)
      //   }
      //   resultSet <- {
      //     val is = IOUtils.toInputStream(serviceResults, StandardCharsets.UTF_8)
      //     val rs = ResultSetFactory.fromJSON(is)
      //     is.close()
      //     ZIO.effect(rs)
      //   }
      // } ZIO.succeed("")

      ZIO.succeed("asdfasdf")
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
      .as(ExitCode.success)
      .catchAllCause(cause => UIO(println(cause.prettyPrint)))
      .as(ExitCode.failure)

  }

}

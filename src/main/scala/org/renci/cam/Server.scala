package org.renci.cam

import java.nio.charset.StandardCharsets

import cats.implicits._
import io.circe.generic.auto._
import org.apache.commons.io.IOUtils
import org.apache.jena.query.ResultSetFactory
import org.http4s._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger
import org.http4s.syntax.kleisli._
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

import scala.collection.mutable

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
      val nodeTypes = QueryService.getNodeTypes(queryGraph.nodes)

      val query = StringBuilder.newBuilder
      queryGraph.nodes foreach {
        case (node) => query.append(String.format("  ?%1$s sesame:directType ?%1$s_type .%n", node.`type`))
      }

      var instanceVars: Set[String] = Set()
      var instanceVarsToTypes: Map[String, String] = Map()

      for ((edge, idx) <- queryGraph.edges.view.zipWithIndex)
        if (edge.`type`.nonEmpty) {
          val predicateQuery = s"""PREFIX bl: <https://w3id.org/biolink/vocab/>
              SELECT DISTINCT ?predicate WHERE { bl:${edge.`type`} <http://reasoner.renci.org/vocab/slot_mapping> ?predicate . }"""
          val predicatesProgram = for {
            response <- QueryService.runBlazegraphQuery(predicateQuery)
            resultSet <- {
              val is = IOUtils.toInputStream(response, StandardCharsets.UTF_8)
              val rs = ResultSetFactory.fromJSON(is)
              is.close()
              ZIO.effect(rs)
            }
            bindings <- {

              var bindingList = new mutable.ListBuffer[String]()
              while (resultSet.hasNext()) {
                val binding = resultSet.nextBinding
                val varIter = binding.vars
                while (varIter.hasNext()) {
                  val nodeVar = varIter.next()
                  val node = binding.get(nodeVar)
                  bindingList += node.toString
                }
              }

              ZIO.effect(bindingList.toList.map(a => String.format("<%s>", a)).mkString(" "))
            }
          } yield bindings

          val predicates = runtime.unsafeRun(predicatesProgram)

          query
            .append(String.format("VALUES ?%s { %s }%n", edge.`type`, predicates))
          query.append(String.format("  ?%s ?%s ?%s .%n)", edge.source_id, edge.id, edge.target_id))

          instanceVars += (edge.source_id, edge.target_id)
          instanceVarsToTypes += (edge.source_id -> edge.source_id, edge.target_id -> edge.target_id)

        }

      for ((key, value) <- instanceVarsToTypes) {
        val varType = nodeTypes.get(value)
        query.append(String.format("?%s rdf:type %s .%n", key, varType))
      }

      query.append("}")

      if (limit > 0) query.append(String.format(" LIMIT %s", limit.toString()))

      val prequel = StringBuilder.newBuilder
      for ((key, value) <- QueryService.PREFIXES)
        prequel.append(String.format("PREFIX %s: <%s>%n", key, value))

      val ids = instanceVars.map(a => String.format("?%s", a)).toList :::
        queryGraph.nodes.map(a => String.format("?%s_type", a.id)).toList :::
        queryGraph.edges.map(a => String.format("?%s", a.id)).toList

      prequel.append(String.format("%nSELECT DISTINCT %s WHERE { %n", ids.mkString(" ")))

      val full_query = prequel.toString() + query.toString()

      val queryResponse = QueryService.runBlazegraphQuery(full_query)

      queryResponse
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

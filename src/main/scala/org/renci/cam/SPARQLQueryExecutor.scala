package org.renci.cam

import java.nio.charset.StandardCharsets

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.IOUtils
import org.apache.jena.query.{Query, QuerySolution, ResultSet, ResultSetFactory}
import org.http4s._
import org.http4s.headers.`Content-Type`
import org.http4s.implicits._
import org.phenoscape.sparql.FromQuerySolution
import org.renci.cam.HttpClient.HttpClient
import zio.ZIO.ZIOAutoCloseableOps
import zio.config.ZConfig
import zio.interop.catz._
import zio.{RIO, Task, ZIO, config => _}

import scala.jdk.CollectionConverters._

object SPARQLQueryExecutor extends LazyLogging {

  implicit val jsonDecoder: EntityDecoder[Task, ResultSet] =
    EntityDecoder.decodeBy(mediaType"application/sparql-results+json") { media =>
      DecodeResult(
        EntityDecoder
          .decodeText(media)
          .map(jsonText => IOUtils.toInputStream(jsonText, StandardCharsets.UTF_8))
          .bracketAuto(input => Task.effect(ResultSetFactory.fromJSON(input)))
          .mapError[DecodeFailure](e => MalformedMessageBodyFailure("Invalid JSON for SPARQL results", Some(e)))
          .either
      )
    }

  implicit val queryEncoder: EntityEncoder[Task, Query] = EntityEncoder[Task, String]
    .withContentType(`Content-Type`(MediaType.application.`sparql-query`))
    .contramap(_.toString)

  def runSelectQueryAs[T: FromQuerySolution](query: Query): RIO[ZConfig[AppConfig] with HttpClient, List[T]] =
    for {
      resultSet <- runSelectQuery(query)
      results = resultSet.map(FromQuerySolution.mapSolution[T])
      validResults <- ZIO.foreach(results)(ZIO.fromTry(_))
    } yield validResults

  def runSelectQuery(query: Query): RIO[ZConfig[AppConfig] with HttpClient, List[QuerySolution]] =
    for {
      appConfig <- zio.config.getConfig[AppConfig]
      _ = logger.debug("query: {}", query)
      client <- HttpClient.client
      uri = appConfig.sparqlEndpoint
      request = Request[Task](Method.POST, uri).withEntity(query)
      response <- client.expect[ResultSet](request)
    } yield response.asScala.toList

}

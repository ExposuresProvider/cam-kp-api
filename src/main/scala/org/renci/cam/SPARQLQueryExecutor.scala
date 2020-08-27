package org.renci.cam

import java.nio.charset.StandardCharsets

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.IOUtils
import org.apache.jena.query.{Query, ResultSet, ResultSetFactory}
import org.http4s._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.headers.`Content-Type`
import org.http4s.implicits._
import org.phenoscape.sparql.FromQuerySolution
import org.renci.cam.HttpClient.HttpClient
import zio.ZIO.ZIOAutoCloseableOps
import zio._
import zio.config.ZConfig
import zio.interop.catz._
import zio.{RIO, Task, TaskManaged, UIO, ZIO, config => _}

import scala.collection.JavaConverters._
import scala.concurrent.duration.{Duration, MINUTES}

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
      results = resultSet.asScala.map(FromQuerySolution.mapSolution[T]).toList
      validResults <- ZIO.foreach(results)(ZIO.fromTry(_))
    } yield validResults

  def runSelectQuery(query: Query): RIO[ZConfig[AppConfig] with HttpClient, ResultSet] =
    for {
      appConfig <- zio.config.config[AppConfig]
      client <- HttpClient.client
      uri = appConfig.sparqlEndpoint
      _ = logger.debug("query: {}", query)
      request = Request[Task](Method.POST, uri).withEntity(query)
      response <- client.expect[ResultSet](request)
    } yield response

}

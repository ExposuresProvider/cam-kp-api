package org.renci.cam

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.IOUtils
import org.apache.jena.query.{Query, QuerySolution, ResultSet, ResultSetFactory}
import org.http4s._
import org.http4s.headers.`Content-Type`
import org.http4s.implicits._
import org.phenoscape.sparql.FromQuerySolution
import org.renci.cam.HttpClient.HttpClient
import zio.ZIO.ZIOAutoCloseableOps
import zio.cache.{Cache, Lookup}
import zio.config.ZConfig
import zio.duration.durationInt
import zio.interop.catz._
import zio.interop.catz.implicits._
import zio.{Has, RIO, Task, URIO, ZIO, config => _}

import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters._

object SPARQLQueryExecutor extends LazyLogging {

  type SPARQLCache = Cache[Query, Throwable, List[QuerySolution]]

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

  def runSelectQueryWithCacheAs[T: FromQuerySolution](query: Query): RIO[ZConfig[AppConfig] with HttpClient with Has[SPARQLCache], List[T]] =
    for {
      cache <- ZIO.service[SPARQLCache]
      resultSet <- cache.get(query)
      results = resultSet.map(FromQuerySolution.mapSolution[T])
      validResults <- ZIO.foreach(results)(ZIO.fromTry(_))
    } yield validResults

  def runSelectQueryWithCache(query: Query): RIO[ZConfig[AppConfig] with HttpClient with Has[SPARQLCache], List[QuerySolution]] =
    for {
    cache <- ZIO.service[SPARQLCache]
    result <- cache.get(query)
    } yield result

  def makeCache: URIO[ZConfig[AppConfig] with HttpClient, Cache[Query, Throwable, List[QuerySolution]]] = Cache.make(50, 20.days, Lookup(runSelectQuery))

}

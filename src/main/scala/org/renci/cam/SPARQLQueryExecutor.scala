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
import zio.ZIO.ZIOAutoCloseableOps
import zio.config.Config
import zio.interop.catz._
import zio.{RIO, Task, TaskManaged, UIO, ZIO, config => _}

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

  def makeHttpClient: UIO[TaskManaged[Client[Task]]] =
    ZIO.runtime[Any].map { implicit rts =>
      BlazeClientBuilder[Task](rts.platform.executor.asEC).withConnectTimeout(Duration(3, MINUTES)).resource.toManaged
    }

  def runSelectQuery(query: Query): RIO[Config[AppConfig], ResultSet] =
    for {
      appConfig <- zio.config.config[AppConfig]
      clientManaged <- makeHttpClient
      uri = appConfig.sparqlEndpoint
      request = Request[Task](Method.POST, uri).withEntity(query)
      response <- clientManaged.use(_.expect[ResultSet](request))
    } yield response

}

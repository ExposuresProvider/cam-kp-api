package org.renci.cam

import io.circe.Json
import org.http4s.{MediaType, Method, Request}
import org.http4s.headers.{`Content-Type`, Accept}
import zio.{Task, ZIO}
import org.http4s.implicits._
import org.http4s.circe._
import zio.interop.catz._

class Utilities {

  def getBiolinkModel =
    for {
      httpClient <- SPARQLQueryExecutor.makeHttpClient
      uri = uri"https://biolink.github.io/biolink-model/context.jsonld"
      request =
        Request[Task](Method.GET, uri).withHeaders(Accept(MediaType.application.`ld+json`), `Content-Type`(MediaType.application.`ld+json`))
      biolinkModelJson <- httpClient.use(_.expect[Json](request))
      cursor = biolinkModelJson.hcursor
      contextValue <- ZIO.fromEither(cursor.downField("@context").as[Map[String, Json]])
      curies =
        contextValue
          .map {
            case (key, value) => (key, value.as[String])
          }
          .collect {
            case (key, Right(value)) => (key, value)
          }
    } yield curies

}

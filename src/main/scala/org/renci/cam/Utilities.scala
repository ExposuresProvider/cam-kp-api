package org.renci.cam

import io.circe.Json
import org.http4s.circe._
import org.http4s.headers.Accept
import org.http4s.implicits._
import org.http4s.{MediaType, Method, Request}
import org.renci.cam.HttpClient.HttpClient
import zio._
import zio.interop.catz._

object Utilities {

  def getBiolinkPrefixes: ZIO[HttpClient, Throwable, Map[String, String]] =
    for {
      httpClient <- HttpClient.client
      uri = uri"https://biolink.github.io/biolink-model/context.jsonld"
      request = Request[Task](Method.GET, uri).withHeaders(Accept(MediaType.application.`ld+json`))
      biolinkModelJson <- httpClient.expect[Json](request)
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

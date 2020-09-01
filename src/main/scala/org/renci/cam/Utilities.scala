package org.renci.cam

import scala.io.Source._
import io.circe.Json
import io.circe.parser.parse
import org.http4s.circe._
import org.http4s.headers.Accept
import org.http4s.implicits._
import org.http4s.{MediaType, Method, Request}
import org.renci.cam.HttpClient.HttpClient
import zio._
import zio.interop.catz._

object Utilities {

  def getBiolinkPrefixesFromURL: ZIO[HttpClient, Throwable, PrefixesMap] =
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
    } yield PrefixesMap(curies)

  def getBiolinkPrefixesFromFile: ZIO[Any, Throwable, PrefixesMap] = for {
    val prefixesStr = fromFile("prefixes.json").mkString
    val prefixesJson = parse(prefixesStr)
    val cursor = prefixesJson.hcursor
    val contextValue = ZIO.fromEither(cursor.downField("@context").as[Map[String, Json]])
    val curies =
      contextValue
        .map {
          case (key, value) => (key, value.as[String])
        }
        .collect {
          case (key, Right(value)) => (key, value)
        }
  } yield curies //getResourcesAsStream

  def getPrefixes: ZIO[HttpClient, Throwable, PrefixesMap] = getBiolinkPrefixesFromURL.orElse(getBiolinkPrefixesFromFile)

  def makePrefixesLayer: UIO[TaskLayer[Has[PrefixesMap]]] = getBiolinkPrefixesFromURL.map(ZLayer.fromManaged())

  val biolinkPrefixes: ZIO[HttpClient, Nothing, TaskLayer[Has[PrefixesMap]]] = ZIO.service

  case class PrefixesMap(prefixesMap: Map[String, String])

}

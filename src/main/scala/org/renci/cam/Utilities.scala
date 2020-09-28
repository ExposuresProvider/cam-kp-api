package org.renci.cam

import io.circe.Json
import io.circe.parser.parse
import org.http4s.circe._
import org.http4s.headers.Accept
import org.http4s.implicits._
import org.http4s.{MediaType, Method, Request}
import org.renci.cam.HttpClient.HttpClient
import zio._
import zio.interop.catz._

import scala.io.Source

object Utilities {

  def getBiolinkPrefixesFromURL: ZIO[HttpClient, Throwable, PrefixesMap] =
    for {
      httpClient <- HttpClient.client
      uri = uri"https://biolink.github.io/biolink-model/context.jsonld"
      request = Request[Task](Method.GET, uri).withHeaders(Accept(MediaType.application.`ld+json`))
      biolinkModelJson <- httpClient.expect[Json](request)
      contextJson <- ZIO.fromOption(biolinkModelJson.hcursor.downField("@context").focus).orElseFail(new Exception("failed to traverse down to context"))
      curies <- ZIO.fromEither(contextJson.deepDropNullValues.mapObject(f =>
        f.filter(pred => pred._2.isString && pred._1 != "type" && pred._1 != "id" && pred._1 != "@vocab")).as[Map[String, String]])
    } yield PrefixesMap(curies)

  def getBiolinkPrefixesFromFile: ZIO[Any, Throwable, PrefixesMap] = {
    val sourceManaged = for {
      fileStream <- Managed.fromAutoCloseable(Task.effect(getClass.getResourceAsStream("/prefixes.json")))
      source <- Managed.fromAutoCloseable(Task.effect(Source.fromInputStream(fileStream)))
    } yield source
    for {
      prefixesStr <- sourceManaged.use(source => ZIO.effect(source.getLines.mkString))
      prefixesJson <- Task.effect(parse(prefixesStr).getOrElse(Json.Null))
      curies <- ZIO.fromEither(prefixesJson.as[Map[String, String]])
    } yield PrefixesMap(curies)
  }

  def localPrefixes: ZIO[Any, Throwable, Map[String, String]] = {
    val sourceManaged = for {
      fileStream <- Managed.fromAutoCloseable(Task.effect(getClass.getResourceAsStream("/legacy_prefixes.json")))
      source <- Managed.fromAutoCloseable(Task.effect(Source.fromInputStream(fileStream)))
    } yield source
    for {
      prefixesStr <- sourceManaged.use(source => ZIO.effect(source.getLines.mkString))
      prefixesJson <- ZIO.fromEither(parse(prefixesStr))
      prefixes <- ZIO.fromEither(prefixesJson.as[Map[String, String]])
    } yield prefixes
  }

  def getPrefixes: ZIO[HttpClient, Throwable, PrefixesMap] =
    for {
      local <- localPrefixes
      biolink <- getBiolinkPrefixesFromURL.orElse(getBiolinkPrefixesFromFile)
      combined = local ++ biolink.prefixesMap
    } yield PrefixesMap(combined)

  def makePrefixesLayer: ZLayer[HttpClient, Throwable, Has[PrefixesMap]] = ZLayer.fromEffect(getPrefixes)

  val biolinkPrefixes: URIO[Has[PrefixesMap], PrefixesMap] = ZIO.service

  final case class PrefixesMap(prefixesMap: Map[String, String])

}

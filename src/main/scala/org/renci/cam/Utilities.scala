package org.renci.cam

import io.circe.Json
import org.http4s.circe._
import org.http4s.headers.Accept
import org.http4s.implicits._
import org.http4s.{MediaType, Method, Request}
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate}
import zio._
import zio.interop.catz._

import scala.io.Source

object Utilities {

  def makeUtilitiesLayer: ZLayer[HttpClient, Throwable, Has[BiolinkPrefixes] with Has[List[BiolinkPredicate]] with Has[List[BiolinkClass]]] =
    ZLayer.fromEffect(getBiolinkPrefixes) ++ ZLayer.fromEffect(getBiolinkPredicates) ++ ZLayer.fromEffect(getBiolinkClasses)

  def getBiolinkPrefixesFromURL: ZIO[HttpClient, Throwable, BiolinkPrefixes] =
    for {
      httpClient <- HttpClient.client
      uri = uri"https://biolink.github.io/biolink-model/context.jsonld"
      request = Request[Task](Method.GET, uri).withHeaders(Accept(MediaType.application.`ld+json`))
      biolinkModelJson <- httpClient.expect[Json](request)
      contextJson <-
        ZIO.fromOption(biolinkModelJson.hcursor.downField("@context").focus).orElseFail(new Exception("failed to traverse down to context"))
      curies <- ZIO.fromEither(
        contextJson.deepDropNullValues
          .mapObject(f => f.filter(pred => pred._2.isString && pred._1 != "type" && pred._1 != "id" && pred._1 != "@vocab"))
          .as[Map[String, String]]
      )
    } yield BiolinkPrefixes(curies)

  def getBiolinkPrefixesFromFile: ZIO[Any, Throwable, BiolinkPrefixes] = {
    val sourceManaged = for {
      fileStream <- Managed.fromAutoCloseable(Task.effect(getClass.getClassLoader.getResourceAsStream("prefixes.json")))
      source <- Managed.fromAutoCloseable(Task.effect(Source.fromInputStream(fileStream)))
    } yield source
    for {
      prefixesStr <- sourceManaged.use(source => ZIO.effect(source.getLines.mkString))
      prefixesJson <- Task.effect(io.circe.parser.parse(prefixesStr).getOrElse(Json.Null))
      curies <- ZIO.fromEither(prefixesJson.as[Map[String, String]])
    } yield BiolinkPrefixes(curies)
  }

  def localPrefixes: ZIO[Any, Throwable, Map[String, String]] = {
    val sourceManaged = for {
      fileStream <- Managed.fromAutoCloseable(Task.effect(getClass.getClassLoader.getResourceAsStream("legacy_prefixes.json")))
      source <- Managed.fromAutoCloseable(Task.effect(Source.fromInputStream(fileStream)))
    } yield source
    for {
      prefixesStr <- sourceManaged.use(source => ZIO.effect(source.getLines.mkString))
      prefixesJson <- ZIO.fromEither(io.circe.parser.parse(prefixesStr))
      prefixes <- ZIO.fromEither(prefixesJson.as[Map[String, String]])
    } yield prefixes
  }

  def getBiolinkPrefixes: ZIO[HttpClient, Throwable, BiolinkPrefixes] =
    for {
      local <- localPrefixes
      biolink <- getBiolinkPrefixesFromFile
      //biolink <- getBiolinkPrefixesFromURL.orElse(getBiolinkPrefixesFromFile)
      combined = local ++ biolink.prefixes
    } yield BiolinkPrefixes(combined)

  val biolinkPrefixes: URIO[Has[BiolinkPrefixes], BiolinkPrefixes] = ZIO.service

  final case class BiolinkPrefixes(prefixes: Map[String, String])

  // originates from https://biolink.github.io/biolink-model/biolink-model.yaml
  def getBiolinkPredicatesFromFile: ZIO[Any, Throwable, List[BiolinkPredicate]] = {
    val sourceManaged = for {
      source <-
        Managed.fromAutoCloseable(Task.effect(Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("biolink-model.yaml"))))
    } yield source
    for {
      slotsStr <- sourceManaged.use(source => ZIO.effect(source.mkString))
      json <- ZIO.fromEither(io.circe.yaml.parser.parse(slotsStr))
      keys <- ZIO.fromOption(json.hcursor.downField("slots").keys).orElseFail(throw new Exception("couldn't get slots"))
      predicates = keys.map(a => BiolinkPredicate(a.replaceAll(" ", "_"))).toList
    } yield predicates
  }

  def getBiolinkPredicates: ZIO[HttpClient, Throwable, List[BiolinkPredicate]] =
    for {
      biolinkPredicates <- getBiolinkPredicatesFromFile
    } yield biolinkPredicates

  def makeSlotsLayer: ZLayer[HttpClient, Throwable, Has[List[BiolinkPredicate]]] = ZLayer.fromEffect(getBiolinkPredicates)

  val biolinkPredicates: URIO[Has[List[BiolinkPredicate]], List[BiolinkPredicate]] = ZIO.service

  // originates from https://biolink.github.io/biolink-model/biolink-model.yaml
  def getBiolinkClassesFromFile: ZIO[Any, Throwable, List[BiolinkClass]] = {
    val sourceManaged = for {
      source <-
        Managed.fromAutoCloseable(Task.effect(Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("biolink-model.yaml"))))
    } yield source
    for {
      classesStr <- sourceManaged.use(source => ZIO.effect(source.mkString))
      json <- ZIO.fromEither(io.circe.yaml.parser.parse(classesStr))
      keys <- ZIO.fromOption(json.hcursor.downField("classes").keys).orElseFail(throw new Exception("couldn't get classes"))
      classes = keys.map(a => { BiolinkClass(a.replaceAll(" ", "_")) }).toList
    } yield classes
  }

  def getBiolinkClasses: ZIO[HttpClient, Throwable, List[BiolinkClass]] =
    for {
      biolinkClasses <- getBiolinkClassesFromFile
    } yield biolinkClasses

  def makeClassesLayer: ZLayer[HttpClient, Throwable, Has[List[BiolinkClass]]] = ZLayer.fromEffect(getBiolinkClasses)

  val biolinkClasses: URIO[Has[List[BiolinkClass]], List[BiolinkClass]] = ZIO.service



}

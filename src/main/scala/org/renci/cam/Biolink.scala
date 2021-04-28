package org.renci.cam

import io.circe.Json
import org.apache.commons.lang3.StringUtils
import org.http4s.circe._
import org.http4s.headers.Accept
import org.http4s.implicits._
import org.http4s.{MediaType, Method, Request}
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate}
import zio._
import zio.interop.catz._

import scala.io.Source

object Biolink {

  final case class BiolinkData(prefixes: Map[String, String], classes: List[BiolinkClass], predicates: List[BiolinkPredicate])

  def makeUtilitiesLayer: ZLayer[HttpClient, Throwable, Has[BiolinkData]] = getBiolinkData.toLayer

  val biolinkData: URIO[Has[BiolinkData], BiolinkData] = ZIO.service

  def getBiolinkData: ZIO[HttpClient, Throwable, BiolinkData] =
    for {
      (biolinkPrefixes, classes, predicates) <- getBiolinkPrefixesAndClassesAndPredicatesFromFile
      legacy <- localPrefixes
      prefixes <- getBiolinkPrefixesFromURL.orElse(getBiolinkPrefixesFromFile)
      prefixOverrides <- getPrefixOverrides
      combined = legacy ++ biolinkPrefixes ++ prefixes ++ prefixOverrides

    } yield BiolinkData(combined, classes, predicates)

  def getPrefixOverrides: ZIO[Any, Throwable, Map[String, String]] = {
    val sourceManaged = for {
      fileStream <- Managed.fromAutoCloseable(Task.effect(getClass.getClassLoader.getResourceAsStream("prefixesOverrides.json")))
      source <- Managed.fromAutoCloseable(Task.effect(Source.fromInputStream(fileStream)))
    } yield source
    for {
      prefixesStr <- sourceManaged.use(source => ZIO.effect(source.getLines().mkString))
      prefixesJson <- Task.effect(io.circe.parser.parse(prefixesStr).getOrElse(Json.Null))
      mappings <- ZIO.fromEither(prefixesJson.as[Map[String, String]])
    } yield mappings
  }

  def getBiolinkPrefixesFromURL: ZIO[HttpClient, Throwable, Map[String, String]] =
    for {
      httpClient <- HttpClient.client
      uri = uri"https://biolink.github.io/biolink-model/context.jsonld"
      request = Request[Task](Method.GET, uri).withHeaders(Accept(MediaType.application.`ld+json`))
      biolinkModelJson <- httpClient.expect[Json](request)
      contextJson <-
        ZIO.fromOption(biolinkModelJson.hcursor.downField("@context").focus).orElseFail(new Exception("failed to traverse down to context"))
      contextJsonObject <- ZIO.fromOption(contextJson.asObject).orElseFail(new Exception("failed to get json object from context"))
      firstPass = contextJsonObject.toIterable.filter(entry => entry._2.isObject && entry._2.asObject.get.contains("@id") && entry._2.asObject.get.contains("@prefix")).map(entry => {
        entry._1 -> entry._2.hcursor.downField("@id").focus.get.toString().replaceAll("\"", "")
      }).toMap
      secondPass = contextJsonObject.toIterable.filter(entry => entry._2.isString).map(entry => {
        entry._1 -> entry._2.toString().replaceAll("\"", "")
      }).toMap
      map = firstPass ++ secondPass
    } yield map

  def getBiolinkPrefixesFromFile: ZIO[Any, Throwable, Map[String, String]] = {
    val sourceManaged = for {
      fileStream <- Managed.fromAutoCloseable(Task.effect(getClass.getClassLoader.getResourceAsStream("prefixes.json")))
      source <- Managed.fromAutoCloseable(Task.effect(Source.fromInputStream(fileStream)))
    } yield source
    for {
      prefixesStr <- sourceManaged.use(source => ZIO.effect(source.getLines().mkString))
      prefixesJson <- Task.effect(io.circe.parser.parse(prefixesStr).getOrElse(Json.Null))
      curies <- ZIO.fromEither(prefixesJson.as[Map[String, String]])
    } yield curies
  }

  def localPrefixes: ZIO[Any, Throwable, Map[String, String]] = {
    val sourceManaged = for {
      fileStream <- Managed.fromAutoCloseable(Task.effect(getClass.getClassLoader.getResourceAsStream("legacy_prefixes.json")))
      source <- Managed.fromAutoCloseable(Task.effect(Source.fromInputStream(fileStream)))
    } yield source
    for {
      prefixesStr <- sourceManaged.use(source => ZIO.effect(source.getLines().mkString))
      prefixesJson <- ZIO.fromEither(io.circe.parser.parse(prefixesStr))
      prefixes <- ZIO.fromEither(prefixesJson.as[Map[String, String]])
    } yield prefixes
  }

  // originates from https://biolink.github.io/biolink-model/biolink-model.yaml
  def getBiolinkPrefixesAndClassesAndPredicatesFromFile
    : ZIO[Any, Throwable, (Map[String, String], List[BiolinkClass], List[BiolinkPredicate])] = {
    val sourceManaged = for {
      source <-
        Managed.fromAutoCloseable(Task.effect(Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("biolink-model.yaml"))))
    } yield source
    for {
      slotsStr <- sourceManaged.use(source => ZIO.effect(source.mkString))
      json <- ZIO.fromEither(io.circe.yaml.parser.parse(slotsStr))
      classesKeys <- ZIO.fromOption(json.hcursor.downField("classes").keys).orElseFail(throw new Exception("couldn't get classes"))
      classes = classesKeys.map(a => a.split(" ").toList.map(a => StringUtils.capitalize(a)).mkString).map(a => BiolinkClass(a)).toList
      predicateKeys <- ZIO.fromOption(json.hcursor.downField("slots").keys).orElseFail(throw new Exception("couldn't get slots"))
      predicates = predicateKeys.map(a => BiolinkPredicate(a.replaceAll(",", "_").replaceAll(" ", "_"))).toList
      prefixes <- ZIO.fromOption(json.hcursor.downField("prefixes").focus).orElseFail(throw new Exception("couldn't get prefixes"))
      prefixesMap <- ZIO.fromEither(prefixes.as[Map[String, String]])
    } yield (prefixesMap, classes, predicates)
  }

}

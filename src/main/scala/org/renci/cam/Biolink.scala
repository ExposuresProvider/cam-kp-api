package org.renci.cam

import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.generic.auto._
import org.apache.commons.lang3.StringUtils
import org.http4s.headers.Accept
import org.http4s.implicits._
import org.http4s.{MediaType, Method, Request}
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate}
import zio._
import zio.blocking.{effectBlockingIO, Blocking}
import zio.interop.catz._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.io.Source

object Biolink extends LazyLogging {

  final case class BiolinkData(version: String,
                               prefixes: Map[String, String],
                               classes: List[BiolinkClass],
                               predicates: List[BiolinkPredicate])

  def makeUtilitiesLayer: ZLayer[HttpClient, Throwable, Has[BiolinkData]] = getBiolinkData.toLayer

  val biolinkData: URIO[Has[BiolinkData], BiolinkData] = ZIO.service

  def getBiolinkData: ZIO[HttpClient, Throwable, BiolinkData] = {
    val sourceManaged = for {
      fileStream <- Managed.fromAutoCloseable(Task.effect(getClass.getClassLoader.getResourceAsStream("biolink-data.json")))
      source <- Managed.fromAutoCloseable(Task.effect(Source.fromInputStream(fileStream)))
    } yield source

    implicit val biolinkClassDecoder: Decoder[BiolinkClass] = Decoder.decodeString.map(s => BiolinkClass(s))
    implicit val biolinkPredicateDecoder: Decoder[BiolinkPredicate] = Decoder.decodeString.map(s => BiolinkPredicate(s))

    for {
      biolinkDataString <- sourceManaged.use(source => ZIO.effect(source.getLines().mkString))
      biolinkDataJson <- Task.effect(io.circe.parser.parse(biolinkDataString).getOrElse(Json.Null))
      biolinkData <- ZIO.fromEither(biolinkDataJson.as[BiolinkData])
    } yield biolinkData
  }

  def getPrefixOverrides: ZIO[Any, Throwable, Map[String, String]] = {
    val sourceManaged = for {
      fileStream <- Managed.fromAutoCloseable(Task.effect(getClass.getClassLoader.getResourceAsStream("prefixes.json")))
      source <- Managed.fromAutoCloseable(Task.effect(Source.fromInputStream(fileStream)))
    } yield source
    for {
      prefixesStr <- sourceManaged.use(source => ZIO.effect(source.getLines().mkString))
      prefixesJson <- Task.effect(io.circe.parser.parse(prefixesStr).getOrElse(Json.Null))
      mappings <- ZIO.fromEither(prefixesJson.as[Map[String, String]])
    } yield mappings
  }

  def downloadBiolinkContextJsonLD: ZIO[HttpClient, Throwable, Unit] =
    for {
      httpClient <- HttpClient.client
      uri = uri"https://biolink.github.io/biolink-model/context.jsonld"
      request = Request[Task](Method.GET, uri).withHeaders(Accept(MediaType.application.`ld+json`))
      response <- httpClient.expect[String](request)
      _ = Files.writeString(Paths.get("src/main/resources/context.jsonld"), response)
    } yield ()

  def parseBiolinkContext: ZIO[Blocking, Throwable, Map[String, String]] =
    for {
      contextData <- effectBlockingIO(
        Source.fromInputStream(getClass.getResourceAsStream("/context.jsonld"), StandardCharsets.UTF_8.name()))
        .bracketAuto { source =>
          effectBlockingIO(source.getLines().mkString("\n"))
        }
      biolinkContextJson <- ZIO.fromEither(io.circe.parser.parse(contextData))
      contextJson <-
        ZIO
          .fromOption(biolinkContextJson.hcursor.downField("@context").focus)
          .orElseFail(new Exception("failed to traverse down to context"))
      contextJsonObject <- ZIO.fromOption(contextJson.asObject).orElseFail(new Exception("failed to get json object from context"))
      firstPass = contextJsonObject.toIterable
        .filter(entry => entry._2.isObject && entry._2.asObject.get.contains("@id") && entry._2.asObject.get.contains("@prefix"))
        .map { entry =>
          entry._1 -> entry._2.hcursor.downField("@id").focus.get.toString().replaceAll("\"", "")
        }
        .toMap
      secondPass = contextJsonObject.toIterable
        .filter(entry => entry._2.isString && !entry._1.equals("@vocab") && !entry._1.equals("id"))
        .map { entry =>
          entry._1 -> entry._2.toString().replaceAll("\"", "")
        }
        .toMap
      map = firstPass ++ secondPass
    } yield map

  def downloadBiolinkModelYaml: ZIO[HttpClient, Throwable, Unit] =
    for {
      httpClient <- HttpClient.client
      uri = uri"https://biolink.github.io/biolink-model/biolink-model.yaml"
      request = Request[Task](Method.GET, uri).withHeaders(Accept(MediaType.application.`raml+yaml`))
      response <- httpClient.expect[String](request)
      _ = Files.writeString(Paths.get("src/main/resources/biolink-model.yaml"), response)
    } yield ()

  def parseBiolinkModelYaml: ZIO[Blocking, Throwable, (Map[String, String], List[BiolinkClass], List[BiolinkPredicate], String)] =
    for {
      biolinkYamlData <- effectBlockingIO(
        Source.fromInputStream(getClass.getResourceAsStream("/biolink-model.yaml"), StandardCharsets.UTF_8.name()))
        .bracketAuto { source =>
          effectBlockingIO(source.getLines().mkString("\n"))
        }
      json <- ZIO.fromEither(io.circe.yaml.parser.parse(biolinkYamlData))
      classesKeys <- ZIO.fromOption(json.hcursor.downField("classes").keys).orElseFail(throw new Exception("couldn't get classes"))
      classes = classesKeys.map(a => a.split(" ").toList.map(a => StringUtils.capitalize(a)).mkString).map(a => BiolinkClass(a)).toList
      predicateKeys <- ZIO.fromOption(json.hcursor.downField("slots").keys).orElseFail(throw new Exception("couldn't get slots"))
      predicates = predicateKeys.map(a => BiolinkPredicate(a.replaceAll(",", "_").replaceAll(" ", "_"))).toList
      prefixes <- ZIO.fromOption(json.hcursor.downField("prefixes").focus).orElseFail(throw new Exception("couldn't get prefixes"))
      prefixesMap <- ZIO.fromEither(prefixes.as[Map[String, String]])
      versionJson <- ZIO.fromOption(json.hcursor.downField("version").focus).orElseFail(throw new Exception("couldn't get version"))
      version <- ZIO.fromEither(versionJson.as[String])
    } yield (prefixesMap, classes, predicates, version)

}

package org.renci.cam.util

import com.typesafe.scalalogging.{LazyLogging, Logger}
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import org.apache.commons.lang3.StringUtils
import org.http4s.headers.Accept
import org.http4s.implicits._
import org.http4s.{MediaType, Method, Request}
import org.renci.cam.Biolink.BiolinkData
import org.renci.cam.HttpClient
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate}
import zio.blocking.Blocking
import zio.interop.catz._
import zio.{ExitCode, Task, URIO, ZIO, ZLayer}

import java.nio.file.{Files, Paths}
import scala.collection.immutable.ListMap

object UpdateBiolinkResources extends zio.App with LazyLogging {

  override lazy val logger = Logger(UpdateBiolinkResources.getClass.getSimpleName);

  val layer: ZLayer[Any, Throwable, Blocking with HttpClient] = Blocking.live ++ HttpClient.makeHttpClientLayer

  def downloadBiolinkContextJsonLD: ZIO[HttpClient, Throwable, Unit] =
    for {
      httpClient <- HttpClient.client
      uri = uri"https://biolink.github.io/biolink-model/context.jsonld"
      request = Request[Task](Method.GET, uri).withHeaders(Accept(MediaType.application.`ld+json`))
      response <- httpClient.expect[String](request)
      _ = Files.writeString(Paths.get("src/main/resources/context.jsonld"), response)
      _ = logger.info("downloaded context.jsonld")
    } yield ()

  def parseBiolinkContext: ZIO[Blocking, Throwable, Map[String, String]] =
    for {
      contextData <- ZIO.effectTotal(Files.readString(Paths.get("src/main/resources/context.jsonld")))
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
      _ = logger.info("parsed context.jsonld")
    } yield map

  def downloadBiolinkModelYaml: ZIO[HttpClient, Throwable, Unit] =
    for {
      httpClient <- HttpClient.client
      uri = uri"https://biolink.github.io/biolink-model/biolink-model.yaml"
      request = Request[Task](Method.GET, uri).withHeaders(Accept(MediaType.application.`raml+yaml`))
      response <- httpClient.expect[String](request)
      _ = Files.writeString(Paths.get("src/main/resources/biolink-model.yaml"), response)
      _ = logger.info("downloaded biolink-model.yaml")
    } yield ()

  def parseBiolinkModelYaml: ZIO[Blocking, Throwable, (Map[String, String], List[BiolinkClass], List[BiolinkPredicate], String)] =
    for {
      biolinkYamlData <- ZIO.effectTotal(Files.readString(Paths.get("src/main/resources/biolink-model.yaml")))
      json <- ZIO.fromEither(io.circe.yaml.parser.parse(biolinkYamlData))
      classesKeys <- ZIO.fromOption(json.hcursor.downField("classes").keys).orElseFail(throw new Exception("couldn't get classes"))
      classes = classesKeys.map(a => a.split(" ").toList.map(a => StringUtils.capitalize(a)).mkString).map(a => BiolinkClass(a)).toList
      predicateKeys <- ZIO.fromOption(json.hcursor.downField("slots").keys).orElseFail(throw new Exception("couldn't get slots"))
      predicates = predicateKeys.map(a => BiolinkPredicate(a.replaceAll(",", "_").replaceAll(" ", "_"))).toList
      prefixes <- ZIO.fromOption(json.hcursor.downField("prefixes").focus).orElseFail(throw new Exception("couldn't get prefixes"))
      prefixesMap <- ZIO.fromEither(prefixes.as[Map[String, String]])
      versionJson <- ZIO.fromOption(json.hcursor.downField("version").focus).orElseFail(throw new Exception("couldn't get version"))
      version <- ZIO.fromEither(versionJson.as[String])
      _ = logger.info("parsed biolink-model.yaml")
    } yield (prefixesMap, classes, predicates, version)

  def getPrefixOverrides: ZIO[Any, Throwable, Map[String, String]] =
    for {
      prefixesStr <- ZIO.effectTotal(Files.readString(Paths.get("src/main/resources/prefixes.json")))
      _ = logger.info("read prefixes.json")
      prefixesJson <- Task.effect(io.circe.parser.parse(prefixesStr).getOrElse(Json.Null))
      mappings <- ZIO.fromEither(prefixesJson.as[Map[String, String]])
    } yield mappings

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val program = for {
      _ <- downloadBiolinkContextJsonLD
      _ <- downloadBiolinkModelYaml
      (biolinkPrefixes, classes, predicates, version) <- parseBiolinkModelYaml
      prefixes <- parseBiolinkContext
      prefixOverrides <- getPrefixOverrides
      combined_prefixes = biolinkPrefixes ++ prefixes ++ prefixOverrides
      sorted_combined_prefixes = ListMap(combined_prefixes.toSeq.sortBy(_._1): _*)
      biolinkData = BiolinkData(version, sorted_combined_prefixes, classes.sortBy(_.shorthand), predicates.sortBy(_.shorthand))
      biolinkDataJson = {
        implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap { s =>
          s.shorthand
        }
        implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Encoder.encodeString.contramap { s =>
          s.shorthand
        }
        biolinkData.asJson.deepDropNullValues.noSpaces
      }
      _ = Files.writeString(Paths.get("src/main/resources/biolink-data.json"), biolinkDataJson)
      _ = logger.info("wrote biolink-data.json")
    } yield ()

    program.provideSomeLayer(layer).exitCode

  }

}

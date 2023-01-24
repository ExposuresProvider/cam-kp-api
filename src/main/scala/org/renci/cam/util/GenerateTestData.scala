package org.renci.cam.util

import com.typesafe.scalalogging.{LazyLogging, Logger}
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import org.apache.commons.csv.CSVFormat
import org.http4s.implicits._
import org.renci.cam._
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, TRAPIQueryEdge, TRAPIQueryGraph, TRAPIQueryNode}
import zio._
import zio.blocking.{Blocking, effectBlockingIO}
import zio.config.typesafe.TypesafeConfig
import zio.config.{ZConfig, getConfig}
import zio.interop.catz._
import zio.interop.catz.implicits._
import zio.stream.ZStream

import java.io.StringReader
import java.nio.charset.StandardCharsets
import scala.io.Source
import scala.jdk.javaapi.CollectionConverters

/** The <a href="https://github.com/TranslatorSRI/SRI_testing">SRI Testing harness</a> requires test data to be generated. This utility is
  * designed to generate some test data. It uses the SPARQL endpoint configured in AppConfig as well as the files in src/main/resources to
  * generate this data.
  */
object GenerateTestData extends zio.App with LazyLogging {

  override lazy val logger = Logger(GenerateTestData.getClass.getSimpleName);

  /*

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

   */

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val program = for {
      conf <- getConfig[AppConfig]

      // Get a list of all the predicates in predicates.csv.
      predicates <- effectBlockingIO(Source.fromInputStream(getClass.getResourceAsStream("/predicates.csv"), StandardCharsets.UTF_8.name()))
        .bracketAuto { source =>
          effectBlockingIO(source.getLines().mkString("\n"))
        }
      predicateRecords <- Task.effect(CSVFormat.DEFAULT.parse(new StringReader(predicates)).getRecords)

      // Generate a test record for each record.
      results <- ZStream
        .fromIterable(CollectionConverters.asScala(predicateRecords))
        .flatMap(predicateRecord =>
          ZStream.fromEffect(
            QueryService.run(
              100,
              TRAPIQueryGraph(
                Map(
                  "n0" -> TRAPIQueryNode(None, Some(List(BiolinkClass(predicateRecord.get(0)))), None),
                  "n1" -> TRAPIQueryNode(None, Some(List(BiolinkClass(predicateRecord.get(2)))), None)
                ),
                Map(
                  "e0" -> TRAPIQueryEdge(Some(List(BiolinkPredicate("related_to"))), "n0", "n1", None)
                )
              )
            )
          ))
        .tap { resp =>
          logger.info(f"Found TRAPI Response: " + resp)
          ZIO.succeed(resp)
        }
        .runCollect
    } yield ({
      logger.info(f"Found result: " + results)
    })

    val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
    val camkpapiLayer = Blocking.live >>> HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer

    program.provideCustomLayer(configLayer ++ camkpapiLayer >+> SPARQLQueryExecutor.makeCache.toLayer).exitCode
  }

}

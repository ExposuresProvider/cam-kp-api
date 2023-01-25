package org.renci.cam.util

import com.typesafe.scalalogging.{LazyLogging, Logger}
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import org.apache.commons.csv.CSVFormat
import org.http4s.implicits._
import org.phenoscape.sparql.SPARQLInterpolation._
import org.renci.cam.Biolink.biolinkData
import org.renci.cam.QueryService.RDFSSubClassOf
import org.renci.cam._
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, IRI, TRAPIKnowledgeGraph, TRAPIMessage, TRAPIQueryEdge, TRAPIQueryGraph, TRAPIQueryNode, TRAPIResponse}
import zio._
import zio.blocking.{effectBlockingIO, Blocking}
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.interop.catz._
import zio.interop.catz.implicits._
import zio.stream.{ZSink, ZStream}

import java.io.{File, StringReader}
import java.nio.charset.StandardCharsets
import scala.io.Source
import scala.jdk.javaapi.CollectionConverters

/** The <a href="https://github.com/TranslatorSRI/SRI_testing">SRI Testing harness</a> requires test data to be generated. This utility is
  * designed to generate some test data. It uses the SPARQL endpoint configured in AppConfig as well as the files in src/main/resources to
  * generate this data.
  */
object GenerateTestData extends zio.App with LazyLogging {

  val LinkMLClassDefinition = IRI("https://w3id.org/linkml/ClassDefinition")

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

  case class ParentChildRelation(
    parent: String,
    child: String
  )

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    if (args.length > 1) {
      throw new RuntimeException("Only a single argument is allowed: the test-edges JSONL file to write.")
    }

    val testEdgesFile = if (args.length == 1) args(1) else "src/main/resources/test-edges.jsonl"

    case class TestEdge(
      subject_category: String,
      object_category: String,
      predicate: String,
      subject: String,
      `object`: String
    )

    val program = for {
      biolinkData <- biolinkData

      // Get a list of all the Biolink parent-child relationships we know about.
      parent_child_query =
        sparql"""SELECT ?parent ?child WHERE {
            ?child ${RDFSSubClassOf} ?parent .
            FILTER(?child != ?parent)

            ?child a ${LinkMLClassDefinition} .
            ?parent a ${LinkMLClassDefinition} .
          }
          """
      parent_child_relations_solutions <- SPARQLQueryExecutor.runSelectQuery(parent_child_query.toQuery)
      parent_child_relations = parent_child_relations_solutions
        .map { qs =>
          (
            qs.getResource("parent").getURI.replaceFirst("https://w3id.org/biolink/vocab/", "biolink:"),
            qs.getResource("child").getURI.replaceFirst("https://w3id.org/biolink/vocab/", "biolink:")
          )
        }
        .groupMap(_._1)(_._2)
      _ = logger.info(f"Parent-child relations: ${parent_child_relations}")

      // Get a list of all the predicates in predicates.csv.
      predicates <- effectBlockingIO(Source.fromInputStream(getClass.getResourceAsStream("/predicates.csv"), StandardCharsets.UTF_8.name()))
        .bracketAuto { source =>
          effectBlockingIO(source.getLines().mkString("\n"))
        }
      predicateRecords <- Task.effect(CollectionConverters.asScala(CSVFormat.DEFAULT.parse(new StringReader(predicates)).getRecords))

      // Generate a test record for each record.
      results <- ZStream
        .fromIterable(predicateRecords)
        .filter { predicateRecord =>
          val subj = s"biolink:${predicateRecord.get(0)}"
          val obj = s"biolink:${predicateRecord.get(2)}"

          if (parent_child_relations.contains(subj)) {
            logger.info(f"Skipping ${predicateRecord}, as subject has children: ${parent_child_relations.getOrElse(subj, List())}")
            false
          } else if (parent_child_relations.contains(obj)) {
            logger.info(f"Skipping ${predicateRecord}, as object has children: ${parent_child_relations.getOrElse(obj, List())}")
            false
          } else true
        }
        .flatMap { predicateRecord =>
          val subj = s"biolink:${predicateRecord.get(0)}"
          val obj = s"biolink:${predicateRecord.get(2)}"

          ZStream
            .fromEffect(
              QueryService.run(
                1,
                TRAPIQueryGraph(
                  Map(
                    "n0" -> TRAPIQueryNode(None, Some(List(BiolinkClass(subj))), None),
                    "n1" -> TRAPIQueryNode(None, Some(List(BiolinkClass(obj))), None)
                  ),
                  Map(
                    "e0" -> TRAPIQueryEdge(Some(List(BiolinkPredicate("related_to"))), "n0", "n1", None)
                  )
                )
              )
            )
            .mapError { err =>
              val errMsg = f"Caught error while querying ${predicateRecord}: ${err}"
              logger.error(errMsg)
              TRAPIResponse(TRAPIMessage(None, None, None), Some("Error"), Some(errMsg), None)
            }
            .tap { resp =>
              logger.info(f"TRAPI Response: ${resp}")

              val results = resp.message.results.getOrElse(List())
              logger.info(f"TRAPI results contains ${results.size}: ${results}")

              ZIO.succeed(resp)
            }
            .flatMap(resp =>
              ZStream.fromIterable(
                resp.message.knowledge_graph.getOrElse(TRAPIKnowledgeGraph(Map(), Map())).edges.values.map { edge =>
                  TestEdge(
                    subj,
                    obj,
                    Implicits.compactIRIIfPossible(edge.predicate.getOrElse(BiolinkPredicate("example:error")).iri, biolinkData.prefixes),
                    Implicits.compactIRIIfPossible(edge.subject, biolinkData.prefixes),
                    Implicits.compactIRIIfPossible(edge.`object`, biolinkData.prefixes)
                  )
                }
              ))
            .tap { edge =>
              logger.info(f"TestEdge found: ${edge}")
              ZIO.succeed(edge)
            }
        }
        .map { edge =>
          val edgeAsStr = edge.asJson.noSpacesSortKeys

          logger.info(f"Converted test edge to JSON: ${edgeAsStr}")

          edgeAsStr
        }
        .intersperse("\n")
        .run(ZSink
          .fromFile(new File(testEdgesFile).toPath)
          .contramapChunks[String](_.flatMap(_.getBytes)))
    } yield ({
      logger.info(f"Found result: " + results)
    })

    val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
    val camkpapiLayer = Blocking.live >>> HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer

    program.provideCustomLayer(configLayer ++ camkpapiLayer >+> SPARQLQueryExecutor.makeCache.toLayer).exitCode
  }

}

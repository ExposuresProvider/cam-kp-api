package org.renci.cam.util

import com.typesafe.scalalogging.{LazyLogging, Logger}
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.implicits._
import org.phenoscape.sparql.SPARQLInterpolation._
import org.renci.cam.Biolink.biolinkData
import org.renci.cam.QueryService.{BiolinkNamedThing, RDFSSubClassOf, SesameDirectType}
import org.renci.cam._
import org.renci.cam.domain.{BiolinkPredicate, IRI, PredicateMappings}
import zio._
import zio.blocking.Blocking
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.interop.catz._
import zio.interop.catz.implicits._
import zio.stream.{ZSink, ZStream}

import java.nio.file.Path

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

  case class TestEdge(
    subject_category: String,
    object_category: String,
    predicate: String,
    subject: String,
    `object`: String
  )

  case class SRITestingFile(
    source_type: String,
    infores: String,
    exclude_tests: List[String],
    edges: List[TestEdge]
  )

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    if (args.isEmpty) {
      throw new RuntimeException("A single argument is required: the test-edges JSONL file to write.")
    }
    if (args.length > 1) {
      throw new RuntimeException("Only a single argument is allowed: the test-edges JSONL file to write.")
    }

    val jsonl_file = Path.of(args.head)

    val program = for {
      biolinkData <- biolinkData

      // Get a list of all the Biolink classes (and their parents) that we know about.
      biolink_classes_query = sparql"""
      SELECT ?biolinkClass (GROUP_CONCAT(DISTINCT ?parentClass; SEPARATOR="|") AS ?parents) WHERE {
        ?biolinkClass ${RDFSSubClassOf} ${BiolinkNamedThing.iri} .
        ?biolinkClass a ${LinkMLClassDefinition} .

        OPTIONAL {
          ?biolinkClass ${RDFSSubClassOf} ?parentClass .
          FILTER(?biolinkClass != ?parentClass) .
          ?parentClass a ${LinkMLClassDefinition}
        }
      } GROUP BY ?biolinkClass"""

      biolink_classes_solutions <- SPARQLQueryExecutor.runSelectQuery(biolink_classes_query.toQuery)
      biolink_classes_parents = biolink_classes_solutions
        .map { qs =>
          (
            qs.getResource("biolinkClass").getURI,
            qs.getLiteral("parents").getString.split('|').toSet
          )
        }
        .groupMapReduce(_._1)(_._2)(_ ++ _)
      _ = logger.info(f"Biolink classes with parents: ${biolink_classes_parents}")

      // Invert this map so we get a Map[String,
      biolink_classes_children =
        biolink_classes_parents
          .flatMap { case (concept, parents) =>
            parents.map { parent =>
              (parent, concept)
            }
          }
          .groupMap(_._1)(_._2)
          .view
          .mapValues(_.toSet)
          .toMap

      // Generate a list of biolink class - biolink class pairs.
      biolink_classes_pairs = for {
        x <- biolink_classes_parents.keySet
        y <- biolink_classes_parents.keySet
      } yield (x, y)

      // All our relations should be part of biolink:related_to.
      relationsSet = PredicateMappings.mapQueryEdgePredicates(Some(List(BiolinkPredicate("related_to"))), None)

      // Generate test records for each Biolink pair.
      results <- ZStream
        .fromIterable(biolink_classes_pairs)
        .filter { biolinkPair =>
          val subj = (biolinkPair._1)
          val obj = (biolinkPair._2)

          // For generating this test data, we only want the leaf nodes. So any class with
          // children should be eliminated here.
          val subj_is_leaf = biolink_classes_children.getOrElse(subj, Set()).isEmpty
          val obj_is_leaf = biolink_classes_children.getOrElse(subj, Set()).isEmpty

          if (subj_is_leaf && obj_is_leaf) true
          else {
            logger.info(s"Skipping ${subj} (Is leaf? ${subj_is_leaf}) -> ${obj} (Is leaf? ${obj_is_leaf})")
            false
          }
        }
        .mapM { biolinkPair =>
          val subj = IRI(biolinkPair._1)
          val obj = IRI(biolinkPair._2)

          if (relationsSet.isEmpty) {
            throw new RuntimeException("Biolink predicate `biolink:related_to` could not be mapped to any Biolink predicates.")
          }

          // Skip any pairs where the subj or obj contain more than one hash, because this causes Jena to error.
          if (subj.value.matches(".*#.*#.*") || obj.value.matches(".*#.*#.*")) {
            logger.warn(s"Skipping pair $subj and $obj as one of them contains more than one '#' character.")
          }

          val subjEntitiesQuery =
            sparql"""
              SELECT ?a {
                ?aClass <http://www.w3.org/2000/01/rdf-schema#subClassOf> ${subj} .
                ?a <http://www.openrdf.org/schema/sesame#directType> ?aClass .
              } LIMIT 1000
            """

          val objEntitiesQuery =
            sparql"""
              SELECT ?b {
                ?bClass <http://www.w3.org/2000/01/rdf-schema#subClassOf> ${obj} .
                ?b <http://www.openrdf.org/schema/sesame#directType> ?bClass .
              } LIMIT 1000
            """

          /*

          val relationsAsSPARQL = relationsSet.map(n => sparql" $n ").fold(sparql"")(_ + _)
          val relationsQuery =
            sparql"""
            SELECT ?relation WHERE {
              {
                SELECT ?aClass {
                  ?aClass ${RDFSSubClassOf} ${subj}
                } LIMIT 1000
              }
              {
                SELECT ?bClass {
                  ?bClass ${RDFSSubClassOf} ${obj}
                } LIMIT 1000
              }
              ?a ${SesameDirectType} ?aClass .
              ?b ${SesameDirectType} ?bClass .
              ?a ?relation ?b .
            } LIMIT 1000"""

          logger.info(s"Querying database for relations between ${subj} and ${obj} with: ${relationsQuery}")

           */

          val edges = for {
            subjEntitiesResult <- SPARQLQueryExecutor.runSelectQuery(subjEntitiesQuery.toQuery).orElse(ZIO(Seq()))
            subjEntities = subjEntitiesResult
              .map(qs => qs.getResource("a").getURI)
              // We have IRIs with multiple '#'s, which causes a Jena exception later. Let's filter those out.
              .filterNot(_.matches(".*#.*#.*"))
            _ = logger.info(s"Found ${subjEntities.length} subject entities for ${subj}: ${subjEntities}")
            objEntitiesResult <- SPARQLQueryExecutor.runSelectQuery(objEntitiesQuery.toQuery).orElse(ZIO(Seq()))
            objEntities = objEntitiesResult
              .map(qs => qs.getResource("b").getURI)
              // We have IRIs with multiple '#'s, which causes a Jena exception later. Let's filter those out.
              .filterNot(_.matches(".*#.*#.*"))
            _ = logger.info(s"Found ${objEntities.length} object entities for ${obj}: ${objEntities}")
            relationsQuery =
              sparql"""
                SELECT DISTINCT ?relation WHERE {
                  VALUES ?a { ${subjEntities.map(IRI(_)).map(n => sparql" $n ").fold(sparql"")(_ + _)} } .
                  VALUES ?b { ${objEntities.map(IRI(_)).map(n => sparql" $n ").fold(sparql"")(_ + _)} } .
                  FILTER (?a != ?b) .
                  ?a ?relation ?b
                }
              """
            _ = logger.info(s"Prepared relations query: ${relationsQuery.toQuery}")
            relationsResult <- SPARQLQueryExecutor.runSelectQuery(relationsQuery.toQuery).orElse(ZIO(Seq()))
            _ = logger.info(s"Relations result obtained: ${relationsResult}")
            relations = relationsResult.map(qs => qs.getResource("relation").getURI)
            _ = {
              if (relations.isEmpty) {
                logger.warn(f"Did not find any relations from ${subj} to ${obj}: ${relations}")
              } else {
                logger.info(f"Found relations: ${relations.size} from ${subj} to ${obj}: ${relations}")
              }
            }
            testEdges <- ZStream
              .fromIterable(relations)
              .mapM { relation =>
                val relationIRI = IRI(relation)
                val singleTestRecordQuery =
                  sparql"""
                    SELECT ?aClass ?bClass WHERE {
                      ?aClass ${RDFSSubClassOf} ${subj} .
                      ?bClass ${RDFSSubClassOf} ${obj} .
                      ?a ${SesameDirectType} ?aClass .
                      ?b ${SesameDirectType} ?bClass .
                      ?a ${relationIRI} ?b .
                    } LIMIT 1"""

                for {
                  singleTestRecordResult <- SPARQLQueryExecutor.runSelectQuery(singleTestRecordQuery.toQuery)
                  singleTestRecords = singleTestRecordResult.map(qs => (qs.getResource("aClass").getURI, qs.getResource("bClass").getURI))
                  testEdges = singleTestRecords.map(singleTestRecord =>
                    TestEdge(subj.value, obj.value, relation, singleTestRecord._1, singleTestRecord._2))
                  _ = logger.info(
                    f"- Found ${testEdges.length} test edges for ${subj.value} --${relation}--> ${obj.value}: ${testEdges.take(100)}")
                } yield testEdges
              }
              .runCollect
          } yield testEdges

          edges.map(r => r.toList.flatten)
        }
        .mapError { err =>
          val errMsg = f"Caught error in main loop: ${err}"
          logger.error(errMsg)
        }
        .flatMap(value => ZStream.fromIterable(value.map(v => v.asJson.deepDropNullValues.noSpacesSortKeys)))
        .intersperse("\n")
        .run(ZSink.fromFile(jsonl_file).contramapChunks[String](_.flatMap(_.getBytes)))

      /*
      sriTestingFile = SRITestingFile("aggregator", "cam-kp", List(), results.toList.flatten.distinct)
      sriTestingFileJson = sriTestingFile.asJson
      _ = Files.writeString(sriTestingFilePath, sriTestingFileJson.deepDropNullValues.spaces2SortKeys)

       */
    } yield ({
      logger.info(f"Found result: " + results)
    })

    val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
    val camkpapiLayer = Blocking.live >>> HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer

    program.provideCustomLayer(configLayer ++ camkpapiLayer >+> SPARQLQueryExecutor.makeCache.toLayer).exitCode
  }

}

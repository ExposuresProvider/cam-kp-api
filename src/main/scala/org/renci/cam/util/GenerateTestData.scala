package org.renci.cam.util

import com.typesafe.scalalogging.{LazyLogging, Logger}
import io.circe.generic.auto._
import io.circe.syntax._
import org.phenoscape.sparql.SPARQLInterpolation._
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.QueryService.{BiolinkNamedThing, RDFSSubClassOf, SesameDirectType}
import org.renci.cam._
import org.renci.cam.domain.{BiolinkPredicate, IRI, PredicateMappings}
import zio._
import zio.blocking.Blocking
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.stream.{ZSink, ZStream}

import java.nio.file.Path

/** The <a href="https://github.com/TranslatorSRI/SRI_testing">SRI Testing harness</a> requires test data to be generated. This utility is
  * designed to generate some test data. It uses the SPARQL endpoint configured in AppConfig as well as the files in src/main/resources to
  * generate this data.
  */
object GenerateTestData extends zio.App with LazyLogging {
  /* Helper classes. */

  /** Model a parent-to-child relationship between two Biolink classes. For example, biolink:MolecularEntity is a child of
    * biolink:ChemicalEntity.
    */
  case class ParentChildRelation(
    parent: String,
    child: String
  )

  /** A single test edge to be generated in the output JSONL file, as documented at
    * https://github.com/TranslatorSRI/SRI_testing/blob/83358cb18bdebd5a45b8f4c4671cb027328df700/tests/onehop/README.md#biolink-3-revisions
    * @param subject_category
    *   The Biolink class of the subject (e.g. `biolink:SmallMolecule`).
    * @param object_category
    *   The Biolink class of the object (e.g. `biolink:Disease`).
    * @param predicate
    *   The Biolink predicate of the relation (e.g. `biolink:treats`).
    * @param subject_id
    *   The identifier of the subject (e.g. `CHEBI:3002`).
    * @param object_id
    *   The identifier of the object (e.g. `MESH:D001249`).
    */
  case class TestEdge(
    subject_category: String,
    object_category: String,
    predicate: String,
    subject_id: String,
    object_id: String,
    qualifiers: List[domain.TRAPIQualifier] = List()
  )

  /** The structure of the final JSON file to generate. */
  case class SRITestingFile(
    source_type: String,
    infores: String,
    exclude_tests: List[String],
    edges: List[TestEdge]
  )

  /* Object variables. */
  val LinkMLClassDefinition: IRI = IRI("https://w3id.org/linkml/ClassDefinition")
  override lazy val logger: Logger = Logger(GenerateTestData.getClass.getSimpleName);

  private def generateJSONLFile(jsonlPath: Path): ZIO[Blocking with ZConfig[AppConfig] with HttpClient, Throwable, Long] = {
    // Get a list of all the Biolink classes (and their parents) that we know about.
    val biolink_classes_query =
      sparql"""
    SELECT ?biolinkClass (GROUP_CONCAT(DISTINCT ?parentClass; SEPARATOR="|") AS ?parents) WHERE {
      ?biolinkClass ${RDFSSubClassOf} ${BiolinkNamedThing.iri} .
      ?biolinkClass a ${LinkMLClassDefinition} .

      OPTIONAL {
        ?biolinkClass ${RDFSSubClassOf} ?parentClass .
        FILTER(?biolinkClass != ?parentClass) .
        ?parentClass a ${LinkMLClassDefinition}
      }
    } GROUP BY ?biolinkClass"""

    for {
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
          val obj_is_leaf = biolink_classes_children.getOrElse(obj, Set()).isEmpty

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
            ZIO.succeed(Seq())
          } else {
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
                    testEdges = singleTestRecords.flatMap { singleTestRecord =>
                      val qualifiedPreds = PredicateMappings.getBiolinkQualifiedPredicates(relationIRI)
                      qualifiedPreds.map { qualifiedPred =>
                        logger.info(
                          f"- Found test edge for ${subj.value} --${relation} (${qualifiedPred.biolinkPredicate.shorthand})--> ${obj.value} (predicates: ${qualifiedPred.qualifierList})"
                        )
                        TestEdge(subj.value,
                                 obj.value,
                                 "biolink:" + qualifiedPred.biolinkPredicate.shorthand,
                                 singleTestRecord._1,
                                 singleTestRecord._2,
                                 qualifiedPred.qualifierList)
                      }
                    }
                  } yield testEdges
                }
                .runCollect
            } yield testEdges

            edges.map(r => r.toList.flatten)
          }
        }
        .flatMap(value => ZStream.fromIterable(value.map(v => v.asJson.deepDropNullValues.noSpacesSortKeys)))
        .intersperse("\n")
        .run(ZSink.fromFile(jsonlPath).contramapChunks[String](_.flatMap(_.getBytes)))
    } yield results
  }

  /** Process the command line arguments and return a ZIO program that generates the test data.
    *
    * @param args
    *   The command line arguments to generate.
    * @return
    *   A ZIO program that generates an ExitCode.
    */
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    if (args.isEmpty) {
      throw new RuntimeException("A single argument is required: the test-edges JSONL file to write.")
    }
    if (args.length > 1) {
      throw new RuntimeException("Only a single argument is allowed: the test-edges JSONL file to write.")
    }

    /*
    sriTestingFile = SRITestingFile("aggregator", "cam-kp", List(), results.toList.flatten.distinct)
    sriTestingFileJson = sriTestingFile.asJson
    _ = Files.writeString(sriTestingFilePath, sriTestingFileJson.deepDropNullValues.spaces2SortKeys)

     */

    val jsonlPath = Path.of(args.head)
    val program = for {
      // Generate JSONL file.
      linesGenerated <- generateJSONLFile(jsonlPath)
        .onError { cause =>
          logger.error(s"Could not generate JSONL file ${jsonlPath}: $cause")
          ZIO.succeed(ExitCode.failure)
        }
    } yield {
      logger.info(s"Successfully wrote ${linesGenerated} to ${jsonlPath}")
      ExitCode.success
    }

    // Set up layers and return program.
    val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
    val camkpapiLayer: ZLayer[Any, Throwable, HttpClient] = Blocking.live >>> HttpClient.makeHttpClientLayer
    val layer: ZLayer[Any, Throwable, Blocking with ZConfig[AppConfig] with HttpClient] =
      configLayer ++ camkpapiLayer ++ Blocking.live >+> SPARQLQueryExecutor.makeCache.toLayer

    program.provideCustomLayer(layer).exitCode
  }

}

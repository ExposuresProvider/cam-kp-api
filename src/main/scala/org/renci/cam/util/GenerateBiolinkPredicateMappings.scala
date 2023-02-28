package org.renci.cam.util

import com.typesafe.scalalogging.{LazyLogging, Logger}
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.phenoscape.sparql.SPARQLInterpolation.SPARQLStringContext
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.LookupService.LabeledIRI
import org.renci.cam.SPARQLQueryExecutor.SPARQLCache
import org.renci.cam.domain.{BiolinkPredicate, IRI, TRAPIQualifier, TRAPIQualifierConstraint}
import org.renci.cam.{AppConfig, HttpClient, QueryService, SPARQLQueryExecutor}
import zio._
import zio.blocking.{Blocking, blocking}
import zio.config.typesafe.TypesafeConfig
import zio.config.{ZConfig, getConfig}
import zio.stream.ZStream

import java.nio.file.{Files, Paths}
import scala.io.Source

/** This class can generate Biolink predicate mappings in the src/main/resources/biolink directory, based on two input sources:
  *   1. It queries the triplestore to download the list of all predicates in use in the triplestore, along with Biolink mappings where
  *      available.
  *
  * 2. It downloads the predicate mapping file from a specific Biolink model version, and uses that to map single predicates to
  * predicate+qualifier combinations.
  *
  * It generates a single JSON document that can be used to handle all our predicate mapping needs:
  *   1. Map a Biolink predicate with zero or more qualifiers to a set of triplestore predicates.
  *
  * 2. Map a triplestore predicate to a Biolink predicate with zero or more qualifiers.
  *
  * It also generates warnings for predicates not mapped.
  */
object GenerateBiolinkPredicateMappings extends zio.App with LazyLogging {
  override lazy val logger: Logger = Logger(GenerateBiolinkPredicateMappings.getClass.getSimpleName);

  /** Where should we save the predicates.json files? */
  val PredicateJsonFilePath = Paths.get("src/main/resources/biolink/predicates.json")

  case class PredicateMapping(
    predicate: LabeledIRI,
    biolinkPredicate: Option[BiolinkPredicate],
    biolinkQualifiers: Option[TRAPIQualifierConstraint]
  )

  /** Retrieve the list of predicates from the triplestore, along with Biolink mappings where available.
    *
    * @return
    *   A sequence of PredicateMappings from the triplestore.
    */
  def getPredicatesFromSPARQL: RIO[ZConfig[AppConfig] with HttpClient with Has[SPARQLCache], Seq[PredicateMapping]] = {
    val predicateQuery =
      sparql"""SELECT DISTINCT ?pred ?pred_label ?biolink_slot WHERE {
                 OPTIONAL { ?pred ${QueryService.RDFSLabel} ?pred_label } .
                OPTIONAL { ?pred ${QueryService.SlotMapping} ?biolink_slot }

                {
                  SELECT DISTINCT ?pred WHERE { ?s ?pred ?o }
                }
              }""".toQuery

    for {
      result <- SPARQLQueryExecutor.runSelectQuery(predicateQuery)
      preds <- ZStream
        .fromIterable(result)
        .map(
          { qs =>
            val predURI = qs.getResource("pred").getURI
            val predLabel = qs.getLiteral("pred_label").getString
            val biolinkSlot = qs.getResource("biolink_slot")

            if (biolinkSlot == null) {
              logger.warn(f"No Biolink mapping known for predicate ${predURI} (${predLabel})")
              PredicateMapping(
                LabeledIRI(predURI, Set(predLabel)),
                None,
                None
              )
            } else {
              val biolinkURI = biolinkSlot.getURI
              val shorthand = biolinkURI.replaceFirst(raw"^https://w3id.org/biolink/vocab/", "")

              PredicateMapping(
                LabeledIRI(predURI, Set(predLabel)),
                Some(BiolinkPredicate(shorthand, IRI(biolinkURI))),
                None
              )
            }
          }
        )
        .runCollect
    } yield preds
  }

  /* PREDICATE MAPPING.

    For Biolink v3, the TRAPI developers created a file of predicate mappings. We use that to expand the predicates
    currently in the

    For future versions, we will either continue to rely on that file, maintain our own copy of those mappings,
    or replace this with some other mechanism for predicate mapping.
   */

  /** A case class for predicate mappings. */
  case class PredicateMappingRow(
    `mapped predicate`: String,
    `object aspect qualifier`: Option[String],
    `object direction qualifier`: Option[String],
    predicate: String,
    `qualified predicate`: Option[String],
    `exact matches`: Option[Set[String]]
  ) {
    def biolinkPredicate: BiolinkPredicate = BiolinkPredicate(predicate.replace(' ', '_'))
    def biolinkMappedPredicate: BiolinkPredicate = BiolinkPredicate(`mapped predicate`.replace(' ', '_'))

    def qualifiers: Seq[TRAPIQualifier] = (`object aspect qualifier` match {
      case Some(aspect: String) =>
        List(TRAPIQualifier(qualifier_type_id = "biolink:object_aspect_qualifier", qualifier_value = aspect.replace(' ', '_')))
      case _ => List()
    }) ++ (`object direction qualifier` match {
      case Some(direction: String) =>
        List(TRAPIQualifier(qualifier_type_id = "biolink:object_direction_qualifier", qualifier_value = direction.replace(' ', '_')))
      case _ => List()
    }) ++ (`qualified predicate` match {
      case Some(qualified_predicate: String) =>
        List(
          TRAPIQualifier(qualifier_type_id = "biolink:qualified_predicate",
                         qualifier_value = BiolinkPredicate(qualified_predicate.replace(' ', '_')).withBiolinkPrefix)
        )
      case _ => List()
    })

    def qualifierConstraint: TRAPIQualifierConstraint = TRAPIQualifierConstraint(qualifier_set = qualifiers.toList)

    def qualifierConstraintList = List(qualifierConstraint)
  }

  case class PredicateMappings(
    `predicate mappings`: List[PredicateMappingRow]
  )

  /** To initialize this object, we need to download and parse the predicate_mapping.yaml file from the Biolink model, which needs to be
    * downloaded to the package resources (src/main/resources) from
    * https://github.com/biolink/biolink-model/blob/${biolinkVersion}/predicate_mapping.yaml (the raw version is available from
    * https://raw.githubusercontent.com/biolink/biolink-model/v3.2.1/predicate_mapping.yaml)
    */
  val getPredicateMappingsFromGitHub: RIO[ZConfig[AppConfig] with Blocking, List[PredicateMappingRow]] =
    for {
      appConfig <- getConfig[AppConfig]
      predicateMappingText <- blocking(
        Task.effect(
          Source
            .fromURL(s"https://raw.githubusercontent.com/biolink/biolink-model/${appConfig.biolinkVersion}/predicate_mapping.yaml")
            .getLines()
            .mkString("\n")
        )
      )
      predicateMappingsYaml <- ZIO.fromEither(io.circe.yaml.parser.parse(predicateMappingText))
      predicateMappings <- ZIO.fromEither(predicateMappingsYaml.as[PredicateMappings])
    } yield predicateMappings.`predicate mappings`

  /** Compares two qualifier lists. */
  def compareQualifierConstraintLists(qcl1: List[TRAPIQualifierConstraint], qcl2: List[TRAPIQualifierConstraint]): Boolean = {
    val set1 = qcl1.map(_.qualifier_set.map(q => (q.qualifier_value, q.qualifier_type_id)).toSet)
    val set2 = qcl2.map(_.qualifier_set.map(q => (q.qualifier_value, q.qualifier_type_id)).toSet)

    (set1 == set2)
  }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    def transformPredicateWithMappedPredicateInfo(pred: PredicateMapping, mp: PredicateMappingRow): PredicateMapping = {
      val predReplaced = PredicateMapping(
        pred.predicate,
        Some(mp.biolinkPredicate),
        Some(mp.qualifierConstraint)
      )

      logger.error(f"Replacing ${pred} with ${predReplaced} with information from ${mp}")

      predReplaced
    }

    val program = for {
      preds <- getPredicatesFromSPARQL
      mappedPredicates <- getPredicateMappingsFromGitHub
      predsMappedByExactMatch = preds.map { pred =>
        val foundByExactMatch = mappedPredicates.find(
          { mp =>
            val exactMatches = mp.`exact matches`.toList.flatten.toSet

            // These are in CURIEs. Normally I would write some very complicated code to un-CURIE-fy it, but
            // instead...
            exactMatches
              .map(_.replaceFirst(raw"^RO:", "http://purl.obolibrary.org/obo/RO_"))
              .contains(pred.predicate.iri)
          }
        )

        foundByExactMatch match {
          case None     => pred
          case Some(mp) => transformPredicateWithMappedPredicateInfo(pred, mp)
        }
      }
      qualifiedPreds: Seq[PredicateMapping] = predsMappedByExactMatch.map {
        /* For each predicate, we need to check to see if this is a "mapped predicate". If so, we transform it
         into a qualified predicate as per the predicate mapping.
         */
        case pred @ PredicateMapping(_, Some(biolinkPredicate: BiolinkPredicate), _) =>
          // In some cases, we might be able to find the mappings by Biolink predicate.
          val foundByBiolinkPredicate = mappedPredicates.find(mp => mp.biolinkMappedPredicate == biolinkPredicate)

          foundByBiolinkPredicate match {
            case None     => pred
            case Some(mp) => transformPredicateWithMappedPredicateInfo(pred, mp)
          }

        // If we haven't matched it, don't transform it.
        case pred => pred
      }
      uniquePreds: Set[PredicateMapping] = qualifiedPreds.toSet
      predsOutput = uniquePreds.asJson.deepDropNullValues.spaces2SortKeys
    } yield {
      logger.info(f"Found ${preds.size} predicates:")
      preds.foreach(pred => logger.info(f" - ${pred}"))

      logger.info(f"Found ${mappedPredicates.size} predicate mappings:")
      mappedPredicates.foreach(predMap => logger.info(f" - ${predMap}"))

      logger.info(f"Transformed to ${qualifiedPreds.size} predicates:")
      qualifiedPreds.foreach(pred => logger.info(f" - ${pred}"))

      Files.writeString(PredicateJsonFilePath, predsOutput)
      logger.info(f"Wrote predicate output (${predsOutput.size}) to ${PredicateJsonFilePath}")
    }

    val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
    val runLayer = HttpClient.makeHttpClientLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer
    program.provideCustomLayer(runLayer).exitCode
  }

}

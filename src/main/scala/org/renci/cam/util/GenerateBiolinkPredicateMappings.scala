package org.renci.cam.util

import com.typesafe.scalalogging.{LazyLogging, Logger}
import io.circe.generic.auto._
import io.circe.syntax._
import org.renci.cam.LookupService.LabeledIRI
import org.renci.cam.domain.{BiolinkPredicate, TRAPIQualifier, TRAPIQualifierConstraint}
import org.renci.cam.domain.PredicateMappings.{getPredicateMappingsFromGitHub, getPredicatesFromSPARQL, PredicateMapping, PredicateMappingRow}
import org.renci.cam.{AppConfig, HttpClient, SPARQLQueryExecutor}
import zio._
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig

import java.nio.file.{Files, Paths}

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

  /* Manual annotations. */
  val manualPredicateMappings = Seq(
    PredicateMapping(
      predicate = LabeledIRI("http://purl.obolibrary.org/obo/RO_0002450", Set("directly positively regulates activity of")),
      biolinkPredicate = Some(BiolinkPredicate("affects")),
      biolinkQualifiers = Some(
        TRAPIQualifierConstraint(
          List(
            TRAPIQualifier("biolink:object_aspect_qualifier", "activity_or_abundance"),
            TRAPIQualifier("biolink:object_direction_qualifier", "increased")
          ))
      )
    ),
    PredicateMapping(
      predicate = LabeledIRI("http://purl.obolibrary.org/obo/RO_0002449", Set("directly negatively regulates activity of")),
      biolinkPredicate = Some(BiolinkPredicate("affects")),
      biolinkQualifiers = Some(
        TRAPIQualifierConstraint(
          List(
            TRAPIQualifier("biolink:object_aspect_qualifier", "activity_or_abundance"),
            TRAPIQualifier("biolink:object_direction_qualifier", "decreased")
          ))
      )
    )
  )

  /** Where should we save the predicates.json files? */
  val PredicateJsonFilePath = Paths.get("src/main/resources/biolink/predicates.json")

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
      uniquePreds: Set[PredicateMapping] = (manualPredicateMappings ++ qualifiedPreds).toSet
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

package org.renci.cam.util

import com.typesafe.scalalogging.{LazyLogging, Logger}
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.implicits._
import org.phenoscape.sparql.SPARQLInterpolation.SPARQLStringContext
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.LookupService.LabeledIRI
import org.renci.cam.SPARQLQueryExecutor.SPARQLCache
import org.renci.cam.domain.{BiolinkPredicate, IRI, TRAPIQualifierConstraint}
import org.renci.cam.{AppConfig, HttpClient, QueryService, SPARQLQueryExecutor}
import zio._
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.interop.catz._
import zio.interop.catz.implicits._
import zio.stream.ZStream

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
  override lazy val logger = Logger(GenerateBiolinkPredicateMappings.getClass.getSimpleName);

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

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val program = for {
      preds <- getPredicatesFromSPARQL
    } yield preds

    val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
    val runLayer = HttpClient.makeHttpClientLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer
    program.provideCustomLayer(runLayer).exitCode
  }

}

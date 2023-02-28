package org.renci.cam.domain

import org.renci.cam.{AppConfig, QueryService, SPARQLQueryExecutor}
import zio.{Has, RIO, Task, ZIO}
import zio.blocking.{blocking, Blocking}
import zio.config.{getConfig, ZConfig}
import io.circe.generic.auto._
import org.phenoscape.sparql.SPARQLInterpolation.SPARQLStringContext
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.LookupService.LabeledIRI
import org.renci.cam.SPARQLQueryExecutor.SPARQLCache
import org.renci.cam.util.GenerateBiolinkPredicateMappings.logger
import zio.stream.ZStream

import scala.io.Source

object PredicateMappings {

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
  def compareQualifierConstraints(ql1: List[TRAPIQualifier], ql2: List[TRAPIQualifier]): Boolean = {

    val set1 = ql1.map(q => (q.qualifier_value, q.qualifier_type_id)).toSet
    val set2 = ql2.map(q => (q.qualifier_value, q.qualifier_type_id)).toSet

    (set1 == set2)
  }

  /** Load the predicates data so we can use it subsequently. */
  val predicatesDataAsString = Source
    .fromInputStream(PredicateMappings.getClass.getResourceAsStream("biolink/predicates.json"))
    .getLines()
    .mkString("\n")

  val predicatesData = io.circe.parser.parse(predicatesDataAsString).toTry.get.as[Seq[PredicateMapping]].toTry.get

  def mapQueryEdgePredicates(predicates: Option[List[BiolinkPredicate]],
                             qualifier_constraints: Option[List[TRAPIQualifierConstraint]]): Set[IRI] = {
    // predicatesData consists of unique mappings between relations and (biolinkPredicate, biolinkQualifier) pairs.
    val biolinkPredicates = predicates.toList.flatten.toSet
    val qualifierConstraint = qualifier_constraints.toList.flatten.flatMap(_.qualifier_set)

    val relations = predicatesData.filter {
      case PredicateMapping(_, Some(biolinkPredicate), qualifierOpt) =>
        if (!biolinkPredicates.contains(biolinkPredicate)) false
        else
          qualifierOpt match {
            case None             => if (qualifierOpt.isEmpty) true else false
            case Some(constraint) => if (compareQualifierConstraints(qualifierConstraint, constraint.qualifier_set)) true else false
          }
      case _ => false
    }

    relations.map(pred => IRI(pred.predicate.iri)).toSet
  }

  def getBiolinkQualifiedPredicate(relationIRI: IRI): (BiolinkPredicate, Option[List[TRAPIQualifier]]) = {
    val biolinkPredicates = predicatesData.flatMap {
      case PredicateMapping(relation, Some(biolinkPredicate), qualifierOpt) =>
        if (relation.iri != relationIRI.value) None
        else
          qualifierOpt match {
            case None                                                 => Some((biolinkPredicate, None))
            case Some(constraint) if constraint.qualifier_set.isEmpty => Some((biolinkPredicate, None))
            case Some(constraint)                                     => Some((biolinkPredicate, Some(constraint.qualifier_set)))
          }
      case _ => None
    }

    if (biolinkPredicates.isEmpty) {
      logger.error(f"Could not find Biolink predicates for relation ${relationIRI}")
      (QueryService.BiolinkRelatedTo, None)
    } else if (biolinkPredicates.size > 1) {
      logger.error(f"Found multiple Biolink predicates for relation ${relationIRI}: ${biolinkPredicates}")
      logger.error(f"Using the first one: ${biolinkPredicates.head}")
      biolinkPredicates.head
    } else {
      biolinkPredicates.head
    }
  }

}

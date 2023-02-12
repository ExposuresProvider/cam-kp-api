package org.renci.cam.domain

import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import org.phenoscape.sparql.SPARQLInterpolation.SPARQLStringContext
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.QueryService._
import org.renci.cam.SPARQLQueryExecutor.SPARQLCache
import org.renci.cam.Util.IterableSPARQLOps
import org.renci.cam.{AppConfig, QueryService, SPARQLQueryExecutor}
import zio.blocking.{blocking, Blocking}
import zio.config.ZConfig
import zio.stream.ZStream
import zio.{Has, RIO, Task, ZIO}

import java.nio.charset.StandardCharsets
import scala.io.Source

/** Biolink 3 brings about pretty significant changes in way in which predicates work. For instance, the predicate
  * biolink:affects_activity_of has been deprecated, and replaced with biolink:affects with qualifiers indicating that what is being
  * modified is the activity.
  *
  * The good news is that this suggests a straightforward way for us to support Biolink 3 in CAM-KP:
  *   1. We will continue to allow deprecated predicates, so that biolink:affects_activity_of will continue to be mapped to
  *      http://translator.renci.org/ubergraph-axioms.ofn#acts_upstream_of_o_enabled_by.
  *
  * 2. With any luck, the validation code in the CAM-KP pipeline will tell us whether there are Biolink 3 predicates that are not mapped to
  * RO predicates; if so, we can map them manually and rerun the pipeline.
  *
  * 3. Finally, we need a new piece of code that is capable of understanding qualified predicates and converting them into AND out of our RO
  * predicates. Thus far, we have been relying on the database to do that, but at least in this initial release, the quickest way to
  * implement this will be to ingest predicate mappings from
  * https://github.com/biolink/biolink-model/blob/ac69bb2dc94d62d50f5cfab3fa07414b0ca092b1/predicate_mapping.yaml and to use them to handle
  * the conversion from Biolink2 predicates (as currently supported by the triplestore) and Biolink3 predicate+qualifier combinations.
  *
  * This new code needs to support two operations:
  *   1. Given a TRAPI Edge (i.e. a Biolink predicate + qualifiers), produce a set of RO predicates that represents this edge.
  *
  * 2. Given a single RO predicate (which is what our SPARQL query should return), produce a single TRAPI Edge that represents this edge.
  *
  * This file is intended to store that interconversion code. It will replace some of our current SPARQL accessing code.
  *
  * References:
  *   - https://github.com/biolink/biolink-model/blob/ac69bb2dc94d62d50f5cfab3fa07414b0ca092b1/Migration_3.0_Guide.md
  *   - https://github.com/biolink/biolink-model/blob/ac69bb2dc94d62d50f5cfab3fa07414b0ca092b1/predicate_mapping.yaml
  *   - https://github.com/NCATSTranslator/TranslatorArchitecture/issues/79
  *   - https://github.com/NCATSTranslator/TranslatorArchitecture/issues/80
  *   - https://github.com/NCATSTranslator/TranslatorArchitecture/issues/81
  *   - https://github.com/NCATSTranslator/TranslatorArchitecture/issues/82
  */
object Biolink3 extends LazyLogging {

  /** A case class for predicate mappings. */
  case class PredicateMapping(
    `mapped predicate`: String,
    `object aspect qualifier`: Option[String],
    `object direction qualifier`: Option[String],
    predicate: String,
    `qualified predicate`: Option[String],
    `exact matches`: Option[Set[String]]
  ) {
    def biolinkPredicate: BiolinkPredicate = BiolinkPredicate(predicate)

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

    def qualifierConstraint = TRAPIQualifierConstraint(qualifier_set = qualifiers.toList)
    def qualifierConstraintList = List(qualifierConstraint)
  }

  case class PredicateMappings(
    `predicate mappings`: List[PredicateMapping]
  )

  /** To initialize this object, we need to download and parse the predicate_mapping.yaml file from the Biolink model, which needs to be
    * downloaded to the package resources (src/main/resources) from
    * https://github.com/biolink/biolink-model/blob/master/predicate_mapping.yaml
    */
  val predicateMappings: RIO[Blocking, List[PredicateMapping]] =
    for {
      predicateMappingText <- blocking(
        Task.effect(
          Source
            .fromInputStream(getClass.getResourceAsStream("/predicate_mapping.yaml"), StandardCharsets.UTF_8.name())
            .getLines()
            .mkString("\n")))
      predicateMappingsYaml <- ZIO.fromEither(io.circe.yaml.parser.parse(predicateMappingText))
      predicateMappings <- ZIO.fromEither(predicateMappingsYaml.as[PredicateMappings])
    } yield predicateMappings.`predicate mappings`

  /** Compares two qualifier lists. */
  def compareQualifierConstraintLists(qcl1: List[TRAPIQualifierConstraint], qcl2: List[TRAPIQualifierConstraint]): Boolean = {
    val set1 = qcl1.map(_.qualifier_set.map(q => (q.qualifier_value, q.qualifier_type_id)).toSet)
    val set2 = qcl2.map(_.qualifier_set.map(q => (q.qualifier_value, q.qualifier_type_id)).toSet)

    (set1 == set2)
  }

  /** Case class used to report that an unmapped predicate was found. */
  case class UnmappedPredicate(predicates: List[BiolinkPredicate], qualifierConstraints: List[TRAPIQualifierConstraint])

  /** Map Biolink 2 edges to Biolink 3 edges.
    *
    * @param queryGraph
    *   A Biolink 3 query graph.
    * @return
    *   A Biolink 2 query graph.
    */
  def mapBL3toBL2(
    queryGraph: TRAPIQueryGraph): ZIO[ZConfig[AppConfig] with HttpClient with Has[SPARQLCache], UnmappedPredicate, TRAPIQueryGraph] =
    for {
      newEdges <- ZStream
        .fromIterable(queryGraph.edges)
        .mapM { case (edgeName, edge) =>
          edge.qualifier_constraints match {
            // If the qualifier constraints are empty, we don't need to change anything.
            case None         => ZIO.succeed(List((edgeName, edge)))
            case Some(List()) => ZIO.succeed(List((edgeName, edge)))
            case Some(qcs)    =>
              // A TRAPIQueryEdge may have multiple predicates, or none at all. If there are none, we assume that it is
              // the default Biolink predicate.

              val preds = edge.predicates.getOrElse(List(QueryService.DefaultBiolinkPredicate))

              // TODO: figure out how to run this without unsafeRun().
              val mappings = zio.Runtime.default.unsafeRun(
                for {
                  mps <- predicateMappings
                  mapping <- ZStream
                    .fromIterable(mps)
                    .map { mp =>
                      preds.flatMap { pred =>
                        if (pred == mp.biolinkPredicate && compareQualifierConstraintLists(qcs, mp.qualifierConstraintList)) {
                          logger.info(s"Found matching predicate for edge '${edgeName}' ${edge}: ${mp}")
                          List((pred, BiolinkPredicate(mp.`mapped predicate`.replace(' ', '_'))))
                        } else {
                          logger.info(s"Edge ${edge} did not match ${mp.biolinkPredicate}:${mp.qualifierConstraintList}")
                          List()
                        }
                      }
                    }
                    .runCollect
                } yield mapping.toList.flatten
              )

              val unmappedPredicates = preds.filterNot(mappings.toMap.keySet.contains)
              if (unmappedPredicates.nonEmpty) {
                ZIO.fail(UnmappedPredicate(edge.predicates.get, qcs))
              } else {
                val mappingsMap = mappings.groupMap(_._1)(_._2)
                val transformedPredicates = preds.flatMap(pred => mappingsMap(pred)).distinct
                val transformedQueryEdge = TRAPIQueryEdge(
                  Some(transformedPredicates),
                  edge.subject,
                  edge.`object`,
                  edge.knowledge_type,
                  None,
                  None
                )

                logger.info(f"Transformed ${edge} to ${transformedQueryEdge}")

                ZIO.succeed(List((edgeName, transformedQueryEdge)))
              }
          }
        }
        .runCollect
    } yield TRAPIQueryGraph(
      nodes = queryGraph.nodes,
      edges = newEdges.toList.flatten.toMap
    )

  /** Map Biolink 2 edges to Biolink 3 edges.
    *
    * @param queryGraph
    *   A Biolink 2 query graph.
    * @return
    *   A Biolink 3 query graph.
    */
  def mapBL2toBL3(
    queryGraph: TRAPIQueryGraph): ZIO[ZConfig[AppConfig] with HttpClient with Has[SPARQLCache], UnmappedPredicate, TRAPIQueryGraph] =
    for {
      newEdges <- ZStream
        .fromIterable(queryGraph.edges)
        .mapM { case (edgeName, edge) =>
          // To simplify this, let's assume that qualifier constraints don't qualify predicates -- which means we can
          // leave all of them in when we do the mapping.

          val preds = edge.predicates.getOrElse(List(QueryService.DefaultBiolinkPredicate))

          // TODO: figure out how to run this without unsafeRun.
          val predicatesWithQCLs = zio.Runtime.default
            .unsafeRun(
              for {
                mps <- predicateMappings
                mapping <- ZStream
                  .fromIterable(preds)
                  .flatMap { pred =>
                    ZStream
                      .fromIterable(mps)
                      .map { mp =>
                        logger.info(s"Comparing predicate ${pred} to ${mp}")
                        if (pred == BiolinkPredicate(mp.`mapped predicate`.replace(' ', '_')))
                          List((mp.biolinkPredicate, mp.qualifierConstraintList))
                        else List()
                      }
                  }
                  .runCollect
              } yield mapping.toList
            )
            .flatten

          val transformedQueryEdge = TRAPIQueryEdge(
            Some(predicatesWithQCLs.map(_._1).distinct),
            edge.subject,
            edge.`object`,
            edge.knowledge_type,
            None,
            Some((edge.qualifier_constraints ++ predicatesWithQCLs.map(_._2)).flatten.toSet.toList)
          )

          logger.info(f"Transformed ${edge} to ${transformedQueryEdge}")

          ZIO.succeed(List((edgeName, transformedQueryEdge)))
        }
        .runCollect
    } yield TRAPIQueryGraph(
      nodes = queryGraph.nodes,
      edges = newEdges.toList.flatten.toMap
    )

  def mapQueryBiolinkPredicatesToRelations(predicates: Set[BiolinkPredicate], queryGraph: TRAPIQueryGraph)
    : RIO[ZConfig[AppConfig] with HttpClient with Has[SPARQLCache], (Map[BiolinkPredicate, Set[IRI]], Seq[String])] = {
    val allQualifierConstraints = queryGraph.edges.values
      .flatMap(_.qualifier_constraints.getOrElse(List()))
      .flatMap(_.qualifier_set.map(q => f"${q.qualifier_type_id}=${q.qualifier_value}"))

    final case class Predicate(biolinkPredicate: BiolinkPredicate, predicate: IRI)
    val queryText = sparql"""
        SELECT DISTINCT ?biolinkPredicate ?predicate WHERE {
          VALUES ?biolinkPredicate { ${predicates.asValues} }
          ?predicate $SlotMapping ?biolinkPredicate .
          FILTER EXISTS { ?s ?predicate ?o }
          $BigDataQueryHintQuery $BigDataQueryHintFilterExists "SubQueryLimitOne"
        }"""
    for {
      predicates <- SPARQLQueryExecutor.runSelectQueryWithCacheAs[Predicate](queryText.toQuery)
    } yield (predicates.to(Set).groupMap(_.biolinkPredicate)(_.predicate), allQualifierConstraints.toSeq)
  }

  // TODO:
  // - Change this to cached queries (see mapQueryBiolinkPredicatesToRelations for example)
  def mapRelationsToLabelAndBiolink(relations: Set[IRI]): RIO[ZConfig[AppConfig] with HttpClient, Map[IRI, (Option[String], IRI)]] = {
    final case class RelationInfo(relation: IRI, biolinkSlot: IRI, label: Option[String])
    val queryText = sparql"""
         SELECT DISTINCT ?relation ?biolinkSlot ?label
         WHERE {
           VALUES ?relation { ${relations.asValues} }
           ?relation $SlotMapping ?biolinkSlot .
           ?biolinkSlot a $BiolinkMLSlotDefinition .
           OPTIONAL { ?relation $RDFSLabel ?label . }
           FILTER NOT EXISTS {
             ?relation $SlotMapping ?other .
             ?other $BiolinkMLIsA+/$BiolinkMLMixins* ?biolinkSlot .
           }
         }"""
    SPARQLQueryExecutor.runSelectQueryAs[RelationInfo](queryText.toQuery).map { res =>
      res.groupMap(_.relation)(info => (info.label, info.biolinkSlot)).map { case (relationIRI, infos) => relationIRI -> infos.head }
    }
  }

}

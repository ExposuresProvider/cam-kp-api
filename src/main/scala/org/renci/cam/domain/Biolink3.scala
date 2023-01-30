package org.renci.cam.domain

import io.circe.generic.auto._
import org.phenoscape.sparql.SPARQLInterpolation.SPARQLStringContext
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.QueryService._
import org.renci.cam.SPARQLQueryExecutor.SPARQLCache
import org.renci.cam.Util.IterableSPARQLOps
import org.renci.cam.{AppConfig, SPARQLQueryExecutor}
import zio.ZIO.ZIOAutoCloseableOps
import zio.blocking.{effectBlockingIO, Blocking}
import zio.config.ZConfig
import zio.{Has, RIO, ZIO}

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
object Biolink3 {

  /** A case class for predicate mappings. */
  case class PredicateMapping(
    mappedPredicate: String,
    objectAspectQualifier: String,
    objectDirectionQualified: Option[String],
    predicate: String,
    qualifiedPredicate: String,
    exactMatches: Set[String]
  )

  case class PredicateMappings(
    predicateMappings: List[PredicateMapping]
  )

  /** To initialize this object, we need to download and parse the predicate_mapping.yaml file from the Biolink model, which needs to be
    * downloaded to the package resources (src/main/resources) from
    * https://github.com/biolink/biolink-model/blob/master/predicate_mapping.yaml
    */
  val predicateMappings: RIO[Blocking, List[PredicateMapping]] =
    for {
      predicateMappingText <- effectBlockingIO(
        Source.fromInputStream(getClass.getResourceAsStream("/predicates.csv"), StandardCharsets.UTF_8.name()))
        .bracketAuto { source =>
          effectBlockingIO(source.getLines().mkString("\n"))
        }
      predicateMappingsYaml <- ZIO.fromEither(io.circe.yaml.parser.parse(predicateMappingText))
      predicateMappings <- ZIO.fromEither(predicateMappingsYaml.as[PredicateMappings])
    } yield predicateMappings.predicateMappings

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

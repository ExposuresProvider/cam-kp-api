package org.renci.cam.domain

import org.phenoscape.sparql.SPARQLInterpolation.SPARQLStringContext
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.QueryService.{BigDataQueryHintFilterExists, BigDataQueryHintQuery, BiolinkMLIsA, BiolinkMLMixins, BiolinkMLSlotDefinition, RDFSLabel, SlotMapping}
import org.renci.cam.{AppConfig, SPARQLQueryExecutor}
import org.renci.cam.SPARQLQueryExecutor.SPARQLCache
import org.renci.cam.Util.IterableSPARQLOps
import zio.config.ZConfig
import zio.{Has, RIO}

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
  * predicates. Thus far, we have been relying on the database to do that, but at least in this initial release, this will need to be
  * handled manually until we've gotten it right. Then we can figure out if it makes sense to move this back into the database.
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

  def mapQueryBiolinkPredicatesToRelations(
    predicates: Set[BiolinkPredicate]): RIO[ZConfig[AppConfig] with HttpClient with Has[SPARQLCache], Map[BiolinkPredicate, Set[IRI]]] = {
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
    } yield predicates.to(Set).groupMap(_.biolinkPredicate)(_.predicate)
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

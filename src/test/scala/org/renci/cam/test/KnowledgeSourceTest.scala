package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import org.renci.cam._
import org.renci.cam.domain._
import zio.Layer
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.test._

/** Tests whether we return the correct knowledge sources on queries.
  */
object KnowledgeSourceTest extends DefaultRunnableSpec with LazyLogging {

  /** This query asks: "what is positively regulated by GO:0004709 [MAP3K activity]?" One of the sources from original knowledge source
    * infores:go-cam should be http://model.geneontology.org/568b0f9600000284
    */
  val testPositivelyRegulatedByMAP3K = {
    val trapiQueryGraph = TRAPIQueryGraph(
      Map(
        "n0" -> TRAPIQueryNode(Some(List(IRI("http://purl.obolibrary.org/obo/GO_0004709"))), None, None),
        "n1" -> TRAPIQueryNode(None, Some(List(BiolinkClass("NamedThing"))), None)
      ),
      Map("e0" -> TRAPIQueryEdge(Some(List(BiolinkPredicate("positively_regulates"))), "n0", "n1", None))
    )
    logger.info(s"Querying with TRAPI query: ${trapiQueryGraph}")
    val query = QueryService.run(1000, trapiQueryGraph)

    suite("testPositivelyRegulatedByMAP3K")(
      testM("Received GO-CAM results, including model http://model.geneontology.org/568b0f9600000284") {
        for {
          response <- query
          kgraph = response.message.knowledge_graph.getOrElse(TRAPIKnowledgeGraph(Map(), Map()))
          attrsById = kgraph.edges.transform((_, value) => value.attributes.getOrElse(List()))

          // Sources
          sourcesById = attrsById.transform((_, value) => value.map(_.attribute_source.get))

          // Original knowledge sources
          originalKnowledgeSourcesById = attrsById.transform((_, value) =>
            value.filter(_.attribute_type_id == BiolinkPredicate("original_knowledge_source").iri))
          originalKnowledgeSources = originalKnowledgeSourcesById.values.flatten

          // Find edges from a particular entry of GeneOntology model http://model.geneontology.org/568b0f9600000284
          selectedAttribs = originalKnowledgeSources.filter(_.value_url.getOrElse("") == "http://model.geneontology.org/568b0f9600000284")
          selectedSources = selectedAttribs.flatMap(_.attribute_source)
          selectedOKGs = selectedAttribs.filter(_.attribute_type_id == BiolinkPredicate("original_knowledge_source").iri).flatMap(_.value)

        } yield assert(response.message.results)(Assertion.isSome(Assertion.isNonEmpty)) &&
          assert(sourcesById.values.flatten)(Assertion.contains("infores:cam-kp")) &&
          assert(selectedAttribs)(Assertion.isNonEmpty) &&
          assert(selectedSources)(Assertion.forall(Assertion.equalTo("infores:cam-kp"))) &&
          assert(selectedOKGs)(Assertion.forall(Assertion.equalTo("infores:go-cam")))
      }
    )
  }

  /** This query asks: "list all named things related to valproic acid (CHEBI:39867)" The original knowledge source should include:
    * infores:ctd
    */
  val testRelatedToValproicAcid = {
    val trapiQueryGraph = TRAPIQueryGraph(
      Map(
        "n0" -> TRAPIQueryNode(Some(List(IRI("http://purl.obolibrary.org/obo/CHEBI_39867"))), None, None),
        "n1" -> TRAPIQueryNode(None, Some(List(BiolinkClass("NamedThing"))), None)
      ),
      Map("e0" -> TRAPIQueryEdge(Some(List(BiolinkPredicate("related_to"))), "n0", "n1", None))
    )
    logger.info(s"Querying with TRAPI query: ${trapiQueryGraph}")
    val query = QueryService.run(1000, trapiQueryGraph)

    suite("testRelatedToValproicAcid")(
      testM("Received CTD results, including model http://ctdbase.org/detail.go?type=relationship&ixnId=3203039#inferred") {

        for {
          response <- query
          kgraph = response.message.knowledge_graph.getOrElse(TRAPIKnowledgeGraph(Map(), Map()))
          attrsById = kgraph.edges.transform((_, value) => value.attributes.getOrElse(List()))

          // Sources
          sourcesById = attrsById.transform((_, value) => value.map(_.attribute_source.get))

          // Original knowledge sources
          originalKnowledgeSourcesById = attrsById.transform((_, value) =>
            value.filter(_.attribute_type_id == BiolinkPredicate("original_knowledge_source").iri))
          originalKnowledgeSources = originalKnowledgeSourcesById.values.flatten

          // Find edges from a particular entry of CTD model http://ctdbase.org/detail.go?type=relationship&ixnId=3203039#inferred
          selectedAttribs = originalKnowledgeSources.filter(
            _.value_url.getOrElse("") == "http://ctdbase.org/detail.go?type=relationship&ixnId=3203039#inferred")
          selectedSources = selectedAttribs.flatMap(_.attribute_source)
          selectedOKGs = selectedAttribs.filter(_.attribute_type_id == BiolinkPredicate("original_knowledge_source").iri).flatMap(_.value)

        } yield assert(response.message.results)(Assertion.isSome(Assertion.isNonEmpty)) &&
          assert(sourcesById.values.flatten)(Assertion.contains("infores:cam-kp")) &&
          assert(selectedAttribs)(Assertion.isNonEmpty) &&
          assert(selectedSources)(Assertion.forall(Assertion.equalTo("infores:cam-kp"))) &&
          assert(selectedOKGs)(Assertion.forall(Assertion.equalTo("infores:ctd")))
      }
    )
  }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val testLayer = HttpClient.makeHttpClientLayer ++ Biolink.makeUtilitiesLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer

  def spec: Spec[environment.TestEnvironment, TestFailure[Throwable], TestSuccess] = suite("Knowledge source tests")(
    testPositivelyRegulatedByMAP3K,
    testRelatedToValproicAcid
  ).provideCustomLayer(testLayer.mapError(TestFailure.die))

}

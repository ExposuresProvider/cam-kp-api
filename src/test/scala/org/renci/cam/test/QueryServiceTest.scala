package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.IOUtils
import org.apache.jena.query.{QuerySolution, QuerySolutionMap, ResultSet, ResultSetFactory}
import org.apache.jena.rdf.model.RDFNode
import org.renci.cam.QueryService.Triple
import org.renci.cam._
import org.renci.cam.domain._
import zio._
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.test.Assertion._
import zio.test._

import java.nio.charset.StandardCharsets

/** This suite of tests was intended to interrogate how QueryService.run() worked. However, I ended up implementing a smaller (and hopefully
  * more testable) QueryService.run() before fully finishing this test suite. I'll leave these tests for now, but my plan is to delete the
  * QueryService.oldRun() and related code, and delete these tests as the code they test go away. I'll separate tests that should remain
  * from those that will be deprecated and deleted, but it'll probably all be a bit of a mess until QueryService has been fully cleaned.
  */
object QueryServiceTest extends DefaultRunnableSpec with LazyLogging {
  val n0Node = TRAPIQueryNode(None, Some(List(BiolinkClass("Gene"))), None)
  val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("BiologicalProcess"))), None)
  val e0Edge = TRAPIQueryEdge(None, "n1", "n0", None)
  val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))

  /* Tests for the old QueryService.oldRun() method. These will be deleted as they are deprecated. */
//  val testGetNodeTypes = suite("testGetNodeTypes")(
//    testM("test get node types sans id") {
//      val n0Node = TRAPIQueryNode(None, Some(BiolinkClass("Gene")), None)
//      val n1Node = TRAPIQueryNode(None, Some(BiolinkClass("BiologicalProcess")), None)
//      val nodeMap = Map("n0" -> n0Node, "n1" -> n1Node)
//      for {
//        nodeTypes <- ZIO.effect(QueryService.getNodeTypes(nodeMap))
//      } yield assert(nodeTypes)(hasKey("n0")) && assert(nodeTypes.get("n0").get)(equalTo(BiolinkClass("Gene").iri))
//    },
//    testM("test get node types with id") {
//      val n0Node = TRAPIQueryNode(Some(IRI("http://www.ncbi.nlm.nih.gov/gene/558")), Some(BiolinkClass("Gene")), None)
//      val n1Node = TRAPIQueryNode(None, Some(BiolinkClass("BiologicalProcess")), None)
//      val nodeMap = Map("n0" -> n0Node, "n1" -> n1Node)
//      for {
//        nodeTypes <- ZIO.effect(QueryService.getNodeTypes(nodeMap))
//      } yield assert(nodeTypes)(hasKey("n0")) && assert(nodeTypes.get("n0").get)(equalTo(IRI("http://www.ncbi.nlm.nih.gov/gene/558")))
//    },
//    testM("test get node types with nothing") {
//      val n0Node = TRAPIQueryNode(None, None, None)
//      val n1Node = TRAPIQueryNode(None, None, None)
//      val nodeMap = Map("n0" -> n0Node, "n1" -> n1Node)
//      for {
//        nodeTypes <- ZIO.effect(QueryService.getNodeTypes(nodeMap))
//      } yield assert(nodeTypes)(isEmpty)
//    }
//  )

  val testEnforceQueryEdgeTypes = suite("testEnforceQueryEdgeTypes")(
    testM("test QueryService.enforceQueryEdgeTypes") {
      for {
        nodeTypes <- ZIO.effect(QueryService.enforceQueryEdgeTypes(queryGraph, List(BiolinkPredicate("related_to"))))
      } yield assert(nodeTypes.edges)(hasKey("e0")) && assertTrue(
        nodeTypes.edges("e0").predicates.get == List(BiolinkPredicate("related_to")))
    }
  )

  val testGetTRAPINodeBindings = suite("testGetTRAPINodeBindings")(
    testM("test QueryService.getTRAPINodeBindings") {
      val (queryGraph, resultSet) = getSimpleData
      for {
        nodeBindings <- QueryService.getTRAPINodeBindings(queryGraph, resultSet.next())
      } yield assert(nodeBindings.keys)(
        contains("n0") && contains("n1")
      ) && assertTrue(
        nodeBindings("n0")
          .map(a => a.id)
          .contains(IRI("http://purl.obolibrary.org/obo/go/extensions/reacto.owl#REACTO_R-HSA-166103")))
    }
  )

  def getSimpleData: (TRAPIQueryGraph, ResultSet) = {
    val n0Node = TRAPIQueryNode(None, Some(List(BiolinkClass("Gene"))), None)
    val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("BiologicalProcess"))), None)
    val e0Edge = TRAPIQueryEdge(None, "n1", "n0", None)
    val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))

    val response = """{
          "head" : {
            "vars" : [ "e0", "n1", "n0", "n0_type", "n1_type" ]
          },
          "results" : {
            "bindings" : [ {
            "e0" : {
            "type" : "uri",
            "value" : "http://purl.obolibrary.org/obo/RO_0000057"
          },
            "n1" : {
            "type" : "uri",
            "value" : "http://model.geneontology.org/R-HSA-166362_regulator_bp_RO_0002212_R-HSA-166103_R-HSA-166362_controller"
          },
            "n0" : {
            "type" : "uri",
            "value" : "http://model.geneontology.org/R-HSA-166103_R-HSA-166362_controller"
          },
            "n0_type" : {
            "type" : "uri",
            "value" : "http://purl.obolibrary.org/obo/go/extensions/reacto.owl#REACTO_R-HSA-166103"
          },
            "n1_type" : {
            "type" : "uri",
            "value" : "http://purl.obolibrary.org/obo/GO_0008150"
          }
          } ]
          }
        }"""
    val is = IOUtils.toInputStream(response, StandardCharsets.UTF_8)
    val resultSet = ResultSetFactory.fromJSON(is)
    is.close()
    (queryGraph, resultSet)
  }

  val testQueryTexts = suite("testQueryTexts")(
    testM("test QueryService.getTRAPINodeDetailsQueryText") {
      val nodeIdList = List(
        IRI("http://purl.obolibrary.org/obo/GO_0047196"),
        IRI("http://purl.obolibrary.org/obo/GO_0017064")
      )
      for {
        queryText <- Task.effect(QueryService.getTRAPINodeDetailsQueryText(nodeIdList))
      } yield assertTrue(
        queryText.text.contains(
          "VALUES ?term {  <http://purl.obolibrary.org/obo/GO_0047196>  <http://purl.obolibrary.org/obo/GO_0017064>  }"))
    },
    testM("test QueryService.getProvenanceQueryText") {
      for {
        edges <- Task.effect(
          Set(
            QueryService.Triple(
              IRI("http://model.geneontology.org/R-HSA-163567_R-HSA-163595_controller"),
              IRI("http://purl.obolibrary.org/obo/BFO_0000050"),
              IRI("http://model.geneontology.org/reaction_R-HSA-163595_location_lociGO_0005811")
            ),
            QueryService.Triple(
              IRI("http://model.geneontology.org/R-HSA-6808466_R-HSA-6808464_controller"),
              IRI("http://purl.obolibrary.org/obo/BFO_0000050"),
              IRI("http://model.geneontology.org/reaction_R-HSA-6808464_location_lociGO_0005811")
            )
          )
        )
        queryText <- Task.effect(QueryService.getProvenanceQueryText(edges))
      } yield assert(queryText.text)(
        containsString(
          "( <http://model.geneontology.org/R-HSA-163567_R-HSA-163595_controller> <http://purl.obolibrary.org/obo/BFO_0000050> <http://model.geneontology.org/reaction_R-HSA-163595_location_lociGO_0005811> )"
        ) &&
          containsString(
            "( <http://model.geneontology.org/R-HSA-6808466_R-HSA-6808464_controller> <http://purl.obolibrary.org/obo/BFO_0000050> <http://model.geneontology.org/reaction_R-HSA-6808464_location_lociGO_0005811> )"
          )
      )
    },
    testM("test QueryService.getCAMStuffQueryText") {
      for {
        queryText <- Task.effect(QueryService.getCAMStuffQueryText(IRI("http://model.geneontology.org/R-HSA-2142753")))
      } yield assertTrue(queryText.text.contains("{ GRAPH <http://model.geneontology.org/R-HSA-2142753>"))
    }
  )

  val testGetNodesToDirectTypes = suite("testGetNodesToDirectTypes")(
    zio.test.test("test QueryService.getNodesToDirectTypes") {
      val (queryGraph, _) = getSimpleData
      val queryText = QueryService.getNodesToDirectTypes(queryGraph.nodes)
      assert(queryText.text.trim)(
        containsString("?n0 <http://www.openrdf.org/schema/sesame#directType> ?n0_type .") && containsString(
          "?n1 <http://www.openrdf.org/schema/sesame#directType> ?n1_type .")
      )
    }
  )

  val testGetProjections = suite("testGetProjections")(
    zio.test.test("test QueryService.getProjections") {
      val (queryGraph, _) = getSimpleData
      val queryText = QueryService.getProjections(queryGraph)
      // In addition to ?e0, ?n0, ?n1, we also return ?n0_class and ?n1_class in order to be able to identify the original class.
      assertTrue(queryText.text.trim.split("\\s+", -1).to(Set) == Set("?e0", "?n0", "?n1", "?n0_class", "?n1_class"))
    }
  )

  /*
   * The following tests were added as part of a series of tests about the different components of QueryService.
   * Some of these tests may be better integrated in some of the tests above.
   */

  val testQueryServiceSteps = {
    def createQuerySolutionMap(variableToValueMapping: Map[String, RDFNode]): QuerySolution = {
      val qsm = new QuerySolutionMap()
      variableToValueMapping.foreach { case (varName, rdfNode) => qsm.add(varName, rdfNode) }
      qsm
    }

    val relationsToLabelAndBiolinkPredicate: Map[IRI, (Option[String], IRI)] = Map(
      IRI("http://purl.obolibrary.org/obo/RO_0002565") -> (Some("results in movement of"), IRI(
        "https://w3id.org/biolink/vocab/participates_in")),
      IRI("http://purl.obolibrary.org/obo/RO_0000057") -> (Some("has participant"), IRI("https://w3id.org/biolink/vocab/has_participant")),
      IRI("http://purl.obolibrary.org/obo/RO_0002087") -> (Some("immediately preceded by"), IRI(
        "https://w3id.org/biolink/vocab/preceded_by")),
      IRI("http://purl.obolibrary.org/obo/RO_0002500") -> (Some("causal agent in process"), IRI(
        "https://w3id.org/biolink/vocab/capable_of")),
      IRI("http://www.w3.org/2004/02/skos/core#narrowMatch") -> (None, IRI("https://w3id.org/biolink/vocab/narrow_match")),
      IRI("http://purl.obolibrary.org/obo/RO_0004007") -> (Some("has primary input or output"), IRI(
        "https://w3id.org/biolink/vocab/has_participant")),
      IRI("http://purl.obolibrary.org/obo/RO_0002131") -> (Some("overlaps"), IRI("https://w3id.org/biolink/vocab/overlaps")),
      IRI("http://purl.obolibrary.org/obo/RO_0002206") -> (Some("expressed in"), IRI("https://w3id.org/biolink/vocab/expressed_in")),
      IRI("http://purl.obolibrary.org/obo/RO_0002436") -> (Some("molecularly interacts with"), IRI(
        "https://w3id.org/biolink/vocab/molecularly_interacts_with")),
      IRI("http://purl.obolibrary.org/obo/RO_0002160") -> (Some("only in taxon"), IRI("https://w3id.org/biolink/vocab/in_taxon")),
      IRI("http://purl.obolibrary.org/obo/RO_0001025") -> (Some("located in"), IRI("https://w3id.org/biolink/vocab/located_in")),
      IRI("http://purl.obolibrary.org/obo/RO_0002093") -> (Some("ends during"), IRI(
        "https://w3id.org/biolink/vocab/temporally_related_to")),
      IRI("http://purl.obolibrary.org/obo/RO_0002356") -> (Some("results in specification of"), IRI(
        "https://w3id.org/biolink/vocab/related_to")),
      IRI("http://www.w3.org/2004/02/skos/core#relatedMatch") -> (None, IRI("https://w3id.org/biolink/vocab/related_to")),
      IRI("http://purl.obolibrary.org/obo/RO_0002216") -> (Some("capable of part of"), IRI(
        "https://w3id.org/biolink/vocab/participates_in")),
      IRI("http://purl.obolibrary.org/obo/BFO_0000063") -> (Some("precedes"), IRI("https://w3id.org/biolink/vocab/precedes")),
      IRI("http://purl.obolibrary.org/obo/RO_0002432") -> (Some("is active in"), IRI("https://w3id.org/biolink/vocab/active_in")),
      IRI("http://purl.obolibrary.org/obo/BFO_0000051") -> (Some("has part"), IRI("https://w3id.org/biolink/vocab/has_part")),
      IRI("http://purl.obolibrary.org/obo/RO_0002349") -> (Some("results in determination of"), IRI(
        "https://w3id.org/biolink/vocab/related_to")),
      IRI("http://purl.obolibrary.org/obo/RO_0002204") -> (Some("gene product of"), IRI("https://w3id.org/biolink/vocab/gene_product_of")),
      IRI("http://purl.obolibrary.org/obo/RO_0002488") -> (Some("existence starts during"), IRI(
        "https://w3id.org/biolink/vocab/temporally_related_to")),
      IRI("http://purl.obolibrary.org/obo/RO_0004034") -> (Some("acts upstream of, positive effect"), IRI(
        "https://w3id.org/biolink/vocab/acts_upstream_of_positive_effect")),
      IRI("http://purl.obolibrary.org/obo/RO_0002497") -> (Some("end stage"), IRI("https://w3id.org/biolink/vocab/temporally_related_to")),
      IRI("http://purl.obolibrary.org/obo/RO_0002292") -> (Some("expresses"), IRI("https://w3id.org/biolink/vocab/expresses")),
      IRI("http://purl.obolibrary.org/obo/RO_0004033") -> (Some("acts upstream of or within, negative effect"), IRI(
        "https://w3id.org/biolink/vocab/acts_upstream_of_or_within_negative_effect")),
      IRI("http://purl.obolibrary.org/obo/RO_0001015") -> (Some("location of"), IRI("https://w3id.org/biolink/vocab/location_of")),
      IRI("http://purl.obolibrary.org/obo/RO_0002434") -> (Some("interacts with"), IRI("https://w3id.org/biolink/vocab/interacts_with")),
      IRI("http://purl.obolibrary.org/obo/RO_0002263") -> (Some("acts upstream of"), IRI(
        "https://w3id.org/biolink/vocab/acts_upstream_of")),
      IRI("http://purl.obolibrary.org/obo/RO_0002212") -> (Some("negatively regulates"), IRI(
        "https://w3id.org/biolink/vocab/negatively_regulates")),
      IRI("http://purl.obolibrary.org/obo/RO_0002496") -> (Some("existence starts during or after"), IRI(
        "https://w3id.org/biolink/vocab/temporally_related_to")),
      IRI("http://purl.obolibrary.org/obo/RO_0002331") -> (Some("involved in"), IRI("https://w3id.org/biolink/vocab/actively_involved_in")),
      IRI("http://purl.obolibrary.org/obo/RO_0002608") -> (Some("process has causal agent"), IRI(
        "https://w3id.org/biolink/vocab/caused_by")),
      IRI("http://purl.obolibrary.org/obo/RO_0002314") -> (Some("characteristic of part of"), IRI(
        "https://w3id.org/biolink/vocab/related_to")),
      IRI("http://purl.obolibrary.org/obo/RO_0002449") -> (Some("directly negatively regulates activity of"), IRI(
        "https://w3id.org/biolink/vocab/entity_negatively_regulates_entity")),
      IRI("http://purl.obolibrary.org/obo/RO_0002333") -> (Some("enabled by"), IRI("https://w3id.org/biolink/vocab/enabled_by")),
      IRI("http://purl.obolibrary.org/obo/RO_0002298") -> (Some("results in morphogenesis of"), IRI(
        "https://w3id.org/biolink/vocab/has_output")),
      IRI("http://purl.obolibrary.org/obo/RO_0000056") -> (Some("participates in"), IRI("https://w3id.org/biolink/vocab/participates_in")),
      IRI("http://purl.obolibrary.org/obo/RO_0002229") -> (Some("ends"), IRI("https://w3id.org/biolink/vocab/temporally_related_to")),
      IRI("http://purl.obolibrary.org/obo/RO_0004032") -> (Some("acts upstream of or within, positive effect"), IRI(
        "https://w3id.org/biolink/vocab/acts_upstream_of_or_within")),
      IRI("http://translator.renci.org/ubergraph-axioms.ofn#acts_upstream_of_o_enabled_by") -> (Some("acts_upstream_of_o_enabled_by"), IRI(
        "https://w3id.org/biolink/vocab/affects_activity_of")),
      IRI("http://purl.obolibrary.org/obo/RO_0000052") -> (Some("characteristic of"), IRI("https://w3id.org/biolink/vocab/related_to")),
      IRI("http://purl.obolibrary.org/obo/BFO_0000066") -> (Some("occurs in"), IRI("https://w3id.org/biolink/vocab/occurs_in")),
      IRI("http://www.w3.org/2004/02/skos/core#exactMatch") -> (None, IRI("https://w3id.org/biolink/vocab/same_as")),
      IRI("http://www.w3.org/2000/01/rdf-schema#subClassOf") -> (None, IRI("https://w3id.org/biolink/vocab/subclass_of")),
      IRI("http://purl.obolibrary.org/obo/RO_0002588") -> (Some("results_in_assembly_of"), IRI(
        "https://w3id.org/biolink/vocab/has_output")),
      IRI("http://www.w3.org/2000/01/rdf-schema#subPropertyOf") -> (None, IRI("https://w3id.org/biolink/vocab/subclass_of")),
      IRI("http://purl.obolibrary.org/obo/RO_0002348") -> (Some("results in commitment to"), IRI(
        "https://w3id.org/biolink/vocab/related_to")),
      IRI("http://purl.obolibrary.org/obo/BFO_0000062") -> (Some("preceded by"), IRI("https://w3id.org/biolink/vocab/preceded_by")),
      IRI("http://purl.obolibrary.org/obo/RO_0002211") -> (Some("regulates"), IRI(
        "https://w3id.org/biolink/vocab/process_regulates_process")),
      IRI("http://www.w3.org/2004/02/skos/core#closeMatch") -> (None, IRI("https://w3id.org/biolink/vocab/close_match")),
      IRI("http://purl.obolibrary.org/obo/RO_0002224") -> (Some("starts with"), IRI(
        "https://w3id.org/biolink/vocab/temporally_related_to")),
      IRI("http://purl.obolibrary.org/obo/RO_0002450") -> (Some("directly positively regulates activity of"), IRI(
        "https://w3id.org/biolink/vocab/entity_positively_regulates_entity")),
      IRI("http://purl.obolibrary.org/obo/RO_0002205") -> (Some("has gene product"), IRI(
        "https://w3id.org/biolink/vocab/has_gene_product")),
      IRI("http://purl.obolibrary.org/obo/RO_0002604") -> (Some("is opposite of"), IRI("https://w3id.org/biolink/vocab/opposite_of")),
      IRI("http://purl.obolibrary.org/obo/RO_0002448") -> (Some("directly regulates activity of"), IRI(
        "https://w3id.org/biolink/vocab/entity_regulates_entity")),
      IRI("http://purl.obolibrary.org/obo/RO_0002344") -> (Some("results in transport to from or in"), IRI(
        "https://w3id.org/biolink/vocab/related_to")),
      IRI("http://purl.obolibrary.org/obo/RO_0002232") -> (Some("has end location"), IRI("https://w3id.org/biolink/vocab/occurs_in")),
      IRI("http://purl.obolibrary.org/obo/RO_0002338") -> (Some("has target start location"), IRI(
        "https://w3id.org/biolink/vocab/related_to")),
      IRI("http://purl.obolibrary.org/obo/RO_0002592") -> (Some("results in organization of"), IRI(
        "https://w3id.org/biolink/vocab/affects")),
      IRI("http://purl.obolibrary.org/obo/RO_0002315") -> (Some("results in acquisition of features of"), IRI(
        "https://w3id.org/biolink/vocab/causes")),
      IRI("http://purl.obolibrary.org/obo/RO_0002234") -> (Some("has output"), IRI("https://w3id.org/biolink/vocab/has_output")),
      IRI("http://purl.obolibrary.org/obo/RO_0002327") -> (Some("enables"), IRI("https://w3id.org/biolink/vocab/enables")),
      IRI("http://purl.obolibrary.org/obo/RO_0002492") -> (Some("existence ends during"), IRI(
        "https://w3id.org/biolink/vocab/temporally_related_to")),
      IRI("http://purl.obolibrary.org/obo/RO_0002090") -> (Some("immediately precedes"), IRI("https://w3id.org/biolink/vocab/precedes")),
      IRI("http://purl.obolibrary.org/obo/RO_0002412") -> (Some("immediately causally upstream of"), IRI(
        "https://w3id.org/biolink/vocab/precedes")),
      IRI("http://purl.obolibrary.org/obo/RO_0002296") -> (Some("results in development of"), IRI(
        "https://w3id.org/biolink/vocab/has_output")),
      IRI("http://purl.obolibrary.org/obo/GOREL_0001006") -> (Some("acts_on_population_of"), IRI("https://w3id.org/biolink/vocab/affects")),
      IRI("http://purl.obolibrary.org/obo/RO_0002230") -> (Some("ends with"), IRI("https://w3id.org/biolink/vocab/temporally_related_to")),
      IRI("http://purl.obolibrary.org/obo/RO_0002299") -> (Some("results in maturation of"), IRI(
        "https://w3id.org/biolink/vocab/has_output")),
      IRI("http://purl.obolibrary.org/obo/RO_0002264") -> (Some("acts upstream of or within"), IRI(
        "https://w3id.org/biolink/vocab/acts_upstream_of_or_within_positive_effect")),
      IRI("http://purl.obolibrary.org/obo/RO_0002092") -> (Some("happens during"), IRI(
        "https://w3id.org/biolink/vocab/temporally_related_to")),
      IRI("http://purl.obolibrary.org/obo/RO_0002084") -> (Some("during which ends"), IRI("https://w3id.org/biolink/vocab/related_to")),
      IRI("http://purl.obolibrary.org/obo/RO_0002297") -> (Some("results in formation of"), IRI(
        "https://w3id.org/biolink/vocab/has_output")),
      IRI("http://purl.obolibrary.org/obo/RO_0004009") -> (Some("has primary input"), IRI("https://w3id.org/biolink/vocab/consumes")),
      IRI("http://purl.obolibrary.org/obo/RO_0002223") -> (Some("starts"), IRI("https://w3id.org/biolink/vocab/temporally_related_to")),
      IRI("http://purl.obolibrary.org/obo/RO_0002339") -> (Some("has target end location"), IRI(
        "https://w3id.org/biolink/vocab/related_to")),
      IRI("http://www.w3.org/2004/02/skos/core#broadMatch") -> (None, IRI("https://w3id.org/biolink/vocab/broad_match")),
      IRI("http://purl.obolibrary.org/obo/UPHENO_0000001") -> (Some("has phenotype affecting"), IRI(
        "https://w3id.org/biolink/vocab/affects")),
      IRI("http://purl.obolibrary.org/obo/RO_0002313") -> (Some("transports or maintains localization of"), IRI(
        "https://w3id.org/biolink/vocab/affects_transport_of")),
      IRI("http://purl.obolibrary.org/obo/RO_0004008") -> (Some("has primary output"), IRI("https://w3id.org/biolink/vocab/has_output")),
      IRI("http://purl.obolibrary.org/obo/RO_0002326") -> (Some("contributes to"), IRI("https://w3id.org/biolink/vocab/contributes_to")),
      IRI("http://purl.obolibrary.org/obo/BFO_0000050") -> (Some("part of"), IRI("https://w3id.org/biolink/vocab/part_of")),
      IRI("http://purl.obolibrary.org/obo/RO_0002220") -> (Some("adjacent to"), IRI("https://w3id.org/biolink/vocab/coexists_with")),
      IRI("http://purl.obolibrary.org/obo/RO_0001019") -> (Some("contains"), IRI("https://w3id.org/biolink/vocab/has_part")),
      IRI("http://purl.obolibrary.org/obo/RO_0002215") -> (Some("capable of"), IRI("https://w3id.org/biolink/vocab/capable_of")),
      IRI("http://purl.obolibrary.org/obo/RO_0002231") -> (Some("has start location"), IRI("https://w3id.org/biolink/vocab/occurs_in")),
      IRI("http://purl.obolibrary.org/obo/RO_0000053") -> (Some("has characteristic"), IRI("https://w3id.org/biolink/vocab/has_attribute")),
      IRI("http://purl.obolibrary.org/obo/RO_0002411") -> (Some("causally upstream of"), IRI("https://w3id.org/biolink/vocab/precedes")),
      IRI("http://purl.obolibrary.org/obo/RO_0004035") -> (Some("acts upstream of, negative effect"), IRI(
        "https://w3id.org/biolink/vocab/acts_upstream_of_negative_effect")),
      IRI("http://purl.obolibrary.org/obo/RO_0002213") -> (Some("positively regulates"), IRI(
        "https://w3id.org/biolink/vocab/positively_regulates")),
      IRI("http://purl.obolibrary.org/obo/RO_0002590") -> (Some("results in disassembly of"), IRI(
        "https://w3id.org/biolink/vocab/has_input")),
      IRI("http://purl.obolibrary.org/obo/RO_0002328") -> (Some("functionally related to"), IRI(
        "https://w3id.org/biolink/vocab/related_to")),
      IRI("http://purl.obolibrary.org/obo/RO_0002233") -> (Some("has input"), IRI("https://w3id.org/biolink/vocab/has_input"))
    )

    val initialQuerySolutions: List[QuerySolution] = List(
      createQuerySolutionMap(
        Map(
          "?n1_type" -> IRI("http://purl.obolibrary.org/obo/GO_0006094"),
          "?e0" -> IRI("http://purl.obolibrary.org/obo/RO_0000056"),
          "?n1" -> IRI("http://model.geneontology.org/R-HSA-70263/R-HSA-70263"),
          "?n0_type" -> IRI("http://purl.obolibrary.org/obo/CHEBI_15361"),
          "?n0" -> IRI("http://model.geneontology.org/R-ALL-113557_R-HSA-70501")
        )
      ),
      createQuerySolutionMap(
        Map(
          "?n1_type" -> IRI("http://purl.obolibrary.org/obo/GO_0046034"),
          "?e0" -> IRI("http://purl.obolibrary.org/obo/RO_0000056"),
          "?n1" -> IRI("http://model.geneontology.org/R-HSA-70263/R-HSA-70263"),
          "?n0_type" -> IRI("http://purl.obolibrary.org/obo/CHEBI_15361"),
          "?n0" -> IRI("http://model.geneontology.org/R-ALL-113557_R-HSA-70501")
        )
      ),
      createQuerySolutionMap(
        Map(
          "?n1_type" -> IRI("http://purl.obolibrary.org/obo/GO_0046034"),
          "?e0" -> IRI("http://purl.obolibrary.org/obo/RO_0000056"),
          "?n1" -> IRI("http://model.geneontology.org/R-HSA-73621/R-HSA-73621"),
          "?n0_type" -> IRI("http://purl.obolibrary.org/obo/CHEBI_15361"),
          "?n0" -> IRI("http://model.geneontology.org/R-ALL-113557_R-HSA-909776")
        )
      )
    )

    val queryGraph = TRAPIQueryGraph(
      nodes = Map(
        "n0" -> TRAPIQueryNode(
          ids = Some(List(IRI("http://purl.obolibrary.org/obo/CHEBI_15361"))),
          categories = Some(List(QueryService.BiolinkNamedThing)),
          is_set = None
        ),
        "n1" -> TRAPIQueryNode(
          ids = None,
          categories = Some(List(QueryService.BiolinkNamedThing)),
          is_set = None
        )
      ),
      edges = Map(
        "e0" -> TRAPIQueryEdge(
          predicates = Some(List(BiolinkPredicate("related_to", IRI("https://w3id.org/biolink/vocab/related_to")))),
          subject = "n0",
          `object` = "n1",
          attribute_constraints = None
        )
      )
    )

    // QueryService.extractCoreTriples(initialQuerySolutions, queryGraph)
    suite("testQueryServiceSteps")(
      zio.test.test("Test extractCoreTriples()") {
        val expectedCoreTriples = Set(
          Triple(
            subj = IRI("http://model.geneontology.org/R-ALL-113557_R-HSA-70501"),
            pred = IRI("http://purl.obolibrary.org/obo/RO_0000056"),
            obj = IRI("http://model.geneontology.org/R-HSA-70263/R-HSA-70263")
          ),
          Triple(
            subj = IRI("http://model.geneontology.org/R-ALL-113557_R-HSA-909776"),
            pred = IRI("http://purl.obolibrary.org/obo/RO_0000056"),
            obj = IRI("http://model.geneontology.org/R-HSA-73621/R-HSA-73621")
          )
        )

        assert(QueryService.extractCoreTriples(initialQuerySolutions, queryGraph))(Assertion.equalTo(expectedCoreTriples))
      },
      /*
      zio.test.test("Test getTRAPINodes()") {

      },
      zio.test.test("Test getTRAPIEdges()") {

      },
      zio.test.test("Test getTRAPIEdges()") {

      },
      zio.test.test("Test getTRAPIEdgeBindingsMany()") {

      },*/
      zio.test.testM("Test getTRAPINodeBindings()") {
        // List((Map(n0 -> List(TRAPINodeBinding(IRI(http://purl.obolibrary.org/obo/CHEBI_15361))), n1 -> List(TRAPINodeBinding(IRI(http://purl.obolibrary.org/obo/GO_0006094)))),Map(e0 -> List(TRAPIEdgeBinding(309229ebbc25b5b04fe79bf560d9ea20c1831703fa98605b4e97f78d73b47c56)))), (Map(n0 -> List(TRAPINodeBinding(IRI(http://purl.obolibrary.org/obo/CHEBI_15361))), n1 -> List(TRAPINodeBinding(IRI(http://purl.obolibrary.org/obo/GO_0046034)))),Map(e0 -> List(TRAPIEdgeBinding(3e27e38fc631bbfaac62c84dbe8e475895797f6fe663a852d916df24a0522339)))), (Map(n0 -> List(TRAPINodeBinding(IRI(http://purl.obolibrary.org/obo/CHEBI_15361))), n1 -> List(TRAPINodeBinding(IRI(http://purl.obolibrary.org/obo/GO_0046034>)))),Map(e0 -> List(TRAPIEdgeBinding(86cf45a0368b1fbf6dd93a9e6bdd1b6fcc030d53389d4e8b3e4d81e850ba0420)))))
        for {
          querySolutionsToEdgeBindings <- QueryService.getTRAPIEdgeBindingsMany(queryGraph,
                                                                                initialQuerySolutions,
                                                                                relationsToLabelAndBiolinkPredicate)
          _ = logger.warn(s"querySolutionsToEdgeBindings: $querySolutionsToEdgeBindings (length: ${querySolutionsToEdgeBindings.size})")
          trapiBindings <- ZIO.foreach(initialQuerySolutions) { querySolution =>
            QueryService.getTRAPINodeBindings(queryGraph, querySolution).zip(Task.effect(querySolutionsToEdgeBindings(querySolution)))
          }
          results = trapiBindings.map { case (resultNodeBindings, resultEdgeBindings) =>
            TRAPIResult(resultNodeBindings, resultEdgeBindings)
          }
        } yield
        // We should have three results
        assert(trapiBindings.length)(Assertion.equalTo(3)) &&
          // Each result should have two nodes and one edge
          assert(trapiBindings.flatMap(_._1.keys).distinct.toSet)(Assertion.equalTo(Set("n0", "n1"))) &&
          assert(trapiBindings.flatMap(_._2.keys).distinct.toSet)(Assertion.equalTo(Set("e0"))) &&
          // The results should also have three records.
          // assert(results)(Assertion.equalTo(List())) &&
          assert(results.length)(Assertion.equalTo(3)) &&
          // Three DISTINCT records.
          // TODO: we only get two at the moment, since two of them are identical.
          assert(results.distinct.length)(Assertion.equalTo(2))
      }
    )
  }

  val testQueryIds = {
    def createTestTRAPIQueryGraph(n0: TRAPIQueryNode,
                                  n1: TRAPIQueryNode =
                                    TRAPIQueryNode(None, categories = Some(List(BiolinkClass("BiologicalProcessOrActivity"))), None),
                                  e0: TRAPIQueryEdge = TRAPIQueryEdge(
                                    subject = "n0",
                                    `object` = "n1",
                                    predicates = Some(List(BiolinkPredicate("regulates"))),
                                    qualifier_constraints = Some(
                                      List(TRAPIQualifierConstraint(
                                        List(TRAPIQualifier("biolink:object_direction_qualifier", "downregulated")))))
                                  )) =
      TRAPIQueryGraph(Map("n0" -> n0, "n1" -> n1), Map("e0" -> e0))

    suite("testQueryIds")(
      testM("Ensure that query_id is absent for nodes without ids") {
        for {
          response <- QueryService
            .run(100, createTestTRAPIQueryGraph(TRAPIQueryNode(None, Some(List(BiolinkClass("BiologicalProcessOrActivity"))), None)))
          // _ = logger.warn(s"Response: ${response}")
          nodeBindings = response.message.results.get.flatMap(_.node_bindings.getOrElse("n0", List()))
          queryIds = nodeBindings.map(_.query_id)
        } yield assert(response.message.results)(Assertion.isSome(Assertion.hasSize(Assertion.equalTo(100)))) &&
          assert(queryIds)(Assertion.forall(Assertion.isNone))
      },
      testM(
        "Ensure that query_id is present only when the identifier is ambiguous for a process (GO:0033549, MAP kinase phosphatase activity)") {
        val iriToQuery = IRI("http://purl.obolibrary.org/obo/GO_0017017")

        val query = createTestTRAPIQueryGraph(TRAPIQueryNode(Some(List(iriToQuery)), None))
        for {
          response <- QueryService
            .run(100, query)
          _ = logger.warn(s"Response query_id is present only when the identifier is ambiguous query: ${query}")
          _ = logger.warn(s"Response query_id is present only when the identifier is ambiguous response: ${response}")
          nodeBindings = response.message.results.get.flatMap(_.node_bindings.getOrElse("n0", List()))
          queryIds = nodeBindings.map(_.query_id)
          queryIdsUnambiguous = nodeBindings.filter(_.id == iriToQuery).map(_.query_id)
          queryIdsAmbiguous = nodeBindings.filter(_.id != iriToQuery).map(_.query_id)
        } yield assert(response.message.results)(Assertion.isSome(Assertion.isNonEmpty)) &&
          assert(queryIds)(Assertion.isNonEmpty) &&
          assert(queryIdsUnambiguous)(Assertion.isNonEmpty) &&
          assert(queryIdsUnambiguous)(Assertion.forall(Assertion.isNone)) &&
          assert(queryIdsAmbiguous)(Assertion.isNonEmpty) &&
          assert(queryIdsAmbiguous)(Assertion.forall(Assertion.isSome(Assertion.equalTo(iriToQuery))))
      },
      testM("Ensure that query_id is present only when the identifier is ambiguous for a location (GO:0005737, cytoplasm)") {
        val cytoplasm = IRI("http://purl.obolibrary.org/obo/GO_0005737")
        val germplasm = IRI("http://purl.obolibrary.org/obo/GO_0060293")

        for {
          response <- QueryService
            .run(
              100,
              createTestTRAPIQueryGraph(
                TRAPIQueryNode(Some(List(cytoplasm)), None),
                TRAPIQueryNode(None, categories = Some(List(BiolinkClass("BiologicalProcessOrActivity"))), None),
                TRAPIQueryEdge(
                  subject = "n1",
                  `object` = "n0",
                  predicates = Some(List(BiolinkPredicate("occurs_in")))
                )
              )
            )
          // _ = logger.warn(s"Response: ${response}")
          node0Bindings = response.message.results.get.flatMap(_.node_bindings.getOrElse("n0", List()))
          // _ = logger.warn(s"Node bindings for n0 in GO:0005737, cytoplasm query: ${node0Bindings}")
          queryIds = node0Bindings.map(_.query_id)
          queryIdsUnambiguous = node0Bindings.filter(_.id == cytoplasm).map(_.query_id)
          queryIdsAmbiguous = node0Bindings.filter(_.id != cytoplasm).map(_.query_id)
        } yield assert(response.message.results)(Assertion.isSome(Assertion.isNonEmpty)) &&
          assert(queryIds)(Assertion.isNonEmpty) &&
          assert(queryIdsUnambiguous)(Assertion.isNonEmpty) &&
          assert(queryIdsUnambiguous)(Assertion.forall(Assertion.isNone)) &&
          assert(queryIdsAmbiguous)(Assertion.isNonEmpty) &&
          assert(queryIdsAmbiguous)(Assertion.forall(Assertion.isSome(Assertion.equalTo(cytoplasm)))) &&
          // If this works, some of the ambiguous results should be for germplasm, which is a kind of cytoplasm
          assert(node0Bindings)(Assertion.contains(TRAPINodeBinding(id = germplasm, query_id = Some(cytoplasm))))
      },
      testM("Ensure that two different identifiers can be provided for the same node, to be disambiguated by query_id") {
        val glucose = IRI("http://purl.obolibrary.org/obo/CHEBI_17925")
        val rna = IRI("http://purl.obolibrary.org/obo/CHEBI_33697")

        val query = createTestTRAPIQueryGraph(
          TRAPIQueryNode(Some(List(glucose, rna)), None),
          TRAPIQueryNode(None, categories = Some(List(BiolinkClass("BiologicalProcessOrActivity"))), None),
          TRAPIQueryEdge(
            subject = "n1",
            `object` = "n0",
            predicates = Some(List(BiolinkPredicate("has_participant")))
          )
        )
        logger.debug(f"Glucose/RNA query: ${query}")
        for {
          response <- QueryService
            .run(
              10,
              query
            )
          // _ = logger.warn(s"Response: ${response}")
          node0Bindings = response.message.results.get.flatMap(_.node_bindings.getOrElse("n0", List()))
          queryIdsUnambiguous = node0Bindings.filter(b => b.id == glucose || b.id == rna).map(_.query_id)
          idsAmbiguousGlucose = node0Bindings.filter(_.query_id.contains(glucose)).map(_.id)
          idsAmbiguousRNA = node0Bindings.filter(_.query_id.contains(rna)).map(_.id)
        } yield assert(response.message.results)(Assertion.isSome(Assertion.hasSize(Assertion.isGreaterThanEqualTo(10)))) &&
          // All unambiguous IDs -- glucose and RNA -- should have empty query_ids.
          assert(queryIdsUnambiguous)(Assertion.isNonEmpty) &&
          assert(queryIdsUnambiguous)(Assertion.forall(Assertion.isNone)) &&
          // Ambiguous IDs that came from glucose should not have either glucose or RNA in them.
          assert(idsAmbiguousGlucose)(Assertion.isNonEmpty) &&
          assert(idsAmbiguousGlucose)(Assertion.not(Assertion.contains(TRAPINodeBinding(id = glucose, query_id = Some(glucose))))) &&
          assert(idsAmbiguousGlucose)(Assertion.not(Assertion.contains(TRAPINodeBinding(id = rna, query_id = Some(glucose))))) &&
          // Ambiguous IDs that came from RNA should not have either DNA or RNA in them.
          assert(idsAmbiguousRNA)(Assertion.isNonEmpty) &&
          assert(idsAmbiguousRNA)(Assertion.not(Assertion.contains(TRAPINodeBinding(id = glucose, query_id = Some(rna))))) &&
          assert(idsAmbiguousRNA)(Assertion.not(Assertion.contains(TRAPINodeBinding(id = rna, query_id = Some(rna)))))
      }
    )
  }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val testLayer = HttpClient.makeHttpClientLayer ++ Biolink.makeUtilitiesLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer

  def spec = suite("QueryService tests")(
//    testGetNodeTypes,
    testEnforceQueryEdgeTypes,
    testGetTRAPINodeBindings,
    testQueryTexts,
    testGetNodesToDirectTypes,
    testGetProjections,
    testQueryServiceSteps,
    testQueryIds
  ).provideCustomLayer(testLayer.mapError(TestFailure.die))

}

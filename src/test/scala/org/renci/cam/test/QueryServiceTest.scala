package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.IOUtils
import org.apache.jena.query.{ResultSet, ResultSetFactory}
import org.renci.cam._
import org.renci.cam.domain._
import zio._
import zio.config.typesafe.TypesafeConfig
import zio.test.Assertion._
import zio.test.{testM, _}

import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters._

object QueryServiceTest extends DefaultRunnableSpec with LazyLogging {

  val testLayer = HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer >+> TypesafeConfig.fromDefaultLoader(AppConfig.config)

  val testGetNodeTypes = suite("testGetNodeTypes")(
    testM("test get node types sans id") {
      val n0Node = TRAPIQueryNode(None, Some(BiolinkClass("Gene")), None)
      val n1Node = TRAPIQueryNode(None, Some(BiolinkClass("BiologicalProcess")), None)
      val nodeMap = Map("n0" -> n0Node, "n1" -> n1Node)
      for {
        nodeTypes <- ZIO.effect(QueryService.getNodeTypes(nodeMap))
      } yield assert(nodeTypes)(hasKey("n0")) && assert(nodeTypes.get("n0").get)(equalTo(BiolinkClass("Gene").iri))
    },
    testM("test get node types with id") {
      val n0Node = TRAPIQueryNode(Some(IRI("http://www.ncbi.nlm.nih.gov/gene/558")), Some(BiolinkClass("Gene")), None)
      val n1Node = TRAPIQueryNode(None, Some(BiolinkClass("BiologicalProcess")), None)
      val nodeMap = Map("n0" -> n0Node, "n1" -> n1Node)
      for {
        nodeTypes <- ZIO.effect(QueryService.getNodeTypes(nodeMap))
      } yield assert(nodeTypes)(hasKey("n0")) && assert(nodeTypes.get("n0").get)(equalTo(IRI("http://www.ncbi.nlm.nih.gov/gene/558")))
    },
    testM("test get node types with nothing") {
      val n0Node = TRAPIQueryNode(None, None, None)
      val n1Node = TRAPIQueryNode(None, None, None)
      val nodeMap = Map("n0" -> n0Node, "n1" -> n1Node)
      for {
        nodeTypes <- ZIO.effect(QueryService.getNodeTypes(nodeMap))
      } yield assert(nodeTypes)(isEmpty)
    }
  )

  val testEnforceQueryEdgeTypes = suite("testEnforceQueryEdgeTypes")(
    testM("test QueryService.enforceQueryEdgeTypes") {
      val n0Node = TRAPIQueryNode(None, Some(BiolinkClass("Gene")), None)
      val n1Node = TRAPIQueryNode(None, Some(BiolinkClass("BiologicalProcess")), None)
      val e0Edge = TRAPIQueryEdge("n1", "n0", None, None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      for {
        nodeTypes <- ZIO.effect(QueryService.enforceQueryEdgeTypes(queryGraph))
      } yield assert(nodeTypes.edges)(hasKey("e0")) && assert(nodeTypes.edges.get("e0").get.predicate.get)(
        equalTo(BiolinkPredicate("related_to")))
    }
  )

  val testGetTRAPIEdges = suite("testGetTRAPIEdges")(
    testM("test QueryService.getTRAPIEdges") {
      val (queryGraph, resultSet) = getSimpleData
      val testCase =
        for {
          trapiEdges <- QueryService.getTRAPIEdges(queryGraph, resultSet.asScala.toList)
        } yield assert(trapiEdges.values.map(a => (a.subject, a.`object`)))(
          contains((IRI("http://purl.obolibrary.org/obo/GO_0008150"),
                    IRI("http://purl.obolibrary.org/obo/go/extensions/reacto.owl#REACTO_R-HSA-166103"))))
      testCase.provideCustomLayer(testLayer)
    }
  )

  val testGetTRAPINodeBindings = suite("testGetTRAPINodeBindings")(
    testM("test QueryService.getTRAPINodeBindings") {
      val (queryGraph, resultSet) = getSimpleData
      val testCase =
        for {
          nodeBindings <- QueryService.getTRAPINodeBindings(queryGraph, resultSet.next())
        } yield assert(nodeBindings.keys)(
          contains("n0") && contains("n1")
        ) && assert(nodeBindings.get("n0").get.map(a => a.id))(
          contains(IRI("http://purl.obolibrary.org/obo/go/extensions/reacto.owl#REACTO_R-HSA-166103")))
      testCase.provideCustomLayer(testLayer)
    }
  )

  def getSimpleData: (TRAPIQueryGraph, ResultSet) = {
    val n0Node = TRAPIQueryNode(None, Some(BiolinkClass("Gene")), None)
    val n1Node = TRAPIQueryNode(None, Some(BiolinkClass("BiologicalProcess")), None)
    val e0Edge = TRAPIQueryEdge("n1", "n0", None, None)
    val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))

    val response = """
        {
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
      val testCase =
        for {
          biolinkData <- Biolink.biolinkData
          nodeIdList = List(
            IRI("http://purl.obolibrary.org/obo/GO_0047196"),
            IRI("http://purl.obolibrary.org/obo/GO_0017064")
          )
          queryText <- QueryService.getTRAPINodeDetailsQueryText(nodeIdList, biolinkData.classes)
        } yield assert(queryText.text)(
          containsString("VALUES ?term {  <http://purl.obolibrary.org/obo/GO_0047196>  <http://purl.obolibrary.org/obo/GO_0017064>  }"))
      testCase.provideCustomLayer(testLayer)
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
        queryText <- QueryService.getProvenanceQueryText(edges)
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
        queryText <- QueryService.getCAMStuffQueryText(IRI("http://model.geneontology.org/R-HSA-2142753"))
      } yield assert(queryText.text)(containsString("{ GRAPH <http://model.geneontology.org/R-HSA-2142753>"))
    },
    testM("test QueryService.getSlotStuffQueryText") {
      for {
        predicates <- Task.effect(
          List(
            IRI("http://purl.obolibrary.org/obo/RO_0002333"),
            IRI("http://purl.obolibrary.org/obo/BFO_0000066"),
            IRI("http://purl.obolibrary.org/obo/RO_0002234"),
            IRI("http://purl.obolibrary.org/obo/RO_0002413"),
            IRI("http://purl.obolibrary.org/obo/BFO_0000050"),
            IRI("http://purl.obolibrary.org/obo/RO_0002411"),
            IRI("http://purl.obolibrary.org/obo/RO_0002233")
          )
        )
        queryText <- QueryService.getSlotStuffQueryText(predicates)
      } yield assert(queryText.text)(
        containsString("( <http://purl.obolibrary.org/obo/RO_0002333> \"e0000\" )") &&
          containsString("( <http://purl.obolibrary.org/obo/BFO_0000066> \"e0001\" )") &&
          containsString("( <http://purl.obolibrary.org/obo/RO_0002234> \"e0002\" )") &&
          containsString("( <http://purl.obolibrary.org/obo/RO_0002413> \"e0003\" )") &&
          containsString("( <http://purl.obolibrary.org/obo/BFO_0000050> \"e0004\" )") &&
          containsString("( <http://purl.obolibrary.org/obo/RO_0002411> \"e0005\" )") &&
          containsString("( <http://purl.obolibrary.org/obo/RO_0002233> \"e0006\" )")
      )
    },
    testM("test QueryService.getTRAPIQEdgePredicatesQueryText") {
      for {
        queryText <- QueryService.getTRAPIQEdgePredicatesQueryText(BiolinkPredicate("has_participant").iri)
      } yield assert(queryText.text)(
        containsString("?predicate <http://cam.renci.org/biolink_slot> <https://w3id.org/biolink/vocab/has_participant> ."))
    }
  )

  val testGetNodesToDirectTypes = suite("testGetNodesToDirectTypes")(
    zio.test.test("test QueryService.getNodesToDirectTypes") {
      val (queryGraph, _) = getSimpleData
      val queryText = QueryService.getNodesToDirectTypes(queryGraph.nodes)
      assert(queryText.text.trim)(equalTo(
        "?n0 <http://www.openrdf.org/schema/sesame#directType> ?n0_type .   ?n1 <http://www.openrdf.org/schema/sesame#directType> ?n1_type ."))
    }
  )

  val testGetLimit = suite("testGetLimit")(
    zio.test.test("test QueryService.getLimit") {
      val queryText = QueryService.getLimit(Some(10))
      assert(queryText.text.trim)(equalTo("LIMIT 10"))
    }
  )

  val testGetProjections = suite("testGetProjections")(
    zio.test.test("test QueryService.getProjections") {
      val (queryGraph, _) = getSimpleData
      val queryText = QueryService.getProjections(queryGraph)
      assert(queryText.text.trim)(equalTo("?e0  ?n1  ?n0  ?n0_type  ?n1_type"))
    }
  )

  def spec = suite("QueryService tests")(
    testGetNodeTypes,
    testEnforceQueryEdgeTypes,
    testGetTRAPIEdges,
    testGetTRAPINodeBindings,
    testQueryTexts,
    testGetNodesToDirectTypes,
    testGetLimit,
    testGetProjections
  )

}

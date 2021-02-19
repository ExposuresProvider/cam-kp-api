package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.io.IOUtils
import org.apache.jena.query.ResultSetFactory
import org.renci.cam._
import org.renci.cam.domain._
import zio._
import zio.config.typesafe.TypesafeConfig
import zio.test.Assertion._
import zio.test._

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
    testM("test enforcing QueryEdge types") {
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
      val testCase =
        for {
          trapiEdges <- QueryService.getTRAPIEdges(queryGraph, resultSet.asScala.toList)
          _ = logger.info("trapiEdges: {}", trapiEdges)
        } yield assert(trapiEdges.values.map(a => (a.subject, a.`object`)))(
          contains((IRI("http://purl.obolibrary.org/obo/GO_0008150"),
                    IRI("http://purl.obolibrary.org/obo/go/extensions/reacto.owl#REACTO_R-HSA-166103"))))
      testCase.provideCustomLayer(testLayer)
    }
  )

  def spec = suite("All tests")(testGetNodeTypes, testEnforceQueryEdgeTypes, testGetTRAPIEdges)

}

package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import org.renci.cam.QueryService
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, IRI, TRAPIQueryEdge, TRAPIQueryGraph, TRAPIQueryNode}
import zio.ZIO
import zio.test.Assertion.{contains, containsString, equalTo, hasKey, isEmpty}
import zio.test.{DefaultRunnableSpec, assert, suite, testM}

object QueryServiceTest extends DefaultRunnableSpec with LazyLogging {

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

  def spec = suite("All tests")(testGetNodeTypes)

}

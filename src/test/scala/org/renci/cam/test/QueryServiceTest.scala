package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import org.renci.cam.QueryService
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, IRI, TRAPIQueryEdge, TRAPIQueryGraph, TRAPIQueryNode}
import zio.ZIO
import zio.test.Assertion.{contains, containsString, equalTo, hasKey}
import zio.test.{DefaultRunnableSpec, assert, suite, testM}

object QueryServiceTest extends DefaultRunnableSpec with LazyLogging {

  val testGetNodeTypes = suite("testGetNodeTypes")(
    testM("test get node types") {
      val n0Node = TRAPIQueryNode(None /*Some(IRI("http://www.ncbi.nlm.nih.gov/gene/558"))*/, Some(BiolinkClass("Gene")), None)
      val n1Node = TRAPIQueryNode(None, Some(BiolinkClass("BiologicalProcess")), None)
      val nodeMap = Map("n0" -> n0Node, "n1" -> n1Node)
      for {
        nodeTypes <- ZIO.effect(QueryService.getNodeTypes(nodeMap))
      } yield assert(nodeTypes)(hasKey("n0")) && assert(nodeTypes.get("n0"))(equalTo(Some(BiolinkClass("Gene").iri)))
    }
  )

  def spec = suite("All tests")(testGetNodeTypes)

}

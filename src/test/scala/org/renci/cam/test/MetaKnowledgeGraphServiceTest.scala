package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import org.renci.cam.MetaKnowledgeGraphService
import zio.test.Assertion.isNonEmpty
import zio.test._

object MetaKnowledgeGraphServiceTest extends DefaultRunnableSpec with LazyLogging {

  val testGetEdges = suite("MetaKnowledgeGraphService.getEdges")(
    testM("test MetaKnowledgeGraphService.getEdges") {
      for {
        edges <- MetaKnowledgeGraphService.getEdges
      } yield assert(edges)(isNonEmpty)
    }
  )

  val testGetNodes = suite("MetaKnowledgeGraphService.getNodes")(
    testM("test MetaKnowledgeGraphService.getNodes") {
      for {
        nodes <- MetaKnowledgeGraphService.getNodes
//        _ = logger.info("nodes: {}", nodes)
      } yield assert(nodes)(isNonEmpty)
    }
  )

  def spec = suite("MetaKnowledgeGraphService tests")( testGetEdges, testGetNodes )

}

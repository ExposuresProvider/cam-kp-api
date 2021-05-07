package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import zio.test._

object MetaKnowledgeGraphServiceTest extends DefaultRunnableSpec with LazyLogging {

//  val testReadMetaKnowledgeGraph = suite("readMetaKnowledgeGraph")(
//    testM("test MetaKnowledgeGraphService.readMetaKnowledgeGraph") {
//      for {
//        predicates <- MetaKnowledgeGraphService.readPredicates
//      } yield assert(predicates)(isNonEmpty) && assert(predicates.keys)(contains(BiolinkClass("MacromolecularMachine")))
//    }
//  )

  def spec = suite("MetaKnowledgeGraphService tests")( /*testReadMetaKnowledgeGraph*/ )

}

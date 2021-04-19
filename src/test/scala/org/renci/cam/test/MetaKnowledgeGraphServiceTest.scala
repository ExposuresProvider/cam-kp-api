package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import io.circe.{Decoder, Encoder, parser}
import org.renci.cam._
import org.renci.cam.domain._
import zio.test.Assertion._
import zio.test._

import java.nio.file.{Files, Paths}

object MetaKnowledgeGraphServiceTest extends DefaultRunnableSpec with LazyLogging {

  val testReadPredicates = suite("readPredicates")(
    testM("test PredicatesService.readPredicates") {
      for {
        predicates <- MetaKnowledgeGraphService.readPredicates
      } yield assert(predicates)(isNonEmpty) && assert(predicates.keys)(contains(BiolinkClass("MacromolecularMachine")))
    }
  )

  def spec = suite("PredicateService tests")(testReadPredicates)

}

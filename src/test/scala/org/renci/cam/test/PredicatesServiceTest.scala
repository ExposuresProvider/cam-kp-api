package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import org.renci.cam._
import org.renci.cam.domain._
import zio.test.Assertion._
import zio.test._

object PredicatesServiceTest extends DefaultRunnableSpec with LazyLogging {

  val testReadPredicates = suite("readPredicates")(
    testM("test PredicatesService.readPredicates") {
      for {
        predicates <- PredicatesService.readPredicates
      } yield assert(predicates)(isNonEmpty) && assert(predicates.map(a => a.obj))(contains(BiolinkClass("MacromolecularMachine")))
    }
  )

  def spec = suite("All tests")(testReadPredicates)

}

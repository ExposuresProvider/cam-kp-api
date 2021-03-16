package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import org.renci.cam.Biolink
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate}
import zio.test.Assertion._
import zio.test.{assert, suite, testM, DefaultRunnableSpec}

object BiolinkTest extends DefaultRunnableSpec with LazyLogging {

  val testLocalPrefixes = suite("localPrefixes")(
    testM("test Biolink.localPrefixes") {
      for {
        prefixes <- Biolink.localPrefixes
      } yield assert(prefixes)(isNonEmpty) && assert(prefixes.keys)(contains("sesame"))
    }
  )

  val testGetBiolinkPrefixesFromFile = suite("getBiolinkPrefixesFromFile")(
    testM("test Biolink.getBiolinkPrefixesFromFile") {
      for {
        prefixes <- Biolink.getBiolinkPrefixesFromFile
      } yield assert(prefixes)(isNonEmpty) && assert(prefixes.keys.size)(isGreaterThan(190))
    }
  )

  val testGetBiolinkPrefixesAndClassesAndPredicatesFromFile = suite("getBiolinkPrefixesAndClassesAndPredicatesFromFile")(
    testM("test Biolink.getBiolinkPrefixesAndClassesAndPredicatesFromFile") {
      for {
        (prefixes, classes, predicates) <- Biolink.getBiolinkPrefixesAndClassesAndPredicatesFromFile
      } yield assert(prefixes)(isNonEmpty) && assert(prefixes.keys)(contains("GOREL")) && assert(classes)(isNonEmpty) && assert(classes)(
        contains(BiolinkClass("BiologicalEntity"))) && assert(predicates)(isNonEmpty) && assert(predicates)(
        contains(BiolinkPredicate("related_to")))
    }
  )

  def spec =
    suite("Biolink tests")(testLocalPrefixes, testGetBiolinkPrefixesFromFile, testGetBiolinkPrefixesAndClassesAndPredicatesFromFile)

}

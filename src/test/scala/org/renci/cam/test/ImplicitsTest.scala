package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import io.circe.Encoder
import io.circe.syntax._
import org.renci.cam.Biolink.BiolinkData
import org.renci.cam.domain._
import org.renci.cam.test.SerializationTest.{suite, testM}
import org.renci.cam.{AppConfig, Biolink, HttpClient, Implicits}
import zio.{Has, URIO, ZIO}
import zio.config.typesafe.TypesafeConfig
import zio.test.Assertion._
import zio.test._

object ImplicitsTest extends DefaultRunnableSpec {

  val testBiolinkClassEncoder = suite("testBiolinkClassEncoder")(
    test("test Implicits.biolinkClassEncoder") {
      val bc = BiolinkClass("MacromolecularMachine")
      val json = bc.asJson(Implicits.biolinkClassEncoder).deepDropNullValues.noSpaces.replace("\"", "")
      assert(json)(equalTo("biolink:MacromolecularMachine"))
    }
  )

  val testIRIEncoder = suite("testIRIEncoder")(
    test("test Implicits.iriEncoder") {
      val iri = IRI("http://identifiers.org/ncbigene/558")
      val prefixes: Map[String, String] = Map("NCBIGENE" -> "http://identifiers.org/ncbigene/")
      val json = iri.asJson(Implicits.iriEncoder(prefixes)).deepDropNullValues.noSpaces.replace("\"", "")
      assert(json)(equalTo("NCBIGENE:558"))
    }
  )

  def spec = suite("Implicits tests")(testBiolinkClassEncoder, testIRIEncoder) @@ TestAspect.sequential

}

package org.renci.cam.test

import io.circe.Encoder
import io.circe.syntax._
import org.renci.cam.Implicits
import org.renci.cam.domain._
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
      val prefixes: Map[String, String] = Map("NCBIGene" -> "http://identifiers.org/ncbigene/")
      val json = iri.asJson(Implicits.iriEncoder(prefixes)).deepDropNullValues.noSpaces.replace("\"", "")
      assert(json)(equalTo("NCBIGene:558"))
    }
  )

  val testMetaNodeEncoder = suite("testMetaNodeEncoder")(
    test("test Implicits.metaNodeEncoder") {
      val metaNode = MetaNode(BiolinkClass("MacromolecularMachine"), List("NCBI", "CHEBI"), None)
      implicit val metaNodeEncoder: Encoder[MetaNode] = Implicits.metaNodeEncoder
      val json = metaNode.asJson(metaNodeEncoder).deepDropNullValues.noSpaces.replace("\"", "")
      assert(json)(equalTo("NCBIGene:558"))
    }
  )

  def spec = suite("Implicits tests")(testBiolinkClassEncoder, testIRIEncoder, testMetaNodeEncoder) @@ TestAspect.sequential

}

package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.renci.cam.domain._
import zio._
import zio.test.Assertion._
import zio.test._

object Scratch extends DefaultRunnableSpec with LazyLogging {

  val scratch = suite("ScratchSpec")(
    testM("1") {
      for {
        list <- ZIO.effect(List(1, 2, 3))
      } yield assert(list)(equalTo(List(1, 2, 3)))
    } //@@ ignore
  )

  val printEncoded = suite("printEncoded")(
    testM("print encoded") {
      val expected = """{["n0":{"id":"NCBIGene:558", "category":"biolink:Gene"}, "n1":{"category":"biolink:BiologicalProcess"}]}"""
      println("expected: " + expected)

      val n0Node = TRAPIQueryNode(Some(IRI("http://www.ncbi.nlm.nih.gov/gene/558")), Some(BiolinkClass("gene")), None)
      val n1Node = TRAPIQueryNode(None, Some(BiolinkClass("biological_process")), None)

      for {
        nodes <- ZIO.effect(Map("n0" -> n0Node, "n1" -> n1Node))
      } yield {
        implicit val encodeFoo: Encoder[TRAPIQueryNode] = new Encoder[TRAPIQueryNode] {
          final def apply(a: TRAPIQueryNode): Json = Json.obj(
            ("id", Json.fromString(a.id.get.value)),
            ("category", Json.fromString(a.category.get.withBiolinkPrefix))
          )
        }

        println(n0Node.asJson.deepDropNullValues.noSpaces)
        assert(nodes)(isNonEmpty)
      }
    } //@@ ignore
  )

  def spec = suite("All tests")(scratch, printEncoded)
}

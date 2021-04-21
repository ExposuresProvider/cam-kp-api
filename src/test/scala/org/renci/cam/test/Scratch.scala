package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import io.circe.syntax._
import io.circe.{Encoder, Json}
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json, KeyDecoder, KeyEncoder}
import org.renci.cam.Implicits
import org.renci.cam.domain._
import zio.ZIO
import zio.test.Assertion._
import zio.test.TestAspect.ignore
import zio.test._

object Scratch extends DefaultRunnableSpec with LazyLogging {

  val scratch = suite("ScratchSpec")(
    testM("1") {
      for {
        list <- ZIO.effect(List(1, 2, 3))
      } yield assert(list)(equalTo(List(1, 2, 3)))
    } @@ ignore
  )

  val printEncoded = suite("printEncoded")(
    testM("print encoded") {
      val expected = """{["n0":{"id":"NCBIGene:558", "category":"biolink:Gene"}, "n1":{"category":"biolink:BiologicalProcess"}]}"""
      println("expected: " + expected)

      val n0Node = TRAPIQueryNode(Some(List(IRI("http://www.ncbi.nlm.nih.gov/gene/558"))), Some(List(BiolinkClass("gene"))), None)
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("BiologicalProcess"))), None)

      for {
        nodes <- ZIO.effect(Map("n0" -> n0Node, "n1" -> n1Node))
      } yield {
        implicit val encodeFoo: Encoder[TRAPIQueryNode] = new Encoder[TRAPIQueryNode] {
          final def apply(a: TRAPIQueryNode): Json = Json.obj(
            ("id", Json.fromString(a.ids.get.head.value)),
            ("category", Json.fromString(a.categories.get.head.withBiolinkPrefix))
          )
        }

        println(n0Node.asJson.deepDropNullValues.noSpaces)
        assert(nodes)(isNonEmpty)
      }
    } @@ ignore
  )


  def spec = suite("Scratch tests")(scratch, printEncoded)
}

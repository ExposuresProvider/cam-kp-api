package org.renci.cam

import com.typesafe.scalalogging.LazyLogging
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.renci.cam.domain.{BiolinkClass, IRI, TRAPIQueryNode}
import zio._
import zio.test.Assertion._
import zio.test._

import scala.io.Source

object Scratch extends DefaultRunnableSpec with LazyLogging {

//  val decoder = Decoder.decodeMap(KeyDecoder.decodeKeyString, Decoder.decodeString)

  val scratch = suite("ScratchSpec")(
    testM("1") {
      for {
        list <- ZIO.effect(List(1, 2, 3))
      } yield assert(list)(equalTo(List(1, 2, 3)))
    } //@@ ignore
  )

  val parseBiolinkYAML = suite("parse biolink yaml")(
    testM("parse biolink yaml") {
      for {
        source <- Task.effect(Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("biolink-model.yaml")).mkString)
        json <- ZIO.fromEither(io.circe.yaml.parser.parse(source))
        slotKeys <- ZIO.fromOption(json.hcursor.downField("slots").keys).orElseFail(throw new Exception("couldn't get slots"))
        slots = slotKeys.map(a => a.replaceAll(" ", "_")).toList
        _ = logger.info("slots: {}", slots)
        prefixes <- ZIO.fromOption(json.hcursor.downField("prefixes").focus).orElseFail(throw new Exception("couldn't get slots"))
        prefixesMap <- ZIO.fromEither(prefixes.as[Map[String, String]])
        _ = logger.info("prefixesMap: {}", prefixesMap)
//        slots = slotsFromCursor.flatMap(_.hcursor.focus.get.toString())

//        asdf <- ZIO.fromOption().orElseFail(throw new Exception("asdfasd"))
//        qwer <- ZIO.fromEither(asdf.as[List[String]])

      } yield assert(List(1, 2, 3))(equalTo(List(1, 2, 3)))
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

  def spec = suite("All tests")(scratch, parseBiolinkYAML, printEncoded)
}

package org.renci.cam

import com.typesafe.scalalogging.LazyLogging
import io.circe.yaml.parser
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
        json <- ZIO.fromEither(parser.parse(source))
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

  def spec = suite("All tests")(scratch, parseBiolinkYAML)
}

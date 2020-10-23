package org.renci.cam

import com.typesafe.scalalogging.LazyLogging
import io.circe.{Decoder, KeyDecoder}

import scala.io.Source
import io.circe.yaml.parser
import zio._
import zio.test.Assertion._
import zio.test._

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
//        slots = slotsFromCursor.flatMap(_.hcursor.focus.get.toString())

//        asdf <- ZIO.fromOption().orElseFail(throw new Exception("asdfasd"))
//        qwer <- ZIO.fromEither(asdf.as[List[String]])
        _ = logger.info("slots: {}", slots)

//val itemsFromCursor: Vector[Json] = json.hcursor.
//      downField("order").
//      downField("items").
//      focus.
//      flatMap(_.asArray).
//      getOrElse(Vector.empty)
//        asdf <- json.mapObject(a => a.)
//        is -> Task.effect(IOUtils.toInputStream(getClass.getClassLoader.getResourceAsStream("biolink-model.yaml"), StandardCharsets.UTF_8))
//        resultSet = ResultSetFactory.fromJSON(is)
//        is.close()


      } yield assert(List(1, 2, 3))(equalTo(List(1, 2, 3)))
    } //@@ ignore
  )

  def spec = suite("All tests")(scratch, parseBiolinkYAML)
}

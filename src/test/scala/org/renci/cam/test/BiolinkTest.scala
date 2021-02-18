package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import zio.{Task, ZIO}
import zio.test.Assertion.contains
import zio.test.{DefaultRunnableSpec, assert, suite, testM}

import scala.io.Source

object BiolinkTest extends DefaultRunnableSpec with LazyLogging {

  val parseBiolinkYAML = suite("parse biolink yaml")(
    testM("parse biolink yaml") {
      for {
        source <- Task.effect(Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("biolink-model.yaml")).mkString)
        json <- ZIO.fromEither(io.circe.yaml.parser.parse(source))
        slotKeys <- ZIO.fromOption(json.hcursor.downField("slots").keys).orElseFail(throw new Exception("couldn't get slots"))
        slots = slotKeys.map(a => a.replaceAll(" ", "_")).toList
        prefixes <- ZIO.fromOption(json.hcursor.downField("prefixes").focus).orElseFail(throw new Exception("couldn't get slots"))
        prefixesMap <- ZIO.fromEither(prefixes.as[Map[String, String]])
      } yield assert(slots)(contains("related_to")) && assert(prefixesMap.keys)(contains("GOREL"))
    }
  )

  def spec = suite("All tests")(parseBiolinkYAML)

}

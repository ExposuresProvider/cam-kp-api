package org.renci.cam

import java.io.StringReader
import java.nio.file.{Files, Paths}

import io.circe.syntax._
import io.circe.{Encoder, KeyEncoder}
import org.apache.commons.csv.CSVFormat
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate}
import zio.Task
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import scala.jdk.CollectionConverters._

object PredicateServiceTest extends DefaultRunnableSpec {

  val testLayer = HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer

  val mapTest = suite("map test")(
    testM("map test") {
      val testCase = for {
        biolinkData <- Biolink.biolinkData
        predicates <- Task.effect(Files.readString(Paths.get(getClass.getClassLoader.getResource("predicates.csv").toURI)))
        records = CSVFormat.DEFAULT.parse(new StringReader(predicates)).getRecords
        triples = records.asScala
          .map(a => PredicatesService.Triple(BiolinkClass(a.get(0)), BiolinkPredicate(a.get(1)), BiolinkClass(a.get(2))))
          .toList
        map = triples.groupBy(_.subj).view.mapValues(_.groupBy(_.obj).view.mapValues(_.map(_.pred)).toMap).toMap
        encoded = {
          implicit val blPredicateEncoder: Encoder[BiolinkPredicate] = Encoder.encodeString.contramap(predicate => predicate.shorthand)
          implicit val blClassKeyEncoder: KeyEncoder[BiolinkClass] = (blClass: BiolinkClass) => blClass.shorthand
          map.asJson.noSpaces
        }
//        _ = println("encoded: " + encoded)
        _ = Files.writeString(Paths.get("src/test/resources/predicates.json"), encoded)
      } yield assert(map)(isNonEmpty)
      testCase.provideCustomLayer(testLayer)
    } //@@ ignore
  )

  def spec = suite("All tests")(mapTest)

}

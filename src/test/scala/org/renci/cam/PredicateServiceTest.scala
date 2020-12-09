package org.renci.cam

import java.io.StringReader
import java.nio.file.{Files, Paths}

import io.circe.{Encoder, KeyEncoder}
import io.circe.generic.auto._
import io.circe.syntax._
import org.apache.commons.csv.CSVFormat
import org.apache.commons.lang3.StringUtils
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, IRI}
import zio.Task
import zio.test.Assertion.isNonEmpty
import zio.test._

import scala.collection.JavaConverters._

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
        map = triples.groupBy(_.subj).mapValues(_.groupBy(_.obj).mapValues(_.map(_.pred)))
        encoded = {
          implicit val blPredicateEncoder: Encoder[BiolinkPredicate] = Encoder.encodeString.contramap { predicate => predicate.shorthand }
          implicit val blClassKeyEncoder = new KeyEncoder[BiolinkClass] {
            override def apply(blClass: BiolinkClass): String = blClass.shorthand
          }
          map.asJson.noSpaces
        }
//        _ = println("encoded: " + encoded)
        _ = Files.writeString(Paths.get("src/test/resources/predicates.json"), encoded)
      } yield assert(map)(isNonEmpty)
      testCase.provideCustomLayer(testLayer)
    } @@ ignore
  )

  def spec = suite("All tests")(mapTest)

}

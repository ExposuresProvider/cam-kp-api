package org.renci.cam

import java.io.StringReader
import java.nio.file.{Files, Paths}

import com.typesafe.scalalogging.LazyLogging
import io.circe.{Encoder, KeyEncoder}
import io.circe.syntax._
import org.apache.commons.csv.CSVFormat
import org.renci.cam.Biolink.BiolinkData
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate}
import zio.config.ZConfig
import zio.{Has, RIO, Task}

import scala.jdk.CollectionConverters._

object PredicatesService extends LazyLogging {

  case class Triple(subj: BiolinkClass, pred: BiolinkPredicate, obj: BiolinkClass)

  def run(): RIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], String] =
    for {
      predicates <- Task.effect(Files.readString(Paths.get(getClass.getClassLoader.getResource("predicates.csv").toURI)))
      records = CSVFormat.DEFAULT.parse(new StringReader(predicates)).getRecords
      triples = records.asScala
        .map(a => PredicatesService.Triple(BiolinkClass(a.get(0)), BiolinkPredicate(a.get(1)), BiolinkClass(a.get(2))))
        .toList
      map = triples.groupBy(_.subj).view.mapValues(_.groupBy(_.obj).view.mapValues(_.map(_.pred)).toMap).toMap
      result = {
        implicit val blPredicateEncoder: Encoder[BiolinkPredicate] = Encoder.encodeString.contramap(predicate => predicate.shorthand)
        implicit val blClassKeyEncoder: KeyEncoder[BiolinkClass] = (blClass: BiolinkClass) => blClass.shorthand
        map.asJson.noSpaces
      }
    } yield result

}

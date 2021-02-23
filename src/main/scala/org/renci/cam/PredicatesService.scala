package org.renci.cam

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.csv.CSVFormat
import org.renci.cam.Biolink.BiolinkData
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate}
import zio.ZIO.ZIOAutoCloseableOps
import zio._
import zio.blocking._
import zio.config.ZConfig

import java.io.StringReader
import java.nio.charset.StandardCharsets
import scala.io.Source
import scala.jdk.CollectionConverters._

object PredicatesService extends LazyLogging {

  case class Triple(subj: BiolinkClass, pred: BiolinkPredicate, obj: BiolinkClass)

  def run: RIO[ZConfig[AppConfig] with Blocking with HttpClient with Has[BiolinkData],
               Map[BiolinkClass, Map[BiolinkClass, List[BiolinkPredicate]]]] =
    for {
      triples <- readPredicates
    } yield triples.groupBy(_.subj).view.mapValues(_.groupBy(_.obj).view.mapValues(_.map(_.pred)).toMap).toMap

  def readPredicates: ZIO[Blocking, Throwable, List[Triple]] = for {
    predicates <- effectBlockingIO(Source.fromInputStream(getClass.getResourceAsStream("/predicates.csv"), StandardCharsets.UTF_8.name()))
      .bracketAuto { source =>
        effectBlockingIO(source.getLines().mkString("\n"))
      }
    records <- Task.effect(CSVFormat.DEFAULT.parse(new StringReader(predicates)).getRecords)
    triples = records.asScala
      .map(a => Triple(BiolinkClass(a.get(0)), BiolinkPredicate(a.get(1)), BiolinkClass(a.get(2))))
      .toList
  } yield triples

}

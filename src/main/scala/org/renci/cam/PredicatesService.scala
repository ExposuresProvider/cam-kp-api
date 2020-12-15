package org.renci.cam

import java.io.{IOException, StringReader}

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.csv.CSVFormat
import org.renci.cam.Biolink.BiolinkData
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate}
import zio.ZIO.ZIOAutoCloseableOps
import zio._
import zio.blocking.{effectBlockingIO, Blocking}
import zio.config.ZConfig

import scala.io.Source
import scala.jdk.CollectionConverters._

object PredicatesService extends LazyLogging {

  case class Triple(subj: BiolinkClass, pred: BiolinkPredicate, obj: BiolinkClass)

  def readPredicates: ZIO[Blocking, IOException, String] =
    effectBlockingIO(Source.fromInputStream(getClass.getResourceAsStream("/predicates.csv"), "utf-8"))
      .bracketAuto { source =>
        effectBlockingIO(source.getLines().mkString("\n"))
      }

  def run: RIO[ZConfig[AppConfig] with Blocking with HttpClient with Has[BiolinkData],
               Map[BiolinkClass, Map[BiolinkClass, List[BiolinkPredicate]]]] =
    for {
      predicates <- readPredicates
      records = CSVFormat.DEFAULT.parse(new StringReader(predicates)).getRecords
      triples = records.asScala
        .map(a => PredicatesService.Triple(BiolinkClass(a.get(0)), BiolinkPredicate(a.get(1)), BiolinkClass(a.get(2))))
        .toList
      map = triples.groupBy(_.subj).view.mapValues(_.groupBy(_.obj).view.mapValues(_.map(_.pred)).toMap).toMap
    } yield map

}

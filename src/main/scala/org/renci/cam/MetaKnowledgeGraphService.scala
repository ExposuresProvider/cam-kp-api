package org.renci.cam

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.csv.CSVFormat
import org.renci.cam.Biolink.BiolinkData
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, MetaEdge, MetaKnowledgeGraph, MetaNode}
import zio.ZIO.ZIOAutoCloseableOps
import zio._
import zio.blocking._
import zio.config.ZConfig

import java.io.StringReader
import java.nio.charset.StandardCharsets
import scala.io.Source
import scala.jdk.CollectionConverters._

object MetaKnowledgeGraphService extends LazyLogging {

//  case class Triple(subj: BiolinkClass, pred: BiolinkPredicate, obj: BiolinkClass)

  def run: RIO[ZConfig[AppConfig] with Blocking with HttpClient with Has[BiolinkData], MetaKnowledgeGraph] = readPredicates

  def readPredicates: ZIO[Blocking with HttpClient with Has[BiolinkData], Throwable, MetaKnowledgeGraph] = for {
    biolinkData <- Biolink.biolinkData
    predicates <- effectBlockingIO(Source.fromInputStream(getClass.getResourceAsStream("/predicates.csv"), StandardCharsets.UTF_8.name()))
      .bracketAuto { source =>
        effectBlockingIO(source.getLines().mkString("\n"))
      }
    records <- Task.effect(CSVFormat.DEFAULT.parse(new StringReader(predicates)).getRecords)
    id_prefixes = biolinkData.prefixes.values.toList
    metaNode = MetaNode(id_prefixes)
    metaEdges = records.asScala.map(a => MetaEdge(BiolinkClass(a.get(0)), BiolinkPredicate(a.get(1)), BiolinkClass(a.get(2)), None)).toList
  } yield MetaKnowledgeGraph(metaNode, metaEdges)

//    triples = records.asScala
//      .map(a => Triple(BiolinkClass(a.get(0)), BiolinkPredicate(a.get(1)), BiolinkClass(a.get(2))))
//      .toList
//  } yield triples.groupBy(_.subj).view.mapValues(_.groupBy(_.obj).view.mapValues(_.map(_.pred)).toMap).toMap

}

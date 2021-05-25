package org.renci.cam

import com.typesafe.scalalogging.LazyLogging
import org.apache.commons.csv.CSVFormat
import org.renci.cam.Biolink.BiolinkData
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, MetaEdge, MetaKnowledgeGraph}
import zio.ZIO.ZIOAutoCloseableOps
import zio._
import zio.blocking._
import zio.config.ZConfig

import java.io.StringReader
import java.nio.charset.StandardCharsets
import scala.io.Source
import scala.jdk.CollectionConverters._

object MetaKnowledgeGraphService extends LazyLogging {

  def run: RIO[ZConfig[AppConfig] with Blocking with HttpClient with Has[BiolinkData], MetaKnowledgeGraph] = getMetaKnowledgeGraph

  def getMetaKnowledgeGraph: ZIO[Blocking with HttpClient with Has[BiolinkData], Throwable, MetaKnowledgeGraph] = for {
//    biolinkData <- Biolink.biolinkData
    metaNodes <- getNodes
    metaEdges <- getEdges
  } yield MetaKnowledgeGraph(metaNodes, metaEdges)

  def getEdges: ZIO[Blocking, Throwable, List[MetaEdge]] = for {
    predicates <- effectBlockingIO(Source.fromInputStream(getClass.getResourceAsStream("/predicates.csv"), StandardCharsets.UTF_8.name()))
      .bracketAuto { source =>
        effectBlockingIO(source.getLines().mkString("\n"))
      }
    predicateRecords <- Task.effect(CSVFormat.DEFAULT.parse(new StringReader(predicates)).getRecords)
    metaEdges = predicateRecords.asScala.map(a => MetaEdge(BiolinkClass(a.get(0)), BiolinkPredicate(a.get(1)), BiolinkClass(a.get(2)), None)).toList
  } yield metaEdges

  def getNodes: ZIO[Blocking, Throwable, Map[BiolinkClass, Map[String, List[String]]]] = for {
    mkgNodesData <- effectBlockingIO(Source.fromInputStream(getClass.getResourceAsStream("/mkg-nodes.csv"), StandardCharsets.UTF_8.name()))
      .bracketAuto { source =>
        effectBlockingIO(source.getLines().mkString("\n"))
      }
    //triples.groupBy(_.subj).view.mapValues(_.groupBy(_.obj).view.mapValues(_.map(_.pred)).toMap).toMap
    mkgNodeRecords <- Task.effect(CSVFormat.DEFAULT.parse(new StringReader(mkgNodesData)).getRecords)
    metaNodes = mkgNodeRecords.asScala.groupBy(a => BiolinkClass(a.get(0))).view.mapValues(_.groupBy(_.get(1)).view.mapValues(_.map(_.get(2)).toList).toMap).toMap
//    mkgMap = mkgNodeRecords.asScala.groupBy(_.get(0)).view.mapValues(_.map(_.get(1)).toList).toMap
//    metaNodes = mkgMap.map(a => MetaNode(BiolinkClass(a._1), a._2)).toList
  } yield metaNodes

}

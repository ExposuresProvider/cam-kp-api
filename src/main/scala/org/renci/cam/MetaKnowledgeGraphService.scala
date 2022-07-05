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

  def run: RIO[ZConfig[AppConfig] with Blocking with HttpClient with Has[BiolinkData], MetaKnowledgeGraph] = getMetaKnowledgeGraph

  def getMetaKnowledgeGraph: ZIO[Blocking with HttpClient with Has[BiolinkData], Throwable, MetaKnowledgeGraph] = for {
    metaNodes <- getNodes
    metaEdges <- getEdges
  } yield MetaKnowledgeGraph(metaNodes, metaEdges)

  def getEdges: ZIO[Blocking, Throwable, List[MetaEdge]] = for {
    predicates <- effectBlockingIO(Source.fromInputStream(getClass.getResourceAsStream("/predicates.csv"), StandardCharsets.UTF_8.name()))
      .bracketAuto { source =>
        effectBlockingIO(source.getLines().mkString("\n"))
      }
    predicateRecords <- Task.effect(CSVFormat.DEFAULT.parse(new StringReader(predicates)).getRecords)
    metaEdges = predicateRecords.asScala
      .map(a => MetaEdge(BiolinkClass(a.get(0)), BiolinkPredicate(a.get(1)), BiolinkClass(a.get(2)), None))
      .toList
  } yield metaEdges

  def getNodes: ZIO[Blocking, Throwable, Map[BiolinkClass, MetaNode]] = for {
    mkgNodesData <- effectBlockingIO(Source.fromInputStream(getClass.getResourceAsStream("/mkg-nodes.csv"), StandardCharsets.UTF_8.name()))
      .bracketAuto { source =>
        effectBlockingIO(source.getLines().mkString("\n"))
      }
    mkgNodeRecords <- Task.effect(CSVFormat.DEFAULT.parse(new StringReader(mkgNodesData)).getRecords)
    attributes = None // attributes: Option[List[MetaAttribute]]
    metaNodes = mkgNodeRecords.asScala
      .groupBy(a => BiolinkClass(a.get(0)))
      .view
      .mapValues(a => MetaNode(a.groupBy(b => b.get(2)).keys.toList, attributes))
      .toMap
  } yield metaNodes

}

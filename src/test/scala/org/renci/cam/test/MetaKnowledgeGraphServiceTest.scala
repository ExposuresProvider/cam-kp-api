package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import org.renci.cam.MetaKnowledgeGraphService
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, IRI, MetaEdge, MetaNode}
import zio.ZIO
import zio.ZIO.ZIOAutoCloseableOps
import zio.blocking._
import zio.stream.ZStream
import zio.test.Assertion.isNonEmpty
import zio.test._

import java.io.{File, FileWriter, PrintWriter}

object MetaKnowledgeGraphServiceTest extends DefaultRunnableSpec with LazyLogging {

  val namedThing = BiolinkClass("NamedThing", IRI("https://w3id.org/biolink/vocab/NamedThing"))

  def writeNodesToTSV(tsvFile: File, nodesMap: Map[BiolinkClass, MetaNode]) =
    // Is there an output directory to write to?
    if (!tsvFile.getParentFile.exists()) ZIO.unit
    else {
      effectBlockingIO(new PrintWriter(new FileWriter(tsvFile)))
        .bracketAuto { pw =>
          // We sort by shorthands
          val rows = ZStream
            .fromIterable(nodesMap.keySet.map(blc => (blc.shorthand, blc)).toSeq.sortBy(_._1).map(_._2))
            .map { biolinkClass =>
              val metaNode = nodesMap(biolinkClass)
              val attrs = metaNode.attributes.getOrElse(List())

              biolinkClass.shorthand + "\t" +
                biolinkClass.iri.value + "\t" +
                metaNode.id_prefixes.mkString("|") + "\t" +
                attrs.size + "\t" +
                attrs.mkString("|").replace('\t', ' ')
            }

          val output = ZStream("biolinkclass\tbiolinkclass_iri\tid_prefixes\tattrs_size\tattrs") ++ rows
          output.foreach { row =>
            pw.println(row)
            ZIO.unit
          }
        }
    }

  def writeEdgesToTSV(tsvFile: File, edgesList: List[MetaEdge]) =
    if (!tsvFile.getParentFile.exists()) ZIO.unit
    else {
      effectBlockingIO(new PrintWriter(new FileWriter(tsvFile)))
        .bracketAuto { pw =>
          // We sort by predicate
          val groupedByPreds = edgesList.groupBy(_.predicate)
          val sortedPreds = groupedByPreds.keySet.toSeq.sortBy(_.shorthand)

          val rows = ZStream
            .fromIterable(sortedPreds)
            .flatMap { predicate =>
              ZStream.fromIterable(groupedByPreds(predicate)).map { edge =>
                val attrs = edge.attributes.getOrElse(List())

                // I'm going to assume that shorthands and IRIs don't have tabs in them.
                edge.subject.shorthand + "\t" +
                  edge.subject.iri.value + "\t" +
                  edge.predicate.shorthand + "\t" +
                  edge.predicate.iri.value + "\t" +
                  edge.`object`.shorthand + "\t" +
                  edge.`object`.iri.value + "\t" +
                  attrs.size + "\t" +
                  attrs.mkString("|").replace('\t', ' ')
              }
            }

          val output =
            ZStream("subject\tsubject_iri\tpredicate\tpredicate_iri\tobject\tobject_iri\tattributes_count\tattributes_list") ++ rows

          output.foreach { row =>
            pw.println(row)
            ZIO.unit
          }
        }
    }

  val testGetEdges = suite("MetaKnowledgeGraphService.getEdges")(
    testM("test MetaKnowledgeGraphService.getEdges") {
      for {
        edges <- MetaKnowledgeGraphService.getEdges
        _ <- writeEdgesToTSV(new File("src/test/resources/meta-edges.tsv"), edges)
        ensureNamedThingRelatedToNamedThingMissing = {
          // In order to meet the requirements of
          // https://github.com/NCATSTranslator/ReasonerAPI/commit/0359f99054133d348076ce0ff6686cbc28825a4a
          // we need to confirm that implied ancestor relations are NOT reported in the edge results: only the most
          // specific predicates should be included.
          val expectedEdge = MetaEdge(
            namedThing,
            BiolinkPredicate("related_to", IRI("https://w3id.org/biolink/vocab/related_to")),
            namedThing,
            attributes = None
          )

          assert(edges)(Assertion.not(Assertion.contains(expectedEdge)))
        }
      } yield {
        assertTrue(edges.nonEmpty) && ensureNamedThingRelatedToNamedThingMissing
      }
    }
  )

  val testGetNodes = suite("MetaKnowledgeGraphService.getNodes")(
    testM("test MetaKnowledgeGraphService.getNodes") {
      for {
        nodes <- MetaKnowledgeGraphService.getNodes
        _ <- writeNodesToTSV(new File("src/test/resources/meta-nodes.tsv"), nodes)
        ensureNamedThingMissing: TestResult = {
          // In order to meet the requirements of
          // https://github.com/NCATSTranslator/ReasonerAPI/commit/0359f99054133d348076ce0ff6686cbc28825a4a
          // we need to confirm that implied ancestor relations are NOT reported in the node results: only the most
          // specific nodes should be included. Since we shouldn't include any NamedThings, we can test to
          // ensure that those are missing.
          assert(nodes.keySet)(Assertion.not(Assertion.contains(namedThing)))
        }
      } yield assert(nodes)(isNonEmpty) && ensureNamedThingMissing
    }
  )

  def spec = suite("MetaKnowledgeGraphService tests")(testGetEdges, testGetNodes)

}

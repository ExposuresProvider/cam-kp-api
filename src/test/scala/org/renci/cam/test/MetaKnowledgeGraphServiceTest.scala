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
      } yield {
        val ensureIncludedNamedThingRelatedToNamedThing: TestResult = {
          // In order to meet the requirements of
          // https://github.com/NCATSTranslator/ReasonerAPI/commit/e2ed87aa4f02dac55dcbd8eac7e190b8c188fbdd,
          // we need to confirm that implied ancestor relations are reported in the edge results as well.
          // Since we can be fairly sure that there are no explicit NamedThing-related_to-NamedThing relations,
          // if we see this in the edges, then we can be sure that we are reporting ancestor relations as well.
          val expectedEdge = MetaEdge(
            BiolinkClass("NamedThing", IRI("https://w3id.org/biolink/vocab/NamedThing")),
            BiolinkPredicate("related_to", IRI("https://w3id.org/biolink/vocab/related_to")),
            BiolinkClass("NamedThing", IRI("https://w3id.org/biolink/vocab/NamedThing")),
            attributes = None
          )

          val notExpectedEdge = MetaEdge(
            BiolinkClass("NamedThing", IRI("https://w3id.org/biolink/vocab/NamedThing")),
            // Note typo in BiolinkPredicate's shorthand
            BiolinkPredicate("relate_to", IRI("https://w3id.org/biolink/vocab/related_to")),
            BiolinkClass("NamedThing", IRI("https://w3id.org/biolink/vocab/NamedThing")),
            attributes = None
          )

          assert(edges)(Assertion.contains(expectedEdge)) && assert(edges)(Assertion.not(Assertion.contains(notExpectedEdge)))
        }

        assert(edges)(isNonEmpty) && ensureIncludedNamedThingRelatedToNamedThing
      }
    }
  )

  val testGetNodes = suite("MetaKnowledgeGraphService.getNodes")(
    testM("test MetaKnowledgeGraphService.getNodes") {
      for {
        nodes <- MetaKnowledgeGraphService.getNodes
        _ <- writeNodesToTSV(new File("src/test/resources/meta-nodes.tsv"), nodes)
      } yield assert(nodes)(isNonEmpty)
    }
  )

  def spec = suite("MetaKnowledgeGraphService tests")(testGetEdges, testGetNodes)

}

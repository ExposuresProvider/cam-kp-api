package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import org.renci.cam.MetaKnowledgeGraphService
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, IRI, MetaEdge, MetaNode}
import zio.test.Assertion.isNonEmpty
import zio.test._

import java.io.{File, FileWriter, PrintWriter}

object MetaKnowledgeGraphServiceTest extends DefaultRunnableSpec with LazyLogging {

  def writeNodesToTSV(tsvFile: File, nodesMap: Map[BiolinkClass, MetaNode]) = {
    // We sort by shorthands
    val nodesSorted = nodesMap.keySet.map(blc => (blc.shorthand, blc)).toSeq.sortBy(_._1).map(_._2)

    val pw = new PrintWriter(new FileWriter(tsvFile))

    pw.println("biolinkclass\tbiolinkclass_iri\tid_prefixes\tattrs_size\tattrs")

    nodesSorted.foreach { biolinkClass =>
      val metaNode = nodesMap(biolinkClass)
      val attrs = metaNode.attributes.getOrElse(List())

      pw.println(
        biolinkClass.shorthand + "\t" +
          biolinkClass.iri.value + "\t" +
          metaNode.id_prefixes.mkString("|") + "\t" +
          attrs.size + "\t" +
          attrs.mkString("|").replace('\t', ' ')
      )
    }

    pw.close()
  }

  def writeEdgesToTSV(tsvFile: File, edgesList: List[MetaEdge]) = {
    val groupedByPreds = edgesList.groupBy(_.predicate)
    val sortedPreds = groupedByPreds.keySet.toSeq.sortBy(_.shorthand)

    val pw = new PrintWriter(new FileWriter(tsvFile))

    pw.println("subject\tsubject_iri\tpredicate\tpredicate_iri\tobject\tobject_iri\tattributes_count\tattributes_list")

    sortedPreds.foreach { predicate =>
      val edges = groupedByPreds(predicate)

      edges.foreach { edge =>
        val attrs = edge.attributes.getOrElse(List())

        // I'm going to assume that shorthands and IRIs don't have tabs in them.
        pw.println(
          edge.subject.shorthand + "\t" +
            edge.subject.iri.value + "\t" +
            edge.predicate.shorthand + "\t" +
            edge.predicate.iri.value + "\t" +
            edge.`object`.shorthand + "\t" +
            edge.`object`.iri.value + "\t" +
            attrs.size + "\t" +
            attrs.mkString("|").replace('\t', ' ')
        )
      }
    }

    pw.close()
  }

  val testGetEdges = suite("MetaKnowledgeGraphService.getEdges")(
    testM("test MetaKnowledgeGraphService.getEdges") {
      for {
        edges <- MetaKnowledgeGraphService.getEdges
        _ = writeEdgesToTSV(new File("src/test/resources/meta-edges.tsv"), edges)
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
        _ = writeNodesToTSV(new File("src/test/resources/meta-nodes.tsv"), nodes)
      } yield assert(nodes)(isNonEmpty)
    }
  )

  def spec = suite("MetaKnowledgeGraphService tests")(testGetEdges, testGetNodes)

}

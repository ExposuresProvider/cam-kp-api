package org.renci.cam

import java.nio.charset.StandardCharsets

import io.circe.generic.auto._
import io.circe.syntax._
import org.apache.commons.io.IOUtils
import org.apache.jena.query.ResultSetFactory
import org.renci.cam.domain._
import zio.test.Assertion.{equalTo, _}
import zio.test._
import org.renci.cam.QueryService.sparqlJsonDecoder

import scala.collection.mutable

object SerializationTest extends DefaultRunnableSpec {

  def spec =
    suite("SerializationSpec")(
      test("1") {
        val expected =
          """{"message":{"query_graph":{"nodes":[{"id":"n0","type":"gene","curie":"NCBIGENE:558"},{"id":"n1","type":"biological_process"}],"edges":[{"id":"e0","source_id":"n1","target_id":"n0","type":"has_participant"}]}}}"""

        val n0Node = KGSNode("n0", "gene", Some("NCBIGENE:558"))
        val n1Node = KGSNode("n1", "biological_process", None)
        val e0Edge = KGSEdge("e0", "n1", "n0", "has_participant")

        val queryGraph = KGSQueryGraph(List(n0Node, n1Node), List(e0Edge))
        val message = KGSMessage(queryGraph)
        val requestBody = KGSQueryRequestBody(message)
        val encoded = requestBody.asJson.deepDropNullValues.noSpaces
        println("encoded: " + encoded)
        println("expected: " + expected)
        assert(expected)(equalTo(encoded))
      },
      test("2") {

        val response = s"""{
    "head" : {
      "vars" : [ "predicate" ]
    },
    "results" : {
      "bindings" : [ {
        "predicate" : {
          "type" : "uri",
          "value" : "http://purl.obolibrary.org/obo/RO_0002233"
        }
      }, {
        "predicate" : {
          "type" : "uri",
          "value" : "http://purl.obolibrary.org/obo/RO_0002333"
        }
      }, {
        "predicate" : {
          "type" : "uri",
          "value" : "http://purl.obolibrary.org/obo/RO_0000057"
        }
      }, {
        "predicate" : {
          "type" : "uri",
          "value" : "http://purl.obolibrary.org/obo/RO_0002234"
        }
      }, {
        "predicate" : {
          "type" : "uri",
          "value" : "https://w3id.org/biolink/vocab/enabled_by"
        }
      }, {
        "predicate" : {
          "type" : "uri",
          "value" : "https://w3id.org/biolink/vocab/has_input"
        }
      }, {
        "predicate" : {
          "type" : "uri",
          "value" : "https://w3id.org/biolink/vocab/has_output"
        }
      } ]
    }
  }"""
        val is = IOUtils.toInputStream(response, StandardCharsets.UTF_8)
        val resultSet = ResultSetFactory.fromJSON(is)
        is.close()

        // val bindings = (for {
        //   solution <- resultSet.asScala
        //   v <- solution.varNames.asScala
        //   node = solution.get(v)
        // } yield s"<$node>").mkString(" ")

        var bindings = new mutable.ListBuffer[String]()
        while (resultSet.hasNext()) {
          val binding = resultSet.nextBinding
          println("binding.size(): " + binding.size())
          val varIter = binding.vars
          while (varIter.hasNext()) {
            val nodeVar = varIter.next()
            println("nodeVar.getVarName(): " + nodeVar.getVarName())
            val node = binding.get(nodeVar)
            println("node.toString(): " + node.toString())
            bindings += node.toString()
          }
        }

        assert(bindings.toList)(isNonEmpty)

      }
    )

}

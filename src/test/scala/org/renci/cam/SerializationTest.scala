package org.renci.cam

import java.nio.charset.StandardCharsets

import io.circe.generic.auto._
import io.circe.syntax._
import org.apache.commons.io.IOUtils
import org.apache.jena.query.ResultSetFactory
import org.renci.cam.domain._
import zio.test.Assertion.{equalTo, _}
import zio.test.TestAspect._
import zio.test._

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
      } @@ ignore,
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
//        val resultSet = QueryService.jsonToResultSet(response)
        var is = IOUtils.toInputStream(response, StandardCharsets.UTF_8)
        var resultSet = ResultSetFactory.fromJSON(is)
        is.close()

//        val bindings1 = (for {
//          solution <- resultSet.asScala
//          v <- solution.varNames.asScala
//          node = solution.get(v)
//        } yield s"<$node> ").mkString("")
//        println("bindings1: " + bindings1.mkString(" "))

        var bindings1 = new mutable.ListBuffer[String]()
        while (resultSet.hasNext()) {
          val solution = resultSet.nextSolution
          solution.varNames.forEachRemaining(a => bindings1 += s"<${solution.get(a)}>")
        }
        println("bindings1: " + bindings1.mkString(" "))

        is = IOUtils.toInputStream(response, StandardCharsets.UTF_8)
        resultSet = ResultSetFactory.fromJSON(is)
        is.close()

        var bindings2 = new mutable.ListBuffer[String]()
        while (resultSet.hasNext()) {
          val binding = resultSet.nextBinding
          val varIter = binding.vars
          while (varIter.hasNext()) {
            val nodeVar = varIter.next()
            val node = binding.get(nodeVar)
            bindings2 += s"<${node}>"
          }
        }
        println("bindings2: " + bindings2.mkString(" "))

        assert(bindings2.toList)(isNonEmpty)
      } @@ ignore
    )

}

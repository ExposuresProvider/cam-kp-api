package org.renci.cam

import java.io.ByteArrayOutputStream
import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import io.circe.generic.auto._
import io.circe.syntax._
import org.apache.commons.io.IOUtils
import org.apache.jena.query.{ResultSetFactory, ResultSetFormatter}
import org.renci.cam.QueryService.NewTRAPIEdge
import org.renci.cam.domain._
import zio.Task
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import scala.collection.JavaConverters._
import scala.collection.mutable

object SerializationTest extends DefaultRunnableSpec {

  def spec =
    suite("SerializationSpec")(
      testM("test consistency of message digest") {
        for {
          messageDigest <- Task.effect(MessageDigest.getInstance("SHA-256"))
          firstTRAPIEdge <- Task.effect(NewTRAPIEdge(Some("asdfasdf"), "qwerqwer", "zxcvzxcv").asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8))
          first <- Task.effect(String.format("%064x", new BigInteger(1, messageDigest.digest(firstTRAPIEdge))))
          secondTRAPIEdge <- Task.effect(NewTRAPIEdge(Some("asdfasdf"), "qwerqwer", "zxcvzxcv").asJson.deepDropNullValues.noSpaces.getBytes(StandardCharsets.UTF_8))
          second <- Task.effect(String.format("%064x", new BigInteger(1, messageDigest.digest(secondTRAPIEdge))))
        } yield assert(first)(equalTo(second))
      }/*@@ ignore*/,
      test("1") {
        val expected =
          """{"message":{"query_graph":{"nodes":[{"id":"n0","type":"gene","curie":"NCBIGENE:558"},{"id":"n1","type":"biological_process"}],"edges":[{"id":"e0","source_id":"n0","target_id":"n1","type":"has_participant"}]},"results":[]}}"""

        val n0Node = TRAPIQueryNode("n0", Some("gene"), Some("NCBIGENE:558"))
        val n1Node = TRAPIQueryNode("n1", Some("biological_process"), None)
        val e0Edge = TRAPIQueryEdge("e0", "n0", "n1", Some("has_participant"))

        val queryGraph = TRAPIQueryGraph(List(n0Node, n1Node), List(e0Edge))
        val message = TRAPIMessage(Some(queryGraph), None, None)
        val requestBody = TRAPIQueryRequestBody(message)
        val encoded = requestBody.asJson.deepDropNullValues.noSpaces
        println("encoded: " + encoded)
        println("expected: " + expected)
        assert(expected)(equalsIgnoreCase(encoded))
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
      } @@ ignore,
      test("test empty results") {

        val response = s"""{ "head": {
    "vars": [ "n1" , "n0" , "n0_type" , "n1_type" , "e0" ]
  } ,
  "results": {
    "bindings": [ ]
  }
}"""
        var is = IOUtils.toInputStream(response, StandardCharsets.UTF_8)
        var resultSet = ResultSetFactory.fromJSON(is)
        is.close()

        var baos = new ByteArrayOutputStream()
        ResultSetFormatter.outputAsJSON(baos, resultSet)
        baos.close();
        val json = new String(baos.toByteArray)

        println("json: " + json)
        assert(json)(isNonEmptyString)
      } @@ ignore
    )

}

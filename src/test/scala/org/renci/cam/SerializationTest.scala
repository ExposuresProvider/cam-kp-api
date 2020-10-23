package org.renci.cam

import java.io.ByteArrayOutputStream
import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import org.apache.commons.io.IOUtils
import org.apache.commons.lang3.StringUtils
import org.apache.jena.query.{ResultSetFactory, ResultSetFormatter}
import org.renci.cam.QueryService.TRAPIEdgeKey
import org.renci.cam.domain._
import zio.Task
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import scala.collection.mutable

object SerializationTest extends DefaultRunnableSpec {

  val testLayerZ = HttpClient.makeHttpClientLayer.map { httpLayer =>
    httpLayer >+> Utilities.makeUtilitiesLayer
  }

  val testingMessageDigest = suite("testingMessageDigest")(
    testM("test consistency of message digest") {
      for {
        messageDigest <- Task.effect(MessageDigest.getInstance("SHA-256"))
        firstTRAPIEdge <- Task.effect(
          TRAPIEdgeKey(Some(BiolinkPredicate("asdfasdf")), "qwerqwer", "zxcvzxcv").asJson.deepDropNullValues.noSpaces
            .getBytes(StandardCharsets.UTF_8))
        first <- Task.effect(String.format("%064x", new BigInteger(1, messageDigest.digest(firstTRAPIEdge))))
        secondTRAPIEdge <- Task.effect(
          TRAPIEdgeKey(Some(BiolinkPredicate("asdfasdf")), "qwerqwer", "zxcvzxcv").asJson.deepDropNullValues.noSpaces
            .getBytes(StandardCharsets.UTF_8))
        second <- Task.effect(String.format("%064x", new BigInteger(1, messageDigest.digest(secondTRAPIEdge))))
      } yield assert(first)(equalTo(second))
    } /*@@ ignore*/
  )

  val testTRAPIQueryRequestBodyEncodingOut = suite("testTRAPIQueryRequestBodyEncodingOut")(
    testM("encoding upon departure") {
      val expected =
        """{"message":{"query_graph":{"nodes":[{"id":"n0","type":"gene","curie":"NCBIGene:558"},{"id":"n1","type":"biological_process"}],"edges":[{"id":"e0","source_id":"n0","target_id":"n1","type":"has_participant"}]}}}"""

      val n0Node = TRAPIQueryNode("n0", Some(BiolinkClass("gene")), Some(IRI("http://www.ncbi.nlm.nih.gov/gene/558")))
      val n1Node = TRAPIQueryNode("n1", Some(BiolinkClass("biological_process")), None)
      val e0Edge = TRAPIQueryEdge("e0", "n0", "n1", Some(BiolinkPredicate("has_participant")))

      val queryGraph = TRAPIQueryGraph(List(n0Node, n1Node), List(e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQueryRequestBody(message)
      val testCase = for {
        prefixes <- Utilities.biolinkPrefixes
      } yield {
        implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoderOut(prefixes.prefixes)
        implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap(blTerm => blTerm.shorthand)
        implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Encoder.encodeString.contramap(blTerm => blTerm.shorthand)
        val encoded = requestBody.asJson.deepDropNullValues.noSpaces
//                println("encoded: " + encoded)
//                println("expected: " + expected)
        assert(expected)(equalsIgnoreCase(encoded))
      }
      testLayerZ.flatMap(layer => testCase.provideCustomLayer(layer))
    } /*@@ ignore*/
  )

  val testTRAPIQueryRequestBodyEncodingIn = suite("testTRAPIQueryRequestBodyEncodingIn")(
    testM("encoding upon arrival") {
      val expected =
        """{"message":{"query_graph":{"nodes":[{"id":"n0","type":"https://w3id.org/biolink/vocab/Gene","curie":"http://www.ncbi.nlm.nih.gov/gene/558"},{"id":"n1","type":"https://w3id.org/biolink/vocab/BiologicalProcess"}],"edges":[{"id":"e0","source_id":"n0","target_id":"n1","type":"https://w3id.org/biolink/vocab/has_participant"}]}}}"""

      val n0Node = TRAPIQueryNode("n0", Some(BiolinkClass("gene")), Some(IRI("http://www.ncbi.nlm.nih.gov/gene/558")))
      val n1Node = TRAPIQueryNode("n1", Some(BiolinkClass("biological_process")), None)
      val e0Edge = TRAPIQueryEdge("e0", "n0", "n1", Some(BiolinkPredicate("has_participant")))

      val queryGraph = TRAPIQueryGraph(List(n0Node, n1Node), List(e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQueryRequestBody(message)
      val testCase = for {
        prefixes <- Utilities.biolinkPrefixes
      } yield {
        implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(prefixes.prefixes)
        implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoderIn(prefixes.prefixes)
        implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap(blTerm => blTerm.iri.value)
        implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Encoder.encodeString.contramap(blTerm => blTerm.iri.value)
        val encoded = requestBody.asJson.deepDropNullValues.noSpaces
        //        println("encoded: " + encoded)
        //        println("expected: " + expected)
        assert(expected)(equalsIgnoreCase(encoded))
      }
//        println("encoded: " + encoded)
//        println("expected: " + expected)
      testLayerZ.flatMap(layer => testCase.provideCustomLayer(layer))
    } /*@@ ignore*/
  )

  val testTRAPIQueryRequestBodyDecoding2 = suite("testTRAPIQueryRequestBodyDecoding2")(
    testM("decoding") {

      val n0Node = TRAPIQueryNode("n0", Some(BiolinkClass("gene")), Some(IRI("http://www.ncbi.nlm.nih.gov/gene/558")))
      val n1Node = TRAPIQueryNode("n1", Some(BiolinkClass("biological_process")), None)
      val e0Edge = TRAPIQueryEdge("e0", "n0", "n1", Some(BiolinkPredicate("has_participant")))

      val queryGraph = TRAPIQueryGraph(List(n0Node, n1Node), List(e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQueryRequestBody(message)
      val testCase = for {
        prefixes <- Utilities.biolinkPrefixes
      } yield {
        implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(prefixes.prefixes)
//        implicit val iriEncoder: Encoder[IRI] = IRI.makeEncoderIn(prefixes.prefixesMap)
        val encoded = requestBody.asJson.deepDropNullValues.noSpaces
        //        println("encoded: " + encoded)
        //        println("expected: " + expected)
        println("encoded: " + encoded)
        val decodedJson = encoded.asJson
        val decoded = decode[TRAPIQueryRequestBody](encoded)
        println("n0 == " + decoded.toOption.get.message.query_graph.get.nodes.head.id)
        assertCompletes
      }
      testLayerZ.flatMap(layer => testCase.provideCustomLayer(layer))
    } @@ ignore
  )

  val testParseBlazegraphResponse = suite("testParseBlazegraphResponse")(
    test("1") {
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

  val testParseBlazegraphEmptyResults = suite("testParseBlazegraphEmptyResults")(
    test("test empty results") {
      val response = s"""{
          |"head": { "vars": [ "n1" , "n0" , "n0_type" , "n1_type" , "e0" ] },
          |"results": { "bindings": [ ] }
          |}""".stripMargin
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

  val testIRIWithColonInReference = suite("testIRIWithColonInReference")(
    testM("test iri with colon in reference") {
      val testCase = for {
        prefixes <- Utilities.biolinkPrefixes
        iri = IRI("http://identifiers.org/mgi/MGI:1914846")
        startsWith = prefixes.prefixes.filter { case (_, namespace) => iri.value.startsWith(namespace) }
        asdf =
          if (startsWith.nonEmpty) {
            val (prefix, namespace) = startsWith.maxBy(_._2.length)
            StringUtils.prependIfMissing(iri.value.drop(namespace.length), prefix)
//          s"$prefix:${iri.value.drop(namespace.length)}"
          } else {
            iri.value
          }
        _ = println("asdf: " + asdf)
      } yield assertCompletes
      testLayerZ.flatMap(layer => testCase.provideCustomLayer(layer))
    } /*@@ ignore*/
  )

  def spec = suite("All tests")(
    testIRIWithColonInReference,
    testingMessageDigest,
    testTRAPIQueryRequestBodyEncodingIn,
    testTRAPIQueryRequestBodyEncodingOut,
    testTRAPIQueryRequestBodyDecoding2,
    testParseBlazegraphResponse,
    testParseBlazegraphEmptyResults
  )

}

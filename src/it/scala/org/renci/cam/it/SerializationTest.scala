package org.renci.cam.it

import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.apache.commons.lang3.StringUtils
import org.renci.cam._
import org.renci.cam.domain._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.testEnvironment

object SerializationTest extends DefaultRunnableSpec with LazyLogging {

  val testLayer = (testEnvironment ++ HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer).mapError(TestFailure.die)

  val testTRAPIQueryRequestBodyEncodingOut = suite("testTRAPIQueryRequestBodyEncodingOut")(
    testM("encoding upon departure") {
      val expected =
        """{"message":{"query_graph":{"nodes":{"n0":{"ids":["NCBIGENE:558"],"categories":["biolink:Gene"]},"n1":{"categories":["biolink:BiologicalProcess"]}},"edges":{"e0":{"predicates":["biolink:has_participant"],"subject":"n0","object":"n1"}}}}}"""

      val n0Node = TRAPIQueryNode(Some(List(IRI("http://identifiers.org/ncbigene/558"))), Some(List(BiolinkClass("Gene"))), None)
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("BiologicalProcess"))), None)
      val e0Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("has_participant"))), "n0", "n1", None)

      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        biolinkData <- Biolink.biolinkData
      } yield {

        implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
        implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
        implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder
        implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Implicits.biolinkPredicateEncoder(biolinkData.prefixes)

        implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
        implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
//        implicit val blClassKeyEncoder: KeyEncoder[BiolinkClass] = (blClass: BiolinkClass) => s"biolink:${blClass.shorthand}"
//        implicit val blClassKeyDecoder: Decoder[TRAPIQueryNode] = new Decoder[TRAPIQueryNode] {
//          final def apply(c: HCursor): Decoder.Result[TRAPIQueryNode] =
//            for {
//              key <- c.value.as[String]
//            } yield TRAPIQueryNode(None, None, None)
//        }

        val encoded = requestBody.asJson.deepDropNullValues.noSpaces
        // println("encoded: " + encoded)
        assert(expected)(equalsIgnoreCase(encoded))
      }
    }
  )

  val testTRAPIQueryRequestBodyEncodingIn = suite("testTRAPIQueryRequestBodyEncodingIn")(
    testM("encoding upon arrival") {
      val expected =
        """{"message":{"query_graph":{"nodes":{"n0":{"ids":["http://identifiers.org/ncbigene/558"],"categories":["https://w3id.org/biolink/vocab/Gene"]},"n1":{"categories":["https://w3id.org/biolink/vocab/BiologicalProcess"]}},"edges":{"e0":{"predicates":["https://w3id.org/biolink/vocab/has_participant"],"subject":"n0","object":"n1"}}}}}"""

      val n0Node = TRAPIQueryNode(Some(List(IRI("http://identifiers.org/ncbigene/558"))), Some(List(BiolinkClass("Gene"))), None)
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("BiologicalProcess"))), None)
      val e0Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("has_participant"))), "n0", "n1", None)

      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        biolinkData <- Biolink.biolinkData
      } yield {
        implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
        implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
        implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap(blTerm => blTerm.iri.value)
        implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Encoder.encodeString.contramap(blTerm => blTerm.iri.value)
        implicit val iriEncoder: Encoder[IRI] = Encoder.encodeString.contramap(iri => iri.value)
        val encoded = requestBody.asJson.deepDropNullValues.noSpaces
        assert(expected)(equalsIgnoreCase(encoded))
      }
    }
  )

  val testTRAPIQueryRequestBodyDecoding2 = suite("testTRAPIQueryRequestBodyDecoding2")(
    testM("decoding") {
      val n0Node = TRAPIQueryNode(Some(List(IRI("http://identifiers.org/ncbigene/558"))), Some(List(BiolinkClass("Gene"))), None)
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("BiologicalProcess"))), None)
      val e0Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("has_participant"))), "n0", "n1", None)

      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        biolinkData <- Biolink.biolinkData
      } yield {
        implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
        implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
        implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
//        implicit val iriEncoder: Encoder[IRI] = IRI.makeEncoderIn(biolinkData.prefixesMap)
        val encoded = requestBody.asJson.deepDropNullValues.noSpaces
        //        println("encoded: " + encoded)
        //        println("expected: " + expected)
//        println("encoded: " + encoded)
        val decodedJson = encoded.asJson
        val decoded = decode[TRAPIQuery](encoded)
//        println("node map: " + decoded.toOption.get.message.query_graph.get.nodes)
        assertCompletes
      }
    }
  )

  val testIRIWithColonInReference = suite("testIRIWithColonInReference")(
    testM("test iri with colon in reference") {
      for {
        biolinkData <- Biolink.biolinkData
        iri = IRI("http://identifiers.org/mgi/MGI:1914846")
        startsWith = biolinkData.prefixes.filter { case (_, namespace) => iri.value.startsWith(namespace) }
        asdf =
          if (startsWith.nonEmpty) {
            val (prefix, namespace) = startsWith.maxBy(_._2.length)
            StringUtils.prependIfMissing(iri.value.drop(namespace.length), prefix)
//          s"$prefix:${iri.value.drop(namespace.length)}"
          } else {
            iri.value
          }
//        _ = println("asdf: " + asdf)
      } yield assertCompletes
    }
  )

  val messingWithMaps = suite("messingWithMaps")(
    testM("messingWithMaps") {
      val originalMap: Map[BiolinkClass, Map[BiolinkClass, List[BiolinkPredicate]]] = Map(
        BiolinkClass("IndividualOrganism") -> Map(BiolinkClass("Occurrent") ->
          List(BiolinkPredicate("participates_in"), BiolinkPredicate("related_to"))))
      for {
        biolinkData <- Biolink.biolinkData
      } yield {

//        logger.info("predicates: {}", biolinkData.predicates)

        implicit val biolinkClassKeyEncoder: KeyEncoder[BiolinkClass] = Implicits.biolinkClassKeyEncoder
        implicit val biolinkClassKeyDecoder: KeyDecoder[BiolinkClass] = Implicits.biolinkClassKeyDecoder(biolinkData.classes)
        implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Implicits.biolinkPredicateEncoder(biolinkData.prefixes)
        implicit val biolinkPredicateDecoder: Decoder[BiolinkPredicate] = Implicits.biolinkPredicateDecoder(biolinkData.predicates)

        val json = originalMap.asJson.deepDropNullValues.noSpaces
        val parsed = parser.parse(json).toOption.get
        val map = parsed.as[Map[BiolinkClass, Map[BiolinkClass, List[BiolinkPredicate]]]]

        assert(map)(isRight) && assert(map.toOption.get.keys)(contains(BiolinkClass("IndividualOrganism")))
      }
    }
  )

  def spec = suite("Serialization tests")(
    testIRIWithColonInReference,
    testTRAPIQueryRequestBodyEncodingIn,
    testTRAPIQueryRequestBodyEncodingOut,
    messingWithMaps
  ).provideLayerShared(testLayer)

}

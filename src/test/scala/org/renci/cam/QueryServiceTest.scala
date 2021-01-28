package org.renci.cam

import java.nio.file.{Files, Paths}

import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import org.http4s._
import org.http4s.headers._
import org.http4s.implicits._
import org.renci.cam.domain._
import zio.Task
import zio.interop.catz._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

object QueryServiceTest extends DefaultRunnableSpec {

  val testLayer = HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer

  val listNodeTypes = suite("listNodeTypes")(
    test("testGetNodeTypes") {
      val n0Node = TRAPIQueryNode(Some(IRI("http://www.ncbi.nlm.nih.gov/gene/558")), Some(BiolinkClass("Gene")), None)
      val n1Node = TRAPIQueryNode(None, Some(BiolinkClass("BiologicalProcess")), None)
      val e0Edge = TRAPIQueryEdge("n1", "n0", Some(BiolinkPredicate("has_participant")), None)

      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val map = QueryService.getNodeTypes(queryGraph.nodes)
      map.foreach(a => printf("k: %s, v: %s%n", a._1, a._2))
      assert(map)(isNonEmpty)
    } @@ ignore
  )

  val testSimpleQuery = suite("testSimpleQuery")(
    testM("test simple query") {
      val testCase = for {
        httpClient <- HttpClient.client
        biolinkData <- Biolink.biolinkData
        encoded = {
          implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
          implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoderIn(biolinkData.prefixes)
          implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
          implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
          implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)

          val n0Node = TRAPIQueryNode(None /*Some(IRI("http://www.ncbi.nlm.nih.gov/gene/558"))*/, Some(BiolinkClass("Gene")), None)
          val n1Node = TRAPIQueryNode(None, Some(BiolinkClass("BiologicalProcess")), None)
          val e0Edge = TRAPIQueryEdge("n1", "n0", Some(BiolinkPredicate("has_participant")), None)
          val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
          val message = TRAPIMessage(Some(queryGraph), None, None)
          val requestBody = TRAPIQuery(message)
          requestBody.asJson.deepDropNullValues.noSpaces
        }
        _ = println("encoded: " + encoded)
        uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1) // scala
        //uri = uri"http://127.0.0.1:6434/query".withQueryParam("limit", 1) // python
        request = Request[Task](Method.POST, uri)
          .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
          .withEntity(encoded)
        response <- httpClient.expect[String](request)
        _ = println("response: " + response)
        _ = Files.writeString(Paths.get("src/test/resources/local-scala-new.json"), response)
      } yield assert(response)(isNonEmptyString)
      testCase.provideCustomLayer(testLayer)
    } //@@ ignore
  )

  val testFindGenesEnablingAnyKindOfCatalyticActivity = suite("testFindGenesEnablingAnyKindOfCatalyticActivity")(
    testM("find genes enabling any kind of catalytic activity") {
      val n0Node = TRAPIQueryNode(None, Some(BiolinkClass("GeneOrGeneProduct")), None)
      val n1Node = TRAPIQueryNode(Some(IRI("http://purl.obolibrary.org/obo/GO_0003824")), Some(BiolinkClass("MolecularActivity")), None)
      val e0Edge = TRAPIQueryEdge("n1", "n0", Some(BiolinkPredicate("enabled_by")), None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message)
      val testCase = for {
        httpClient <- HttpClient.client
        biolinkData <- Biolink.biolinkData
        _ = println("biolinkData.prefixes: " + biolinkData.prefixes)
        encoded = {
          implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
          implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoderIn(biolinkData.prefixes)
          implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
          implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
          implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          requestBody.asJson.deepDropNullValues.noSpaces
        }
        _ = println("encoded: " + encoded)
        uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1) // scala
        //uri = uri"http://127.0.0.1:6434/query".withQueryParam("limit", 1) // python
        request = Request[Task](Method.POST, uri)
          .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
          .withEntity(encoded)
        response <- httpClient.expect[String](request)
        _ = println("response: " + response)
        _ = Files.writeString(Paths.get("src/test/resources/local-scala-find-genes-enabling-catalytic-activity.json"), response)
      } yield assert(response)(isNonEmptyString)
      testCase.provideCustomLayer(testLayer)
    } @@ ignore
  )

  val testGene2Process2Process2Gene = suite("testGene2Process2Process2Gene")(
    testM("test gene to process to process to gene") {
      val n0Node = TRAPIQueryNode(Some(IRI("http://identifiers.org/uniprot/P30530")), Some(BiolinkClass("Gene")), None)
      val n1Node = TRAPIQueryNode(None, Some(BiolinkClass("BiologicalProcess")), None)
      val n2Node = TRAPIQueryNode(None, Some(BiolinkClass("BiologicalProcess")), None)
      val n3Node = TRAPIQueryNode(None, Some(BiolinkClass("Gene")), None)
      val e0Edge = TRAPIQueryEdge("n1", "n0", None, None)
      val e1Edge = TRAPIQueryEdge("n1", "n2", None /*Some(CURIEorIRI(None, "enabled_by"))*/, None)
      val e2Edge = TRAPIQueryEdge("n2", "n3", None, None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node, "n2" -> n2Node, "n3" -> n3Node),
                                       Map("e0" -> e0Edge, "e1" -> e1Edge, "e2" -> e2Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message)
      val testCase = for {
        httpClient <- HttpClient.client
        biolinkData <- Biolink.biolinkData
        encoded = {
          //          implicit val iriEncoder: Encoder[IRI] = IRI.makeEncoderIn(prefixes.prefixesMap)
          implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
          implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoderIn(biolinkData.prefixes)
          implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
          implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
          requestBody.asJson.deepDropNullValues.noSpaces
        }
        _ = println("encoded: " + encoded)
        uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1) // scala
        //uri = uri"http://127.0.0.1:6434/query".withQueryParam("limit", 1) // python
        request = Request[Task](Method.POST, uri)
          .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
          .withEntity(encoded)
        response <- httpClient.expect[String](request)
        _ = println("response: " + response)
        _ = Files.writeString(Paths.get("src/test/resources/local-scala-gene-to-process-to-process-to-gene.json"), response)
      } yield assert(response)(isNonEmptyString)
      testCase.provideCustomLayer(testLayer)
    } @@ ignore
  )

  val testNegativeRegulationChaining = suite("testNegativeRegulationChaining")(
    testM("negative regulation chaining") {
      val n0Node =
        TRAPIQueryNode(Some(IRI("http://purl.obolibrary.org/obo/GO_0004252")), Some(BiolinkClass("biological_process_or_activity")), None)
      val n1Node =
        TRAPIQueryNode(Some(IRI("http://purl.obolibrary.org/obo/GO_0003810")), Some(BiolinkClass("biological_process_or_activity")), None)
      val n2Node = TRAPIQueryNode(None, Some(BiolinkClass("GeneOrGeneProduct")), None)
      val e0Edge = TRAPIQueryEdge("n0", "n1", Some(BiolinkPredicate("PositivelyRegulates")), None)
      val e1Edge = TRAPIQueryEdge("n1", "n2", Some(BiolinkPredicate("enabled_by")), None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node, "n2" -> n2Node), Map("e0" -> e0Edge, "e1" -> e1Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message)
      val testCase = for {
        httpClient <- HttpClient.client
        biolinkData <- Biolink.biolinkData
        encoded = {
          //          implicit val iriEncoder: Encoder[IRI] = IRI.makeEncoderIn(prefixes.prefixesMap)
          implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
          implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
          implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
          implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoderIn(biolinkData.prefixes)
          requestBody.asJson.deepDropNullValues.noSpaces
        }
        uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1) // scala
        //uri = uri"http://127.0.0.1:6434/query".withQueryParam("limit", 1) // python
        request = Request[Task](Method.POST, uri)
          .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
          .withEntity(encoded)
        response <- httpClient.expect[String](request)
        _ = println("response: " + response)
        _ = Files.writeString(Paths.get("src/test/resources/local-scala-negative-regulation-chaining.json"), response)
      } yield assert(response)(isNonEmptyString)
      testCase.provideCustomLayer(testLayer)
    } @@ ignore
  )

  def spec = suite("All tests")(listNodeTypes,
                                testSimpleQuery,
                                testGene2Process2Process2Gene,
                                testFindGenesEnablingAnyKindOfCatalyticActivity,
                                testNegativeRegulationChaining)

}

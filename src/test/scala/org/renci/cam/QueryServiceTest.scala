package org.renci.cam

import java.nio.file.{Files, Paths}

import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
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

  val testLayerZ = HttpClient.makeHttpClientLayer.map { httpLayer =>
    httpLayer >+> Biolink.makeUtilitiesLayer
  }

  val listNodeTypes = suite("listNodeTypes")(
    test("testGetNodeTypes") {
      val n0Node = TRAPIQueryNode("n0", Some(BiolinkClass("gene")), Some(IRI("http://www.ncbi.nlm.nih.gov/gene/558")))
      val n1Node = TRAPIQueryNode("n1", Some(BiolinkClass("biological_process")), None)
      val e0Edge = TRAPIQueryEdge("e0", "n1", "n0", Some(BiolinkPredicate("has_participant")))

      val queryGraph = TRAPIQueryGraph(List(n0Node, n1Node), List(e0Edge))
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
          implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap(blTerm => blTerm.shorthand)
          implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Encoder.encodeString.contramap(blTerm => blTerm.shorthand)

          val n0Node = TRAPIQueryNode("n0", Some(BiolinkClass("gene")), None/*Some(IRI("http://www.ncbi.nlm.nih.gov/gene/558"))*/)
          val n1Node = TRAPIQueryNode("n1", Some(BiolinkClass("biological_process")), None)
          val e0Edge = TRAPIQueryEdge("e0", "n1", "n0", Some(BiolinkPredicate("has_participant")))
          val queryGraph = TRAPIQueryGraph(List(n0Node, n1Node), List(e0Edge))
          val message = TRAPIMessage(Some(queryGraph), None, None)
          val requestBody = TRAPIQueryRequestBody(message)
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
        _ = Files.writeString(Paths.get("src/test/resources/local-scala.json"), response)
      } yield assert(response)(isNonEmptyString)
      testLayerZ.flatMap(layer => testCase.provideCustomLayer(layer))
    } @@ ignore
  )

  val testGene2Process2Process2Gene = suite("testGene2Process2Process2Gene")(
    testM("test gene to process to process to gene") {
      val n0Node = TRAPIQueryNode("n0", Some(BiolinkClass("gene")), Some(IRI("http://identifiers.org/uniprot/P30530")))
      val n1Node = TRAPIQueryNode("n1", Some(BiolinkClass("biological_process")), None)
      val n2Node = TRAPIQueryNode("n2", Some(BiolinkClass("biological_process")), None)
      val n3Node = TRAPIQueryNode("n3", Some(BiolinkClass("gene")), None)
      val e0Edge = TRAPIQueryEdge("e0", "n1", "n0", None)
      val e1Edge = TRAPIQueryEdge("e1", "n1", "n2", None /*Some(CURIEorIRI(None, "enabled_by"))*/ )
      val e2Edge = TRAPIQueryEdge("e2", "n2", "n3", None)
      val queryGraph = TRAPIQueryGraph(List(n0Node, n1Node, n2Node, n3Node), List(e0Edge, e1Edge, e2Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQueryRequestBody(message)
      val testCase = for {
        httpClient <- HttpClient.client
        biolinkData <- Biolink.biolinkData
        encoded = {
          //          implicit val iriEncoder: Encoder[IRI] = IRI.makeEncoderIn(prefixes.prefixesMap)
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
        _ = Files.writeString(Paths.get("src/test/resources/local-scala-gene-to-process-to-process-to-gene.json"), response)
      } yield assert(response)(isNonEmptyString)
      testLayerZ.flatMap(layer => testCase.provideCustomLayer(layer))
    } @@ ignore
  )

  val testFindGenesEnablingAnyKindOfCatalyticActivity = suite("testFindGenesEnablingAnyKindOfCatalyticActivity")(
    testM("find genes enabling any kind of catalytic activity") {
      val n0Node = TRAPIQueryNode("n0", Some(BiolinkClass("gene_or_gene_product")), None)
      val n1Node = TRAPIQueryNode("n1", Some(BiolinkClass("molecular_activity")), Some(IRI("http://purl.obolibrary.org/obo/GO_0003824")))
      val e0Edge = TRAPIQueryEdge("e0", "n1", "n0", Some(BiolinkPredicate("enabled_by")))
      val queryGraph = TRAPIQueryGraph(List(n0Node, n1Node), List(e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQueryRequestBody(message)
      val testCase = for {
        httpClient <- HttpClient.client
        biolinkData <- Biolink.biolinkData
        encoded = {
          //          implicit val iriEncoder: Encoder[IRI] = IRI.makeEncoderIn(prefixes.prefixesMap)
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
        _ = Files.writeString(Paths.get("src/test/resources/local-scala-find-genes-enabling-catalytic-activity.json"), response)
      } yield assert(response)(isNonEmptyString)
      testLayerZ.flatMap(layer => testCase.provideCustomLayer(layer))
    } @@ ignore
  )

  val testNegativeRegulationChaining = suite("testNegativeRegulationChaining")(
    testM("negative regulation chaining") {
      val n0Node =
        TRAPIQueryNode("n0", Some(BiolinkClass("biological_process_or_activity")), Some(IRI("http://purl.obolibrary.org/obo/GO_0004252")))
      val n1Node =
        TRAPIQueryNode("n1", Some(BiolinkClass("biological_process_or_activity")), Some(IRI("http://purl.obolibrary.org/obo/GO_0003810")))
      val n2Node = TRAPIQueryNode("n2", Some(BiolinkClass("gene_or_gene_product")), None)
      val e0Edge = TRAPIQueryEdge("e0", "n0", "n1", Some(BiolinkPredicate("positively_regulates")))
      val e1Edge = TRAPIQueryEdge("e1", "n1", "n2", Some(BiolinkPredicate("enabled_by")))
      val queryGraph = TRAPIQueryGraph(List(n0Node, n1Node, n2Node), List(e0Edge, e1Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQueryRequestBody(message)
      val testCase = for {
        httpClient <- HttpClient.client
        biolinkData <- Biolink.biolinkData
        encoded = {
          //          implicit val iriEncoder: Encoder[IRI] = IRI.makeEncoderIn(prefixes.prefixesMap)
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
      testLayerZ.flatMap(layer => testCase.provideCustomLayer(layer))
    } @@ ignore
  )

  def spec = suite("All tests")(listNodeTypes,
                                testSimpleQuery,
                                testGene2Process2Process2Gene,
                                testFindGenesEnablingAnyKindOfCatalyticActivity,
                                testNegativeRegulationChaining)

}

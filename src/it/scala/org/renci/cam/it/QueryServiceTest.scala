package org.renci.cam.it

import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import org.http4s._
import org.http4s.headers._
import org.http4s.implicits._
import org.renci.cam._
import org.renci.cam.domain._
import zio.Task
import zio.blocking.Blocking
import zio.interop.catz._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.testEnvironment

import java.nio.file.{Files, Paths}

object QueryServiceTest extends DefaultRunnableSpec {

  val camkpapiTestLayer = Blocking.live >>> TestContainer.camkpapi
  val camkpapiLayer = HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer
  val testLayer = (testEnvironment ++ camkpapiTestLayer ++ camkpapiLayer).mapError(TestFailure.die)

  val testSimpleQuery = suite("testSimpleQuery")(
    testM("test simple query") {
      for {
        httpClient <- HttpClient.client
        biolinkData <- Biolink.biolinkData
        encoded = {
          implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
          implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
          implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
          implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
          implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] =
            Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)

          val n0Node = TRAPIQueryNode(None, Some(BiolinkClass("Gene")), None)
          val n1Node = TRAPIQueryNode(None, Some(BiolinkClass("BiologicalProcess")), None)
          val e0Edge = TRAPIQueryEdge(Some(BiolinkPredicate("has_participant")), None, "n1", "n0")
          val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
          val message = TRAPIMessage(Some(queryGraph), None, None)
          val requestBody = TRAPIQuery(message)
          requestBody.asJson.deepDropNullValues.noSpaces
        }
        _ = println("encoded: " + encoded)
        uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1) // scala
        request = Request[Task](Method.POST, uri)
          .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
          .withEntity(encoded)
        response <- httpClient.expect[String](request)
//        _ = println("response: " + response)
        _ = Files.writeString(Paths.get("src/test/resources/local-scala.json"), response)
      } yield assert(response)(isNonEmptyString)
    } //@@ TestAspect.ignore
  )

  val testFindGenesEnablingAnyKindOfCatalyticActivity = suite("testFindGenesEnablingAnyKindOfCatalyticActivity")(
    testM("find genes enabling any kind of catalytic activity") {
      val n0Node = TRAPIQueryNode(None, Some(BiolinkClass("GeneOrGeneProduct")), None)
      val n1Node = TRAPIQueryNode(Some(IRI("http://purl.obolibrary.org/obo/GO_0003824")), Some(BiolinkClass("MolecularActivity")), None)
      val e0Edge = TRAPIQueryEdge(Some(BiolinkPredicate("enabled_by")), None, "n1", "n0")
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message)
      for {
        httpClient <- HttpClient.client
        biolinkData <- Biolink.biolinkData
        encoded = {
          implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
          implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
          implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
          implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
          implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] =
            Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          requestBody.asJson.deepDropNullValues.noSpaces
        }
        _ = println("encoded: " + encoded)
        uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1) // scala
        request = Request[Task](Method.POST, uri)
          .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
          .withEntity(encoded)
        response <- httpClient.expect[String](request)
//        _ = println("response: " + response)
//        _ = Files.writeString(Paths.get("src/test/resources/local-scala-find-genes-enabling-catalytic-activity.json"), response)
      } yield assert(response)(isNonEmptyString)
    } //@@ TestAspect.ignore
  )

  val testGene2Process2Process2Gene = suite("testGene2Process2Process2Gene")(
    testM("test gene to process to process to gene") {
      val n0Node = TRAPIQueryNode(Some(IRI("http://identifiers.org/uniprot/P30530")), Some(BiolinkClass("Gene")), None)
      val n1Node = TRAPIQueryNode(None, Some(BiolinkClass("BiologicalProcess")), None)
      val n2Node = TRAPIQueryNode(None, Some(BiolinkClass("BiologicalProcess")), None)
      val n3Node = TRAPIQueryNode(None, Some(BiolinkClass("Gene")), None)
      val e0Edge = TRAPIQueryEdge(None, None, "n1", "n0")
      val e1Edge = TRAPIQueryEdge(Some(BiolinkPredicate("enabled_by")), None, "n1", "n2")
      val e2Edge = TRAPIQueryEdge(None, None, "n2", "n3")
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node, "n2" -> n2Node, "n3" -> n3Node),
                                       Map("e0" -> e0Edge, "e1" -> e1Edge, "e2" -> e2Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message)
      for {
        httpClient <- HttpClient.client
        biolinkData <- Biolink.biolinkData
        encoded = {
          implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
          implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
          implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
          implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
          implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] =
            Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          requestBody.asJson.deepDropNullValues.noSpaces
        }
        _ = println("encoded: " + encoded)
        uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1) // scala
        request = Request[Task](Method.POST, uri)
          .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
          .withEntity(encoded)
        response <- httpClient.expect[String](request)
//        _ = println("response: " + response)
//        _ = Files.writeString(Paths.get("src/test/resources/local-scala-gene-to-process-to-process-to-gene.json"), response)
      } yield assert(response)(isNonEmptyString)
    } //@@ TestAspect.ignore
  )

  val testNegativeRegulationChaining = suite("testNegativeRegulationChaining")(
    testM("negative regulation chaining") {
      val n0Node =
        TRAPIQueryNode(Some(IRI("http://purl.obolibrary.org/obo/GO_0004252")), Some(BiolinkClass("BiologicalProcessOrActivity")), None)
      val n1Node =
        TRAPIQueryNode(Some(IRI("http://purl.obolibrary.org/obo/GO_0003810")), Some(BiolinkClass("BiologicalProcessOrActivity")), None)
      val n2Node = TRAPIQueryNode(None, Some(BiolinkClass("GeneOrGeneProduct")), None)
      val e0Edge = TRAPIQueryEdge(Some(BiolinkPredicate("positively_regulates")), None, "n0", "n1")
      val e1Edge = TRAPIQueryEdge(Some(BiolinkPredicate("enabled_by")), None, "n1", "n2")
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node, "n2" -> n2Node), Map("e0" -> e0Edge, "e1" -> e1Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message)
      for {
        httpClient <- HttpClient.client
        biolinkData <- Biolink.biolinkData
        encoded = {
          implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
          implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
          implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
          implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
          implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] =
            Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          requestBody.asJson.deepDropNullValues.noSpaces
        }
        _ = println("encoded: " + encoded)
        uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1) // scala
        request = Request[Task](Method.POST, uri)
          .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
          .withEntity(encoded)
        response <- httpClient.expect[String](request)
//        _ = println("response: " + response)
//        _ = Files.writeString(Paths.get("src/test/resources/local-scala-negative-regulation-chaining.json"), response)
      } yield assert(response)(isNonEmptyString)
    } //@@ TestAspect.ignore
  )

  val testAcrocyanosis = suite("testAcrocyanosis")(
    testM("acrocyanosis") {
      val n0Node =
        TRAPIQueryNode(Some(IRI("UMLS:C0221347")), Some(BiolinkClass("PhenotypicFeature")), None)
      val n1Node =
        TRAPIQueryNode(None, Some(BiolinkClass("NamedThing")), None)
      val e0Edge = TRAPIQueryEdge(None, None, "n0", "n1")
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message)
      for {
        httpClient <- HttpClient.client
        biolinkData <- Biolink.biolinkData
        encoded = {
          implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
          implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
          implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
          implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
          implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] =
            Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          requestBody.asJson.deepDropNullValues.noSpaces
        }
        _ = println("encoded: " + encoded)
        uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1) // scala
        request = Request[Task](Method.POST, uri)
          .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
          .withEntity(encoded)
        response <- httpClient.expect[String](request)
        //        _ = println("response: " + response)
        //        _ = Files.writeString(Paths.get("src/test/resources/local-scala-negative-regulation-chaining.json"), response)
      } yield assert(response)(isNonEmptyString)
    } //@@ TestAspect.ignore
  )

  val testBeclomethasone = suite("testBeclomethasone")(
    testM("beclomethasone") {
      val n0Node =
        TRAPIQueryNode(Some(IRI("DRUGBANK:DB00394")), Some(BiolinkClass("Drug")), None)
      val n1Node =
        TRAPIQueryNode(None, Some(BiolinkClass("Disease")), None)
      val e0Edge = TRAPIQueryEdge(Some(BiolinkPredicate("treated_by")), None, "n0", "n1")
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message)
      for {
        httpClient <- HttpClient.client
        biolinkData <- Biolink.biolinkData
        encoded = {
          implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
          implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
          implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
          implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
          implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] =
            Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          requestBody.asJson.deepDropNullValues.noSpaces
        }
        _ = println("encoded: " + encoded)
        uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1) // scala
        request = Request[Task](Method.POST, uri)
          .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
          .withEntity(encoded)
        response <- httpClient.expect[String](request)
        //        _ = println("response: " + response)
        //        _ = Files.writeString(Paths.get("src/test/resources/local-scala-negative-regulation-chaining.json"), response)
      } yield assert(response)(isNonEmptyString)
    } //@@ TestAspect.ignore
  )

  val testCorrelatedWith = suite("testCorrelatedWith")(
    testM("correlatedWith") {
      val n0Node =
        TRAPIQueryNode(Some(IRI("MONDO:0004979")), Some(BiolinkClass("Disease")), None)
      val n1Node =
        TRAPIQueryNode(None, Some(BiolinkClass("ChemicalSubstance")), None)
      val e0Edge = TRAPIQueryEdge(Some(BiolinkPredicate("correlated_with")), None, "n0", "n1")
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message)
      for {
        httpClient <- HttpClient.client
        biolinkData <- Biolink.biolinkData
        encoded = {
          implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
          implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
          implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
          implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
          implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] =
            Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          requestBody.asJson.deepDropNullValues.noSpaces
        }
        _ = println("encoded: " + encoded)
        uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1) // scala
        request = Request[Task](Method.POST, uri)
          .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
          .withEntity(encoded)
        response <- httpClient.expect[String](request)
        //        _ = println("response: " + response)
        //        _ = Files.writeString(Paths.get("src/test/resources/local-scala-negative-regulation-chaining.json"), response)
      } yield assert(response)(isNonEmptyString)
    } //@@ TestAspect.ignore
  )

  val testPathway = suite("testPathway")(
    testM("pathway") {
      val n0Node =
        TRAPIQueryNode(Some(IRI("NCBIGENE:1017")), Some(BiolinkClass("Gene")), None)
      val n1Node =
        TRAPIQueryNode(None, Some(BiolinkClass("Pathway")), None)
      val e0Edge = TRAPIQueryEdge(None, None, "n0", "n1")
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message)
      for {
        httpClient <- HttpClient.client
        biolinkData <- Biolink.biolinkData
        encoded = {
          implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
          implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
          implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
          implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
          implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] =
            Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          requestBody.asJson.deepDropNullValues.noSpaces
        }
        _ = println("encoded: " + encoded)
        uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1) // scala
        request = Request[Task](Method.POST, uri)
          .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
          .withEntity(encoded)
        response <- httpClient.expect[String](request)
        //        _ = println("response: " + response)
        //        _ = Files.writeString(Paths.get("src/test/resources/local-scala-negative-regulation-chaining.json"), response)
      } yield assert(response)(isNonEmptyString)
    } //@@ TestAspect.ignore
  )

  val testSpmsyChemicals = suite("testSpmsyChemicals")(
    testM("spmsyChemicals") {
      val n0Node =
        TRAPIQueryNode(Some(IRI("UniProtKB:P52788")), Some(BiolinkClass("Gene")), None)
      val n1Node =
        TRAPIQueryNode(None, Some(BiolinkClass("ChemicalSubstance")), None)
      val e0Edge = TRAPIQueryEdge(None, None, "n0", "n1")
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message)
      for {
        httpClient <- HttpClient.client
        biolinkData <- Biolink.biolinkData
        encoded = {
          implicit val iriDecoder: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
          implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
          implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
          implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
          implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] =
            Encoder.encodeString.contramap(blTerm => blTerm.withBiolinkPrefix)
          requestBody.asJson.deepDropNullValues.noSpaces
        }
        _ = println("encoded: " + encoded)
        uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1) // scala
        request = Request[Task](Method.POST, uri)
          .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
          .withEntity(encoded)
        response <- httpClient.expect[String](request)
        //        _ = println("response: " + response)
        //        _ = Files.writeString(Paths.get("src/test/resources/local-scala-negative-regulation-chaining.json"), response)
      } yield assert(response)(isNonEmptyString)
    } //@@ TestAspect.ignore
  )

  def spec = suite("QueryService tests")(
    testSimpleQuery,
    testGene2Process2Process2Gene,
    testFindGenesEnablingAnyKindOfCatalyticActivity,
    testNegativeRegulationChaining,
    testAcrocyanosis,
    testBeclomethasone,
    testCorrelatedWith,
    testPathway,
    testSpmsyChemicals
  ).provideLayerShared(testLayer) @@ TestAspect.sequential

}

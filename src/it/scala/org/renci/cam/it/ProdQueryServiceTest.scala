package org.renci.cam.it

import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.http4s._
import org.http4s.headers._
import org.renci.cam.Biolink.BiolinkData
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam._
import org.renci.cam.domain._
import zio.blocking.Blocking
import zio.config.typesafe.TypesafeConfig
import zio.config.{ZConfig, getConfig}
import zio.interop.catz._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.testEnvironment
import zio.{Has, Layer, RIO, Task, ZIO}

object ProdQueryServiceTest extends DefaultRunnableSpec {

  def runTest(trapiQuery: TRAPIQuery, limit: Int = 1, include_extra_edges: Boolean = false): RIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], (Map[IRI, TRAPINode], Map[String, TRAPIEdge])] =
    for {
      appConfig <- getConfig[AppConfig]
      httpClient <- HttpClient.client
      biolinkData <- Biolink.biolinkData
      encoded = {
        implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
        implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
        implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder
        implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Implicits.biolinkPredicateEncoder(biolinkData.prefixes)
        trapiQuery.asJson.deepDropNullValues.noSpaces
      }
      _ = println("encoded: " + encoded)
      uri = Uri.fromString(s"https://cam-kp-api.renci.org/${appConfig.trapiVersion}/query").toOption.get
      _ = println("uri: " + uri)
      uriWithQueryParams = uri.withQueryParam("limit", limit).withQueryParam("include_extra_edges", include_extra_edges)
      request = Request[Task](Method.POST, uriWithQueryParams)
        .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
        .withEntity(encoded)
      response <- httpClient.expect[String](request)
      _ = println("response: " + response)
      trapiResponseJson <- ZIO.fromEither(io.circe.parser.parse(response))
      knowledgeGraphJson <-
        ZIO
          .fromOption(trapiResponseJson.hcursor.downField("message").downField("knowledge_graph").focus)
          .orElseFail(new Exception("failed to traverse down to knowledge_graph"))

      knowledgeGraphNodesJson <-
        ZIO
          .fromOption(knowledgeGraphJson.hcursor.downField("nodes").focus)
          .orElseFail(new Exception("failed to traverse down to context"))

      nodesMapResult = {
        implicit val decoderIRI: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
        implicit val keyDecoderIRI: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
        implicit val decoderTRAPINode: Decoder[BiolinkClass] = Implicits.biolinkClassDecoder(biolinkData.classes)
        implicit val decoderTRAPIAttribute: Decoder[TRAPIAttribute] = deriveDecoder[TRAPIAttribute]
        knowledgeGraphNodesJson.as[Map[IRI, TRAPINode]]
      }

      nodesMap <- ZIO.fromOption(nodesMapResult.toOption).orElseFail(new Exception("failed to parse nodesMap"))

      knowledgeGraphEdgesJson <-
        ZIO
          .fromOption(knowledgeGraphJson.hcursor.downField("edges").focus)
          .orElseFail(new Exception("failed to traverse down to context"))

      edgesMapResult = {
        implicit val decoderIRI: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
        implicit val keyDecoderIRI: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
        implicit val decoderBiolinkClass: Decoder[BiolinkClass] = Implicits.biolinkClassDecoder(biolinkData.classes)
        implicit val decoderBiolinkPredicate: Decoder[BiolinkPredicate] = Implicits.biolinkPredicateDecoder(biolinkData.predicates)
        implicit val decoderTRAPIAttribute: Decoder[TRAPIAttribute] = deriveDecoder[TRAPIAttribute]
        knowledgeGraphEdgesJson.as[Map[String, TRAPIEdge]]
      }

      edgesMap <- ZIO.fromOption(edgesMapResult.toOption).orElseFail(new Exception("failed to parse edgesMap"))

    } yield (nodesMap, edgesMap)

  val testSimpleQuery = suite("testSimpleQuery")(
    testM("test simple query") {
      val n0Node = TRAPIQueryNode(None, Some(List(BiolinkClass("GeneOrGeneProduct"))), None)
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("BiologicalProcess"))), None)
      val e0Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("has_participant"))), "n1", "n0", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        (kgNodeMap, kgEdgeMap) <- runTest(requestBody)
      } yield assert(kgNodeMap)(isNonEmpty) && assert(kgEdgeMap)(isNonEmpty)
    }
  )

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val camkpapiLayer = Blocking.live >>> HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer
  val testLayer = (configLayer ++ testEnvironment ++ camkpapiLayer).mapError(TestFailure.die)

  def spec = suite("QueryService tests")(
    testSimpleQuery
  ).provideLayerShared(testLayer) @@ TestAspect.sequential

}

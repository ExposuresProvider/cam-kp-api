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
import zio.config.{getConfig, ZConfig}
import zio.interop.catz._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.testEnvironment
import zio.{Has, Layer, RIO, Task, ZIO}

object ProdQueryServiceTest extends DefaultRunnableSpec {

  def runQuery(
    trapiQuery: TRAPIQuery,
    limit: Int = 1): RIO[ZConfig[AppConfig] with HttpClient with Has[BiolinkData], (Map[IRI, TRAPINode], Map[String, TRAPIEdge])] =
    for {
      appConfig <- getConfig[AppConfig]
      httpClient <- HttpClient.client
      biolinkData <- Biolink.biolinkData
      encoded = {
        import biolinkData.implicits._
        trapiQuery.asJson.deepDropNullValues.noSpaces
      }
      _ = println("encoded: " + encoded)
      // TODO: this should probably be in the AppConfig somewhere.
      uri = Uri.fromString(s"https://cam-kp-api.renci.org/${appConfig.trapiVersion}/query").toOption.get
      _ = println("uri: " + uri)
      uriWithQueryParams = uri.withQueryParam("limit", limit)
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
        import biolinkData.implicits._
        knowledgeGraphNodesJson.as[Map[IRI, TRAPINode]]
      }

      nodesMap <- ZIO.fromOption(nodesMapResult.toOption).orElseFail(new Exception("failed to parse nodesMap"))

      knowledgeGraphEdgesJson <-
        ZIO
          .fromOption(knowledgeGraphJson.hcursor.downField("edges").focus)
          .orElseFail(new Exception("failed to traverse down to context"))

      edgesMapResult = {
        import biolinkData.implicits._
        implicit lazy val decoderTRAPIAttribute: Decoder[TRAPIAttribute] = deriveDecoder[TRAPIAttribute]
        knowledgeGraphEdgesJson.as[Map[String, TRAPIEdge]]
      }

      edgesMap <- ZIO.fromOption(edgesMapResult.toOption).orElseFail(new Exception("failed to parse edgesMap"))

    } yield (nodesMap, edgesMap)

  val testSimpleQuery = suite("testSimpleQuery")(
    testM("test BP has_participant ZFIN gene") {
      val n0Node = TRAPIQueryNode(Some(List(IRI("http://identifiers.org/zfin/ZDB-LINCRNAG-050208-254"))),
                                  Some(List(BiolinkClass("GeneOrGeneProduct"))),
                                  None)
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("BiologicalProcess"))), None)
      val e0Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("has_participant"))), "n1", "n0", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val query = TRAPIQuery(message, None)
      for {
        (kgNodeMap, kgEdgeMap) <- runQuery(query)
      } yield assert(kgNodeMap)(isNonEmpty) && assert(kgEdgeMap)(isNonEmpty)
    },
    testM("test UniProtKB gene enables MF") {
      val n0Node =
        TRAPIQueryNode(Some(List(IRI("http://identifiers.org/uniprot/P51532"))), Some(List(BiolinkClass("GeneOrGeneProduct"))), None)
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("MolecularActivity"))), None)
      val e0Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("enables"))), "n0", "n1", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val query = TRAPIQuery(message, None)
      for {
        (kgNodeMap, kgEdgeMap) <- runQuery(query)
      } yield assert(kgNodeMap)(isNonEmpty) && assert(kgEdgeMap)(isNonEmpty) && assert(kgNodeMap.keys)(
        contains(IRI("http://purl.obolibrary.org/obo/GO_0003713")))
    },
    testM("test GO:0017075 regulates GO:0000149") {
      val n0Node = TRAPIQueryNode(Some(List(IRI("http://purl.obolibrary.org/obo/GO_0017075"))),
                                  Some(List(BiolinkClass("BiologicalProcessOrActivity"))),
                                  None)
      val n1Node = TRAPIQueryNode(Some(List(IRI("http://purl.obolibrary.org/obo/GO_0000149"))),
                                  Some(List(BiolinkClass("BiologicalProcessOrActivity"))),
                                  None)
      val e0Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("regulates"))), "n0", "n1", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val query = TRAPIQuery(message, None)
      for {
        (kgNodeMap, kgEdgeMap) <- runQuery(query)
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

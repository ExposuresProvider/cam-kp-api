package org.renci.cam.it

import org.apache.jena.atlas.json.JSON
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser._
import io.circe.Json._
import org.http4s._
import org.http4s.headers._
import org.http4s.implicits._
import org.renci.cam.Biolink.BiolinkData
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam._
import org.renci.cam.domain._
import zio.blocking.Blocking
import zio.interop.catz._
import zio.test.Assertion
import zio.test._
import zio.test.environment.testEnvironment
import zio.{Has, RIO, Task}
import zio.ZIO
import collection.JavaConverters._

import java.nio.file.{Files, Paths}

/**
 * This test ensures that we return the correct original_knowledge_source on API queries.
 * We currently return one of two values:
 *  - We default to `infores:go-cam`, since that is where most our data comes from.
 *  - Data from `ctdbase.org` is reported with an original_knowledge_source of `infores:ctd`.
 *
 * This code was adapted from QueryServiceTest.
 */
object OriginalKnowledgeSourceTest extends DefaultRunnableSpec {

  def runTest(trapiQuery: TRAPIQuery, limit: Int = 1, include_extra_edges: Boolean = false): RIO[HttpClient with Has[BiolinkData], String] =
    for {
      httpClient <- HttpClient.client
      biolinkData <- Biolink.biolinkData
      encoded = {
        implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
        implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
        implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder
        implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Implicits.biolinkPredicateEncoder(biolinkData.prefixes)
        trapiQuery.asJson.deepDropNullValues.noSpaces
      }
      // _ = println("encoded: " + encoded)
      uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", limit).withQueryParam("include_extra_edges", include_extra_edges)
      request = Request[Task](Method.POST, uri)
        .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
        .withEntity(encoded)
      response <- httpClient.expect[String](request)
      //        _ = println("response: " + response)
    } yield response

  /**
   * This query asks: "what is positively regulated by GO:0004709 [MAP3K activity]?"
   * The answer should include: MAP2K activity and MAPK activity.
   * The source should be: http://noctua.geneontology.org/editor/graph/gomodel:568b0f9600000284
   */
  val testPositivelyRegulatedByMAP3K = suite("testPositivelyRegulatedByMAP3K")(
    testM("testPositivelyRegulatedByMAP3K") {
      val n0Node = TRAPIQueryNode(Some(List(IRI("GO:0004709"))), None, None)
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("NamedThing"))), None)
      val e0Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("positively_regulates"))), "n0", "n1", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        // TODO: setting limit to >28 leads to a 400 Bad Request error.
        response <- runTest(requestBody, limit=28)
        _ = Files.writeString(Paths.get("src/it/resources/test-positively-regulated-by-MAP3K.json"), response)
        json = JSON.parse(response)
        // Jim: I spent way too long trying to figure out how to do this with Circe and failed.
        // If you know a better way of doing this in that system, please let me know!
        message = json.get("message").getAsObject
        results = message.get("results").getAsArray
      } yield {
        // Make sure we have a non-empty response
        assert(response)(Assertion.isNonEmptyString) &&
        // We expect Success.
        assert(message.get("status").getAsString.value)(Assertion.equalsIgnoreCase("Success")) &&
        // We got 11 results as of 2022-02-14; we don't expect to get fewer than that.
        assert(results.size)(Assertion.isGreaterThanEqualTo(11))
      }
    }
  )

  val camkpapiTestLayer = Blocking.live >>> TestContainer.camkpapi
  val camkpapiLayer = HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer
  val testLayer = (testEnvironment ++ camkpapiTestLayer ++ camkpapiLayer).mapError(TestFailure.die)

  def spec = suite("original_knowledge_source tests")(
    testPositivelyRegulatedByMAP3K,
  ).provideLayerShared(testLayer) @@ TestAspect.sequential

}

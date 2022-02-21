package org.renci.cam.it

import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.http4s._
import org.http4s.headers._
import org.http4s.implicits._
import org.renci.cam.Biolink.BiolinkData
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam._
import org.renci.cam.domain._
import zio.blocking.Blocking
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.interop.catz._
import zio.test._
import zio.test.environment.testEnvironment
import zio.{Has, Layer, RIO, Task, ZIO}

import java.nio.file.{Files, Paths}
import scala.collection.immutable

/**
 * This test ensures that we return the correct original_knowledge_source on API queries.
 * We currently return one of two values:
 *  - We default to `infores:go-cam`, since that is where most our data comes from.
 *  - Data from `ctdbase.org` is reported with an original_knowledge_source of `infores:ctd`.
 *
 * This code was adapted from QueryServiceTest and ProdQueryServiceTest.
 */
object OriginalKnowledgeSourceTest extends DefaultRunnableSpec {

  def runQuery(trapiQuery: TRAPIQuery, limit: Int = 1, include_extra_edges: Boolean = false):
    RIO[HttpClient with Has[BiolinkData], TRAPIResponse] =
    for {
      httpClient <- HttpClient.client
      biolinkData <- Biolink.biolinkData
      encoded = {
        // Encode IRIs and Biolink classes
        implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
        implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
        implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder
        implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Implicits.biolinkPredicateEncoder(biolinkData.prefixes)

        trapiQuery.asJson.deepDropNullValues.noSpaces
      }
      // _ = println("encoded: " + encoded)
      // We query the local Docker instance hosting CAM-KP-API.
      uri = uri"http://127.0.0.1:8080/query"
        .withQueryParam("limit", limit)
        .withQueryParam("include_extra_edges", include_extra_edges)
      request = Request[Task](Method.POST, uri)
        .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
        .withEntity(encoded)
      response <- httpClient.expect[String](request)
      // _ = println("response: " + response)
      responseJson <- ZIO.fromEither(parse(response))
      trapiMessage <- ZIO.fromEither({
        // We can decode the TRAPI response into a TRAPIResponse object.
        implicit val decoderIRI: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
        implicit val keyDecoderIRI: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
        implicit val decoderBiolinkPredicate: Decoder[BiolinkPredicate] = Implicits.biolinkPredicateDecoder(biolinkData.predicates)
        implicit val decoderBiolinkClass: Decoder[BiolinkClass] = Implicits.biolinkClassDecoder(biolinkData.classes)

        responseJson.as[TRAPIResponse]
      })
    } yield trapiMessage

  /**
   * This query asks: "what is positively regulated by GO:0004709 [MAP3K activity]?"
   * The answer should include: MAP2K activity (GO:0004708) and MAPK activity (GO:0004707).
   * All the answers should be: biolink:BiologicalProcessOrActivity
   * The source should be: http://model.geneontology.org/568b0f9600000284
   * The original knowledge source should be: infores:go-cam
   */
  val testPositivelyRegulatedByMAP3K = {
    val n0Node = TRAPIQueryNode(Some(List(IRI("GO:0004709"))), None, None)
    val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("NamedThing"))), None)
    val e0Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("positively_regulates"))), "n0", "n1", None)
    val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
    val message = TRAPIMessage(Some(queryGraph), None, None)
    val requestBody = TRAPIQuery(message, None)

    // TODO: setting limit to >28 leads to a 400 Bad Request error.
    val responses = runQuery(requestBody, limit = 28)

    suite("testPositivelyRegulatedByMAP3K")(
      testM("Response should be sensible") {
        for {
          response <- responses
          // For debugging purposes, we write out the JSON response to a file in src/it/resources.
          biolinkData <- Biolink.biolinkData
          _ = Files.writeString(
            Paths.get("src/it/resources/test-positively-regulated-by-MAP3K.json"),
            {
              implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
              implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
              implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder
              implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Implicits.biolinkPredicateEncoder(biolinkData.prefixes)

              response.asJson.deepDropNullValues.spaces2SortKeys
            }
          )
        } yield {
          // Make sure the response was successful.
          assert(response.status.get)(Assertion.equalsIgnoreCase("Success"))
        }
      },
      testM("Provenance information should be correct") {
        for {
          response <- responses
          kg <- ZIO.fromOption(response.message.knowledge_graph)
          // We would usually need to filter this to only the edges we're interested in,
          // but this query only queries a single edge, so we can assume all edges refer
          // to the same kind of thing.
          attrsById = kg.edges.transform((_, value) => value.attributes.getOrElse(List()))
          // We explicitly call `.get` here so that we get an exception if no attribute_source was present.
          attrSources = attrsById.transform((_, value) => value.map(_.attribute_source.get))
          attrsOKG = attrsById.transform((_, value) => value.filter(_.attribute_type_id == BiolinkPredicate("original_knowledge_source").iri))
          attrsOKGValues = attrsOKG.map(_._2)
        } yield {
          // All the attribute_source values should be infores:cam-kp
          assert(attrSources.values.flatten)(Assertion.forall(Assertion.equalTo("infores:cam-kp"))) &&
          // At least one of the source URLs should be http://model.geneontology.org/568b0f9600000284
          assert(attrsOKGValues.flatMap(_.flatMap(_.value_url)))(Assertion.contains("http://model.geneontology.org/568b0f9600000284")) &&
          // At least one of these records should be infores:go-cam
          assert(attrsOKGValues.flatten.flatMap(_.value))(Assertion.contains("infores:go-cam"))
        }
      },
      testM("Check n1 results") {
        // Check that the n1 results are as expected.
        for {
          response <- responses
          kg <- ZIO.fromOption(response.message.knowledge_graph)
          results <- ZIO.fromOption(response.message.results)
          n1results = results.flatMap(_.node_bindings).filter(_._1 == "n1").flatMap(_._2)
          n1ids = n1results.map(_.id)
          // This should trigger an error if any node is returned without a corresponding
          // Knowledge Graph entry.
          n1kgs = n1ids.map(id => (id, kg.nodes(id))).toMap
          n1cats = n1kgs.transform((id, node) => node.categories.getOrElse(List()))
        } yield {
          // We got 11 results as of 2022-02-14; we don't expect to get fewer than that.
          assert(results.size)(Assertion.isGreaterThanEqualTo(11)) &&
          // Check for some expected nodes.
          // - MAP2K activity (GO:0004708)
          assert(n1ids)(Assertion.contains(IRI("http://purl.obolibrary.org/obo/GO_0004708"))) &&
          // - MAPK activity (GO:0004707)
          assert(n1ids)(Assertion.contains(IRI("http://purl.obolibrary.org/obo/GO_0004707"))) &&
          // Every n1 should be a 'biolink:BiologicalProcessOrActivity'
          assert(n1cats.values)(Assertion.forall(Assertion.contains(BiolinkClass("BiologicalProcessOrActivity"))))
        }
      }
    )
  }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val camkpapiTestLayer = Blocking.live >>> TestContainer.camkpapi
  val camkpapiLayer = Blocking.live >>> HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer
  val testLayer = (configLayer ++ testEnvironment ++ camkpapiTestLayer ++ camkpapiLayer).mapError(TestFailure.die)

  def spec = suite("original_knowledge_source tests")(
    testPositivelyRegulatedByMAP3K,
  ).provideLayerShared(testLayer) @@ TestAspect.sequential

}

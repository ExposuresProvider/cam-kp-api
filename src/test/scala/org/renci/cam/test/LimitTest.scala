package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.apache.jena.query.{Query, QuerySolution}
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.SPARQLQueryExecutor.SPARQLCache
import org.renci.cam.{AppConfig, Biolink, HttpClient, QueryService, SPARQLQueryExecutor}
import org.renci.cam.domain._
import zio.ExecutionStrategy.Sequential
import zio.blocking.Blocking
import zio.cache.Cache
import zio.config.{ConfigModule, ZConfig}
import zio.{Layer, RIO, Runtime, ZIO, ZLayer}
import zio.config.typesafe.TypesafeConfig
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test.TestAspect.{debug, ignore}
import zio.test._
import zio.test.environment.testEnvironment

/**
 * Tests whether queries work as expected with limits, i.e. given a query, can we increase limits arbitrarily
 * and see consistently increasing numbers of results?
 */
object LimitTest extends DefaultRunnableSpec with LazyLogging {

  val testQueryWithExpectedResults = {
    val queryGraphExpectedResults = 30 // expected results as of 2022-may-18
    val testQueryGraph = TRAPIQueryGraph(
      Map(
        "n0" -> TRAPIQueryNode(Some(List(IRI("http://purl.obolibrary.org/obo/CHEBI_15361"))), None, None),
        "n1" -> TRAPIQueryNode(None, Some(List(BiolinkClass("NamedThing"))), None)
      ),
      Map(
        "e0" -> TRAPIQueryEdge(Some(List(BiolinkPredicate("related_to"))), "n0", "n1", None)
      )
    )

    val limitsToTest = Seq(1, 2, 3, 4, 5, 10, 20, 30, 40, 50)
    suiteM("testQueryWithExpectedResults") {
      ZStream.fromIterable(limitsToTest)
        .map(limit => testM(s"Test query with limit of ${limit} expecting ${queryGraphExpectedResults} results") {
          for {
            message <- QueryService.run(limit, false, testQueryGraph)
            _ = println(s"Retrieved ${message.results.get.size} results when limit=${limit}")
            results = message.results.get
          } yield {
            /*
            logger.info(s"Knowledge graph:")
            logger.info(s" - Nodes:")
            message.knowledge_graph.foreach(_.nodes.foreach(node =>
              logger.info(s"   - ${node._1}: ${node._2}")
            ))
            logger.info(s" - Edges:")
            message.knowledge_graph.foreach(_.edges.foreach(edge =>
              logger.info(s"   - ${edge._1}: ${edge._2}")
            ))

            logger.info(s"Results:")
            for ((r, index) <- results.zipWithIndex) {
              logger.info(s" - [${index + 1}] ${r}")
            }
             */
            assert(results.size)(Assertion.isGreaterThan(0)) &&
              assert(results.size)(Assertion.equalTo(Math.min(queryGraphExpectedResults, limit)))
          }
        }).runCollect
    }
  }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val testLayer = HttpClient.makeHttpClientLayer ++ Biolink.makeUtilitiesLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer

  def spec = suite("Limit tests")(
    testQueryWithExpectedResults
  ).provideCustomLayer(testLayer.mapError(TestFailure.die))
}

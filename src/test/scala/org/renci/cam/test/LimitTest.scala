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
import zio.{Layer, ZIO, ZLayer}
import zio.config.typesafe.TypesafeConfig
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
    val queryGraphExpectedResults = 14 // As of 2022mar16
    val testQueryGraph = TRAPIQueryGraph(
      Map(
        "n0" -> TRAPIQueryNode(Some(List(IRI("http://purl.obolibrary.org/obo/CHEBI_15361"))), None, None),
        "n1" -> TRAPIQueryNode(None, Some(List(BiolinkClass("NamedThing"))), None)
      ),
      Map(
        "e0" -> TRAPIQueryEdge(Some(List(BiolinkPredicate("related_to"))), "n0", "n1", None)
      )
    )

    def generateTestForLimit(limit: Int) =
      testM(f"Test query with ${queryGraphExpectedResults} results, limit ${limit}") {
        for {
          message <- QueryService.run(limit, false, testQueryGraph)
          _ = println(f"Retrieved ${message.results.get.size} results when limit=${limit}")
          results = message.results.get
        } yield {
          assert(results.size)(Assertion.isGreaterThan(0)) &&
            assert(results.size)(Assertion.equalTo(Math.min(queryGraphExpectedResults, limit)))
        }
      }

    // val limitsToTest = Seq(0, 1, 2, 3, 4, 5, 10, 20, 30, 40)
    suite("testQueryWithExpectedResults")(
      generateTestForLimit(0),
      generateTestForLimit(1),
      generateTestForLimit(2),
      generateTestForLimit(3),
      generateTestForLimit(4),
      generateTestForLimit(5),
      generateTestForLimit(10),
      generateTestForLimit(20),
      generateTestForLimit(30),
      generateTestForLimit(40),
      generateTestForLimit(50),
      generateTestForLimit(60),
      generateTestForLimit(70),
      generateTestForLimit(80),
      generateTestForLimit(90),
      generateTestForLimit(100)
    )
  }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val testLayer = HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer

  def spec = suite("Limit tests")(
    testQueryWithExpectedResults
  ).provideCustomLayer(testLayer.mapError(TestFailure.die))
}

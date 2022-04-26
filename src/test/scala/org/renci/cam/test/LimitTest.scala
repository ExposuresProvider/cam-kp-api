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
import zio.{Layer, RIO, ZIO, ZLayer, Runtime}
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
    val queryGraphExpectedResults = 25 // expected results as of 2022-mar-23
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
    suite("testQueryWithExpectedResults")(
      testM(s"Test query expecting ${queryGraphExpectedResults} results") {
        ZIO.foreach(limitsToTest) { limit =>
          for {
            message <- QueryService.run(limit, false, testQueryGraph)
            _ = println(s"Retrieved ${message.results.get.size} results when limit=${limit}")
            results = message.results.get
          } yield {
            assert(results.size)(Assertion.isGreaterThan(0)) &&
              assert(results.size)(Assertion.equalTo(Math.min(queryGraphExpectedResults, limit)))
          }
        }.map(_.reduce(_ && _))
      }
    )
  }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val testLayer = HttpClient.makeHttpClientLayer ++ Biolink.makeUtilitiesLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer

  def spec = suite("Limit tests")(
    testQueryWithExpectedResults
  ).provideCustomLayer(testLayer.mapError(TestFailure.die))
}

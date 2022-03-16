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

  val testVariousLimits = {
    val limitsToTest = List(10, 20, 30, 40)
    val testQueryGraph = TRAPIQueryGraph(
      Map(
        "n0" -> TRAPIQueryNode(Some(List(IRI("http://purl.obolibrary.org/obo/CHEBI_15361"))), None, None),
        "n1" -> TRAPIQueryNode(None, Some(List(BiolinkClass("NamedThing"))), None)
      ),
      Map(
        "e0" -> TRAPIQueryEdge(Some(List(BiolinkPredicate("related_to"))), "n0", "n1", None)
      )
    )

    suite("testVariousLimits")(
      testM("Test a limit of 10") {
        val limit = 10
        for {
          exec <- QueryService.run(limit, false, testQueryGraph)
          _ = logger.debug(f"Obtained ${exec.results.get.size} results when querying for ${limit}")
          results = exec.results.get
        } yield {
          assert(results.size)(Assertion.isGreaterThan(0)) &&
            assert(results.size)(Assertion.isLessThanEqualTo(limit))
        }
      }
    )
  }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val sparqlCacheLayer = SPARQLQueryExecutor.makeCache.toLayer
  val camkpapiLayer = Blocking.live >>> HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer
  val testLayer = (configLayer ++ camkpapiLayer).mapError(TestFailure.die)

  def spec = suite("Limit tests")(
    testVariousLimits
  ).provideLayerShared(testLayer)
}

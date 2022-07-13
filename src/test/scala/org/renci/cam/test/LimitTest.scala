package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import org.renci.cam.domain._
import org.renci.cam._
import zio.Layer
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.stream.ZStream
import zio.test._

/** Tests whether queries work as expected with limits, i.e. given a query, can we increase limits arbitrarily and see consistently
  * increasing numbers of results?
  */
object LimitTest extends DefaultRunnableSpec with LazyLogging {

  /** For a query with a certain number of expected results, this code block creates a number of tests that test whether setting `limit`
    * from 1..50 return the correct number of expected results.
    */
  val testQueryWithExpectedResults = {
    // Expected results as of 2022-may-18 for the test query
    val queryGraphExpectedResults = 30
    // The limits to test -- this should be set up to go neatly within
    val limitsToTest = Seq(1, 2, 3, 4, 5, 10, 20, 30, 40, 50)
    // The test query: pyruvate related_to $[NamedThing]
    val testQueryGraph = TRAPIQueryGraph(
      Map(
        "n0" -> TRAPIQueryNode(Some(List(IRI("http://purl.obolibrary.org/obo/CHEBI_15361"))), None, None),
        "n1" -> TRAPIQueryNode(None, Some(List(BiolinkClass("NamedThing"))), None)
      ),
      Map(
        "e0" -> TRAPIQueryEdge(Some(List(BiolinkPredicate("related_to"))), "n0", "n1", None)
      )
    )

    /*
     * Try querying the test query, and check to see if (1) we get back the expected number of results
     * (this will change as the backend changes) and (2) make sure that we get back the correct number
     * of limited results as we adjust the limit as per `limitsToTest`.
     */
    suiteM("testQueryWithExpectedResults") {
      ZStream
        .fromIterable(limitsToTest)
        .map(limit =>
          testM(s"Test query with limit of $limit expecting $queryGraphExpectedResults results") {
            for {
              message <- QueryService.run(limit, testQueryGraph)
              _ = logger.info(s"Retrieved ${message.results.get.size} results when limit=$limit")
              results = message.results.get
            } yield {
              logger.debug(s"Knowledge graph:")
              logger.debug(s" - Nodes:")
              message.knowledge_graph.foreach(_.nodes.foreach(node => logger.debug(s"   - ${node._1}: ${node._2}")))
              logger.debug(s" - Edges:")
              message.knowledge_graph.foreach(_.edges.foreach(edge => logger.debug(s"   - ${edge._1}: ${edge._2}")))
              logger.debug(s"Results:")
              for ((r, index) <- results.zipWithIndex)
                logger.debug(s" - [${index + 1}] $r")

              assert(results.size)(Assertion.isGreaterThan(0)) &&
              assert(results.size)(Assertion.equalTo(Math.min(queryGraphExpectedResults, limit)))
            }
          })
        .runCollect
    }
  }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val testLayer = HttpClient.makeHttpClientLayer ++ Biolink.makeUtilitiesLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer

  def spec: Spec[environment.TestEnvironment, TestFailure[Throwable], TestSuccess] = suite("Limit tests")(
    testQueryWithExpectedResults
  ).provideCustomLayer(testLayer.mapError(TestFailure.die))

}

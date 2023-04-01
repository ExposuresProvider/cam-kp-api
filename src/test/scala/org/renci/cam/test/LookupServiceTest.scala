package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe.syntax.EncoderOps
import org.apache.jena.query.{Query, QuerySolution}
import org.http4s.Method.GET
import org.http4s.{EntityDecoder, Request, Uri}
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.Server.EndpointEnv
import org.renci.cam._
import zio.blocking.effectBlockingIO
import zio.cache.Cache
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.duration.durationInt
import zio.interop.catz.concurrentInstance
import zio.stream.ZStream
import zio.test.environment.Live
import zio.test.{assert, _}
import zio.{Layer, Task, ZIO, ZLayer, ZManaged}

import java.io.{File, FileWriter, PrintWriter}

/** Test the `/lookup` endpoint (implemented by LookupService)
  */
object LookupServiceTest extends DefaultRunnableSpec with LazyLogging {

  /** The time limit for an individual lookup query (in testIdentifier()). */
  val LOOKUP_TIME_LIMIT = 60.second

  /** Node normalization endpoint. Defaults to the production NodeNorm, but can be overriden by setting the NODE_NORM_URL environmental
    * variable.
    */
  val NODE_NORM_URL = sys.env.get("NODE_NORM_URL") match {
    case Some(url) => url
    case None      => "https://nodenorm.transltr.io/1.3/get_normalized_nodes"
  }

  /** We should be able to get some default results just by hitting the `/lookup` endpoint. */
  val testDefault =
    suite("testDefault") {
      testM("Check the default lookup") {
        for {
          // Retrieve /lookup from the server, without a NODE_NORM_URL or any other
          server <- Server.httpApp
          response <- server(Request(uri = Uri.unsafeFromString("/lookup")))
          content <- EntityDecoder.decodeText(response)
          resultOrErrorJson <- ZIO.fromEither(io.circe.parser.parse(content))

          result <- ZIO.fromEither(resultOrErrorJson.as[LookupService.Result])
          _ = logger.debug(s"Results: ${result.asJson.deepDropNullValues.spaces2SortKeys}")
        } yield assert(content)(Assertion.isNonEmptyString) &&
          assert(result.subjectTriples)(Assertion.isNonEmpty) &&
          assert(result.objectTriples)(Assertion.isNonEmpty) &&
          assert(result.relations)(Assertion.isNonEmpty) &&
          assert(result.qualifiedBiolinkPredicates)(Assertion.isNonEmpty)
      }
    }

  /** Generate a test for a single identifier. We test whether the identifier returns a properly formed response, with subject and object
    * triples. The lookup response is written to a file in the src/test/resources/lookup-service-test directory.
    *
    * Eventually, we would like to ensure that these identifiers return predicates and biolinkPredicates, but this isn't necessary yet, as
    * this is not yet true for all identifiers.
    *
    * @param id
    *   The identifier to test.
    * @return
    *   A ZSpec that will test the single identifier being queried.
    */
  def testIdentifier(id: String): ZSpec[Live with EndpointEnv, Throwable] =
    testM(s"Test whether we can retrieve results for identifier ${id}") {
      val idFilenameSafe = id.replaceAll("\\W", "_")
      val outputFile = new File(s"./src/test/resources/lookup-service-test/${idFilenameSafe}.json")

      for {
        server <- Server.httpApp
        uri = Uri
          .unsafeFromString("/lookup")
          .withQueryParam("subject", id)
          .withQueryParam("nodeNormURL", NODE_NORM_URL)
        _ = logger.info(s"GET ${uri}")
        response <- server(Request(GET, uri))
        content <- EntityDecoder.decodeText(response)
        resultOrErrorJson <- ZIO.fromEither(io.circe.parser.parse(content))
        _ = logger.debug(s"Raw JSON results for identifier ${id}: ${resultOrErrorJson.deepDropNullValues.spaces2SortKeys}")
        result <- ZIO.fromEither(resultOrErrorJson.as[LookupService.Result])
        // _ = logger.debug(s"Results for identifier ${id}: ${result.asJson.deepDropNullValues.spaces2SortKeys}")
        fileWritten <- effectBlockingIO(new PrintWriter(new FileWriter(outputFile)))
          .bracketAuto { pw =>
            pw.println(result.asJson.deepDropNullValues.spaces2SortKeys)
            ZIO.succeed(true)
          }
      } yield assert(content)(Assertion.isNonEmptyString) &&
        assert(result.subjectTriples)(Assertion.isNonEmpty) &&
        assert(result.objectTriples)(Assertion.isNonEmpty) &&
        // Eventually, all of these test IDs should have relations and biolinkRelations,
        // but that isn't the case yet.
        //
        // assert(result.relations)(Assertion.isNonEmpty) &&
        // assert(result.biolinkPredicates)(Assertion.isNonEmpty) &&
        assert(fileWritten)(Assertion.isTrue)
    } @@ TestAspect.timeout(LOOKUP_TIME_LIMIT)

  /* Generate tests for all the identifiers in the lookupServiceIdFile. */

  /** A text file containing one identifier per line. */
  val lookupServiceIdFile = new File("./src/test/resources/lookup-service-test/ids.txt")

  /** A spec made up of tests for every identifier in the lookupServiceIdFile. */
  val testIdentifiersInFile: Spec[Live with EndpointEnv, TestFailure[Throwable], TestSuccess] =
    suiteM(s"Test identifiers in ${lookupServiceIdFile}") {
      ZStream
        .fromIteratorManaged(
          ZManaged
            .fromAutoCloseable(
              Task(scala.io.Source.fromFile(lookupServiceIdFile))
            )
            .map(_.getLines())
        )
        .map(testIdentifier)
        .runCollect
        .mapError(TestFailure.fail)
    }

  /* Test configuration */

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)

  val testLayer: ZLayer[
    Any,
    Throwable,
    HttpClient with ZConfig[Biolink.BiolinkData] with ZConfig[AppConfig] with ZConfig[Cache[Query, Throwable, List[QuerySolution]]]] =
    HttpClient.makeHttpClientLayer ++ Biolink.makeUtilitiesLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer

  def spec: Spec[environment.TestEnvironment, TestFailure[Throwable], TestSuccess] = suite("Limit tests")(
    testDefault,
    testIdentifiersInFile
  ).provideCustomLayer(testLayer.mapError(TestFailure.die))

}

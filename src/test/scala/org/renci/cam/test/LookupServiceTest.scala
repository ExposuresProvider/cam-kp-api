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
import zio.interop.catz.concurrentInstance
import zio.stream.ZStream
import zio.test.{assert, _}
import zio.{Layer, Task, ZIO, ZLayer}

import java.io.{File, FileWriter, PrintWriter}
import scala.io.Source

/** Test the `/lookup` endpoint (implemented by LookupService)
  */
object LookupServiceTest extends DefaultRunnableSpec with LazyLogging {

  /** We should be able to get some default results just by hitting the `/lookup` endpoint. */
  val testDefault =
    suite("testDefault") {
      testM("Check the default lookup") {
        for {
          // Retrieve /docs/docs.yaml from the server.
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
          assert(result.biolinkPredicates)(Assertion.isNonEmpty)
      }
    }

  def testIdentifier(id: String): ZSpec[EndpointEnv, Throwable] =
    testM(s"Test whether we can retrieve results for identifier ${id}") {
      val idFilenameSafe = id.replaceAll("\\W", "_")
      val outputFile = new File(s"./src/test/resources/lookup-service-test/${idFilenameSafe}.json")

      for {
        server <- Server.httpApp
        response <- server(Request(GET, Uri.unsafeFromString("/lookup").withQueryParam("subject", id)))
        content <- EntityDecoder.decodeText(response)
        resultOrErrorJson <- ZIO.fromEither(io.circe.parser.parse(content))
        result <- ZIO.fromEither(resultOrErrorJson.as[LookupService.Result])
        _ = logger.debug(s"Results for identifier ${id}: ${result.asJson.deepDropNullValues.spaces2SortKeys}")
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
    }

  val lookupServiceIdFile = new File("./src/test/resources/lookup-service-test/ids.txt")

  val testIdentifiersInFile: Spec[EndpointEnv, TestFailure[Throwable], TestSuccess] =
    suiteM(s"Test identifiers in ${lookupServiceIdFile}") {
      // noinspection SourceNotClosed
      ZStream
        .fromIteratorEffect(Task(Source.fromFile(lookupServiceIdFile).getLines()))
        .map(testIdentifier)
        .runCollect
        .mapError(TestFailure.fail)
    }

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

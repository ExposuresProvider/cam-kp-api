package org.renci.cam.it

import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import org.http4s._
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.headers.{`Content-Type`, Accept}
import org.http4s.implicits._
import org.renci.cam.Biolink.biolinkData
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, IRI, TRAPIAttribute, TRAPIResponse}
import org.renci.cam.{AppConfig, Biolink, HttpClient, Implicits}
import zio.blocking.Blocking
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.interop.catz._
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._
import zio.{Layer, Task, ZIO}

import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import scala.jdk.CollectionConverters._

/** Run example queries against a CAM-KP-API endpoint, writing the responses out in `src/it/resources/example-results` and then testing the
  * expectations described in the example file.
  */
object ExampleQueriesEndpointTest extends DefaultRunnableSpec {
  val exampleDir: Path = Paths.get("src/it/resources/examples")
  val exampleResultsDir: Path = Paths.get("src/it/resources/example-results")

  val endpointToTest: Uri =
    sys.env.get("CAM_KP_ENDPOINT") match {
      case None      => uri"https://cam-kp-api.renci.org/1.2.0/query"
      case Some(str) => Uri.fromString(str).toOption.get
    }

  val testEachExampleFile: Spec[ZConfig[Biolink.BiolinkData] with HttpClient, TestFailure[Throwable], TestSuccess] = {
    // List of example files to process.
    val exampleFiles = Files
      .walk(exampleDir)
      .iterator()
      .asScala
      .filter(Files.isRegularFile(_))
      .filter(_.toString.toLowerCase.endsWith(".json"))
      .toSeq

    suiteM("Test example files in the src/it/resources/examples directory") {
      ZStream
        .fromIterable(exampleFiles)
        .map(exampleFile =>
          testM(s"Testing ${exampleDir.relativize(exampleFile)}") {
            val exampleText = {
              val source = Source.fromFile(exampleFile.toFile)
              source.getLines().mkString("\n")
            }
            for {
              httpClient <- HttpClient.client
              biolinkData <- biolinkData

              // Read the example JSON file.
              exampleJson <- ZIO.fromEither(io.circe.parser.parse(exampleText))
              descriptionOpt = exampleJson.hcursor.downField("description").as[String].toOption
              limit = exampleJson.hcursor.downField("limit").as[Long].toOption.getOrElse(0)
              minExpectedResultsOpt = exampleJson.hcursor.downField("minExpectedResults").as[Int].toOption
              maxExpectedResultsOpt = exampleJson.hcursor.downField("maxExpectedResults").as[Int].toOption
              messageText <- ZIO.fromEither(exampleJson.hcursor.downField("message").as[Json].map(_.noSpaces))

              // Prepare request for the CAM-KP-API endpoint.
              request = Request[Task](Method.POST, endpointToTest.withQueryParam("limit", limit.toString))
                .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
                .withEntity("{\"message\": " + messageText + "}")
              response <- httpClient.expect[Json](request)

              // Write out the response in `src/it/resources/example-results` for debugging.
              outputFilename = exampleResultsDir.resolve(exampleDir.relativize(exampleFile))
              _ = Files.createDirectories(outputFilename.getParent)
              _ = Files.writeString(outputFilename, response.spaces2SortKeys)

              // Translate the response into a TRAPIResponse for testing.
              trapiResponse <- ZIO.fromEither(
                {
                  implicit val decoderIRI: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
                  implicit val keyDecoderIRI: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
                  implicit val decoderBiolinkClass: Decoder[BiolinkClass] = Implicits.biolinkClassDecoder(biolinkData.classes)
                  implicit val decoderBiolinkPredicate: Decoder[BiolinkPredicate] =
                    Implicits.biolinkPredicateDecoder(biolinkData.predicates)
                  implicit val decoderTRAPIAttribute: Decoder[TRAPIAttribute] = deriveDecoder[TRAPIAttribute]

                  response.as[TRAPIResponse]
                }
              )
            } yield assert(descriptionOpt)(isSome(isNonEmptyString)) &&
              assert(messageText)(isNonEmptyString) &&
              assert(trapiResponse.status)(isSome(equalTo("Success"))) &&
              // If a minExpectedResults is provided, make sure that the number of results is indeed greater than or equal to it.
              (minExpectedResultsOpt match {
                case None => assertCompletes
                case Some(minExpectedResults) =>
                  val resultCount = trapiResponse.message.results.getOrElse(List()).size
                  assert(resultCount)(isGreaterThanEqualTo(minExpectedResults))
              }) &&
              // If a maxExpectedResults is provided, make sure that the number of results is indeed less than or equal to it.
              (maxExpectedResultsOpt match {
                case None => assertCompletes
                case Some(maxExpectedResults) =>
                  val resultCount = trapiResponse.message.results.getOrElse(List()).size
                  assert(resultCount)(isLessThanEqualTo(maxExpectedResults))
              })
          })
        .runCollect
    }
  }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val camkpapiLayer = Blocking.live >>> HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer
  val testLayer = (configLayer ++ camkpapiLayer).mapError(TestFailure.die)

  def spec = suite("ExampleQueriesTest")(
    testEachExampleFile
  ).provideCustomLayer(testLayer)

}

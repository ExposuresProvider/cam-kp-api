package org.renci.cam.it

import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import io.circe.syntax.EncoderOps
import org.http4s._
import org.http4s.circe.jsonDecoder
import org.http4s.headers.{`Content-Type`, Accept}
import org.http4s.implicits._
import org.renci.cam.Biolink.biolinkData
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, IRI, TRAPIAttribute, TRAPIMessage, TRAPIQuery, TRAPIQueryEdge, TRAPIQueryGraph, TRAPIQueryNode, TRAPIResponse}
import org.renci.cam.{AppConfig, Biolink, HttpClient, Implicits}
import zio.blocking.Blocking
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.interop.catz._
import zio.test.Assertion._
import zio.test._
import zio.{Layer, Task, ZIO}

import java.nio.file.{Path, Paths}
import scala.jdk.CollectionConverters._

object EnhanceEdgesTest extends DefaultRunnableSpec {
  val exampleDir: Path = Paths.get("src/it/resources/enhance-edge-tests")
  val exampleResultsDir: Path = Paths.get("src/it/resources/example-results")

  val endpointToTest: Uri =
    sys.env.get("CAM_KP_ENDPOINT") match {
      case None      => uri"https://cam-kp-api.renci.org/1.3.0/query"
      case Some(str) => Uri.fromString(str).toOption.get
    }

  val limit: Int = sys.env.getOrElse("CAM_KP_LIMIT", 1000).toString.toInt

  def testEdge(subj: String,
               pred: String,
               obj: String): Spec[ZConfig[Biolink.BiolinkData] with HttpClient, TestFailure[Throwable], TestSuccess] =
    testM(s"Testing ${subj} ${pred} ${obj}") {
      for {
        biolinkData <- biolinkData
        httpClient <- HttpClient.client

        messageText = {
          implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
          implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
          implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder
          implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] =
            Implicits.biolinkPredicateEncoder(biolinkData.prefixes)

          val subjIRI = IRI(subj)
          val objIRI = IRI(obj)
          val queryGraph = TRAPIQueryGraph(
            nodes = Map(
              "n0" -> TRAPIQueryNode(ids = Some(List(subjIRI)), None, None, None),
              "n1" -> TRAPIQueryNode(ids = Some(List(objIRI)), None, None, None)
            ),
            edges = Map(
              "e0" -> TRAPIQueryEdge(
                predicates = Some(List(BiolinkPredicate("related_to"))),
                subject = "n0",
                `object` = "n1"
              ))
          )
          val message = TRAPIMessage(Some(queryGraph), None, None)

          TRAPIQuery(message = message, log_level = None).asJson.deepDropNullValues.noSpaces
        }
        request = Request[Task](Method.POST, endpointToTest.withQueryParam("limit", limit.toString))
          .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
          .withEntity(messageText)
        response <- httpClient.expect[Json](request)
        trapiResponse <- ZIO.fromEither(
          {
            implicit val decoderIRI: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
            implicit val keyDecoderIRI: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
            implicit val decoderBiolinkClass: Decoder[BiolinkClass] = Implicits.biolinkClassDecoder(biolinkData.classes)
            implicit val decoderBiolinkPredicate: Decoder[BiolinkPredicate] =
              Implicits.biolinkPredicateDecoder(biolinkData.predicates)
            implicit lazy val decoderTRAPIAttribute: Decoder[TRAPIAttribute] = deriveDecoder[TRAPIAttribute]

            response.as[TRAPIResponse]
          }
        )
        results = trapiResponse.message.results
      } yield assert(results)(Assertion.isSome(Assertion.isNonEmpty))
    }

  // Let's test one out.
  val testOne = testEdge("UniProtKB:P56856", "biolink:has_fisher_exact_test_p_value_with", "CHEMBL.COMPOUND:CHEMBL1201129")

  /*
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
              example <- ZIO.fromEither(
                {
                  implicit val decoderIRI: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
                  implicit val keyDecoderIRI: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
                  implicit val decoderBiolinkClass: Decoder[BiolinkClass] = Implicits.biolinkClassDecoder(biolinkData.classes)
                  implicit val decoderBiolinkPredicate: Decoder[BiolinkPredicate] =
                    Implicits.biolinkPredicateDecoder(biolinkData.predicates)
                  implicit lazy val decoderTRAPIAttribute: Decoder[TRAPIAttribute] = deriveDecoder[TRAPIAttribute]

                  exampleJson.as[ExampleJsonFile]
                }
              )

              descriptionOpt = example.description
              limit = example.limit.getOrElse(0)
              minExpectedResultsOpt = example.minExpectedResults
              maxExpectedResultsOpt = example.maxExpectedResults

              // Prepare request for the CAM-KP-API endpoint.
              messageText = {
                implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
                implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
                implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder
                implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] =
                  Implicits.biolinkPredicateEncoder(biolinkData.prefixes)

                TRAPIQuery(message = example.message, log_level = None).asJson.deepDropNullValues.noSpaces
              }
              // _ = println(s"messageText = ${messageText}")
              request = Request[Task](Method.POST, endpointToTest.withQueryParam("limit", limit.toString))
                .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
                .withEntity(messageText)
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
                  implicit lazy val decoderTRAPIAttribute: Decoder[TRAPIAttribute] = deriveDecoder[TRAPIAttribute]

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
   */

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val camkpapiLayer = Blocking.live >>> HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer
  val testLayer = (configLayer ++ camkpapiLayer).mapError(TestFailure.die)

  def spec = suite("EnhanceEdgesTest")(
    testOne
  ).provideCustomLayer(testLayer)

}

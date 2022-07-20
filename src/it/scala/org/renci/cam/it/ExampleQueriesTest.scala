package org.renci.cam.it

import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import org.http4s._
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.headers.{`Content-Type`, Accept}
import org.http4s.implicits._
import org.renci.cam.Biolink.biolinkData
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

import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.jdk.CollectionConverters._

object ExampleQueriesTest extends DefaultRunnableSpec {
  val exampleDir = Paths.get("src/it/resources/examples")

  val endpointToTest = uri"https://cam-kp-api.renci.org/1.2.0/query"

  val testEachExampleFile = {
    // List of example files to process.
    val exampleFiles = Files
      .walk(exampleDir)
      .iterator()
      .asScala
      .filter(Files.isRegularFile(_))
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

              exampleJson <- ZIO.fromEither(io.circe.parser.parse(exampleText))
              descriptionOpt = exampleJson.hcursor.downField("description").as[String].toOption
              messageText <- ZIO.fromEither(exampleJson.hcursor.downField("message").as[Json].map(_.noSpaces))
              request = Request[Task](Method.POST, endpointToTest)
                .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
                .withEntity("{\"message\": " + messageText + "}")
              response <- httpClient.expect[Json](request)
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
              _ = println("response: " + response)
            } yield assert(descriptionOpt)(isSome(isNonEmptyString)) &&
              assert(messageText)(isNonEmptyString) &&
              assert(trapiResponse.status)(isSome(equalTo("Success")))
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

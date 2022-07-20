package org.renci.cam.it

import io.circe._
import org.renci.cam.{AppConfig, Biolink, HttpClient}
import zio.blocking.Blocking
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._
import zio.{Layer, ZIO}

import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.jdk.CollectionConverters._

object ExampleQueriesTest extends DefaultRunnableSpec {
  val exampleDir = Paths.get("src/it/resources/examples")

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
              exampleJson <- ZIO.fromEither(parser.parse(exampleText))
              descriptions = exampleJson \\ "description"
            } yield assert(descriptions)(hasSize(equalTo(1))) && assert(descriptions.head.asString)(isSome(isNonEmptyString))
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

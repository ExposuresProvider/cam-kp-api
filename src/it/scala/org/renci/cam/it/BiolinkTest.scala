package org.renci.cam.it

import com.typesafe.scalalogging.LazyLogging
import io.circe.parser._
import org.http4s._
import org.http4s.headers._
import org.http4s.implicits._
import org.renci.cam._
import zio._
import zio.interop.catz._
import zio.test.Assertion.{contains, _}
import zio.test._
import zio.test.environment.testEnvironment

object BiolinkTest extends DefaultRunnableSpec with LazyLogging {

  val testLayer = (testEnvironment ++ HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer).mapError(TestFailure.die)

  val testDownloadParseAndFilter = suite("BiolinkTest")(
    testM("download, parse, & filter") {
      for {
        httpClient <- HttpClient.client
        uri = uri"https://biolink.github.io/biolink-model/context.jsonld"
        request = Request[Task](Method.GET, uri).withHeaders(Accept(MediaType.application.`ld+json`),
                                                             `Content-Type`(MediaType.application.`ld+json`))
        response <- httpClient.expect[String](request)
        parsed <- ZIO.fromEither(parse(response))
        contextJson <- ZIO.fromOption(parsed.hcursor.downField("@context").focus)
        contextJsonObject <- ZIO.fromOption(contextJson.asObject)
        firstPass = contextJsonObject.toIterable
          .filter(entry => entry._2.isObject && entry._2.asObject.get.contains("@id") && entry._2.asObject.get.contains("@prefix"))
          .map { entry =>
            entry._1 -> entry._2.hcursor.downField("@id").focus.get.toString().replaceAll("\"", "")
          }
          .toMap
        secondPass = contextJsonObject.toIterable
          .filter(entry => entry._2.isString && !entry._1.equals("@vocab") && !entry._1.equals("id"))
          .map { entry =>
            entry._1 -> entry._2.toString().replaceAll("\"", "")
          }
          .toMap
        map = firstPass ++ secondPass
        _ = println(map)
      } yield assert(response)(isNonEmptyString) && assert(map.keys)(
        contains("NCBIGene") && contains("CHEBI") && contains("GO") && contains("biolink") && not(contains("@vocab")) && not(
          contains("id"))) && assert(map.values)(contains("https://w3id.org/biolink/vocab/"))
    }
  )

  val testLocalPrefixes = suite("localPrefixes")(
    testM("test Biolink.localPrefixes") {
      for {
        biolinkData <- Biolink.biolinkData
        prefixes = biolinkData.prefixes
      } yield assert(prefixes)(isNonEmpty) && assert(prefixes.keys)(contains("sesame"))
    }
  )

  def spec = suite("Biolink tests")(testDownloadParseAndFilter, testLocalPrefixes).provideLayerShared(testLayer) @@ TestAspect.sequential

}

package org.renci.cam.it

import io.circe._
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

object BiolinkTest extends DefaultRunnableSpec {

  val testLayer = (testEnvironment ++ HttpClient.makeHttpClientLayer).mapError(TestFailure.die)

  val decoder = Decoder.decodeMap(KeyDecoder.decodeKeyString, Decoder.decodeString)

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
        contains("NCBIGENE") && contains("CHEBI") && contains("GO") && contains("biolink") && not(contains("@vocab")) && not(
          contains("id"))) && assert(map.values)(contains("https://w3id.org/biolink/vocab/"))
    }
  )

  def spec = suite("Biolink tests")(testDownloadParseAndFilter).provideLayerShared(testLayer)

}

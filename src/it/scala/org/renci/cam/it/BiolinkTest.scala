package org.renci.cam.it

import io.circe._
import io.circe.parser._
import org.http4s._
import org.http4s.headers._
import org.http4s.implicits._
import org.renci.cam._
import zio._
import zio.interop.catz._
import zio.test.Assertion._
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
        firstPass = contextJsonObject.toIterable.filter(entry => entry._2.isObject && entry._2.asObject.get.contains("@id") && entry._2.asObject.get.contains("@prefix")).map(entry => {
          entry._1 -> entry._2.hcursor.downField("@id").focus.get.toString()
        }).toMap
        secondPass = contextJsonObject.toIterable.filter(entry => entry._2.isString).map(entry => {
          entry._1 -> entry._2.toString()
        }).toMap
        map = firstPass ++ secondPass
      } yield assert(response)(isNonEmptyString) && assert(map.keys)(contains("NCBIGENE") && contains("CHEBI") && contains("GO"))
    }
  )

  def spec = suite("Biolink tests")(testDownloadParseAndFilter).provideLayerShared(testLayer)

}

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

object BiolinkTest extends DefaultRunnableSpec {

  val camkpapiLayer = HttpClient.makeHttpClientLayer
  val testLayer = zio.test.environment.testEnvironment ++ camkpapiLayer

  val decoder = Decoder.decodeMap(KeyDecoder.decodeKeyString, Decoder.decodeString)

  val testDownloadParseAndFilter = suite("BiolinkTest")(
    testM("download, parse, & filter") {
      val testCase = for {
        httpClient <- HttpClient.client
        uri = uri"https://biolink.github.io/biolink-model/context.jsonld"
        request = Request[Task](Method.GET, uri).withHeaders(Accept(MediaType.application.`ld+json`),
                                                             `Content-Type`(MediaType.application.`ld+json`))
        response <- httpClient.expect[String](request)
        parsed <- ZIO.fromEither(parse(response))
        contextJson <- ZIO.fromOption(parsed.hcursor.downField("@context").focus)
        filteredJson = contextJson.deepDropNullValues.mapObject(f =>
          f.filter(pred => pred._2.isString && pred._1 != "type" && pred._1 != "id" && pred._1 != "@vocab"))
//        _ <- ZIO.effect(Files.write(Paths.get("src/main/resources/prefixes.json"), filteredJson.toString().getBytes))
        map <- ZIO.fromEither(decoder.decodeJson(filteredJson))
      } yield assert(response)(isNonEmptyString) && assert(map.keys)(contains("NCBIGENE") && contains("CHEBI") && contains("GO"))
      testCase.provideCustomLayer(testLayer)
    }
  )

  def spec = suite("Biolink tests")(testDownloadParseAndFilter)

}

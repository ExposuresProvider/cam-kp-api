package org.renci.cam.it

import java.nio.file.{Files, Paths}
import io.circe._
import io.circe.parser._
import org.http4s._
import org.http4s.headers._
import org.http4s.implicits._
import zio._
import zio.interop.catz._
import zio.test.Assertion._
import zio.test._

import java.nio.file.{Files, Paths}
import org.renci.cam._
import zio.blocking.Blocking

object PullBiolinkTypeMapTest extends DefaultRunnableSpec {

  val camkpapiLayer = HttpClient.makeHttpClientLayer
  val testLayer = zio.test.environment.testEnvironment ++ camkpapiLayer

  val decoder = Decoder.decodeMap(KeyDecoder.decodeKeyString, Decoder.decodeString)

  val suite1 = suite("PullBiolinkTypeMapTestSpec")(
    testM("pull") {
      val testCase = for {
        httpClient <- HttpClient.client
        uri = uri"https://biolink.github.io/biolink-model/context.jsonld"
        request = Request[Task](Method.GET, uri).withHeaders(Accept(MediaType.application.`ld+json`), `Content-Type`(MediaType.application.`ld+json`))
        response <- httpClient.expect[String](request)
        parsed <- ZIO.fromEither(parse(response))
        contextJson <- ZIO.fromOption(parsed.hcursor.downField("@context").focus)
        filteredJson = contextJson.deepDropNullValues.mapObject(f =>
          f.filter(pred => pred._2.isString && pred._1 != "type" && pred._1 != "id" && pred._1 != "@vocab"))
        _ <- ZIO.effect(Files.write(Paths.get("src/main/resources/prefixes.json"), filteredJson.toString().getBytes))
        map <- ZIO.fromEither(decoder.decodeJson(filteredJson))
        _ = ZIO.effect(println(map.toString))
      } yield assert(map.keys)(contains())
      testCase.provideCustomLayer(testLayer)
    }
  )

  def spec = suite("PullBiolinkTypeMap tests")(suite1)

}

package org.renci.cam

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

object PullBiolinkTypeMapTest extends DefaultRunnableSpec {

  val decoder = Decoder.decodeMap(KeyDecoder.decodeKeyString, Decoder.decodeString)

  val suite1 = suite("PullBiolinkTypeMapTestSpec")(
    testM("pull") {
      for {
        httpClient <- HttpClient.makeHttpClient
        uri = uri"https://biolink.github.io/biolink-model/context.jsonld"
        request = Request[Task](Method.GET, uri).withHeaders(Accept(MediaType.application.`ld+json`),
                                                             `Content-Type`(MediaType.application.`ld+json`))
        response <- httpClient.use(_.expect[String](request))
        parsed <- ZIO.fromEither(parse(response))
        contextJson <- ZIO.fromOption(parsed.hcursor.downField("@context").focus)
        filteredJson = contextJson.deepDropNullValues.mapObject(f =>
          f.filter(pred => pred._2.isString && pred._1 != "type" && pred._1 != "id" && pred._1 != "@vocab"))
        _ <- ZIO.effect(Files.write(Paths.get("src/main/resources/prefixes.json"), filteredJson.toString().getBytes))
        map <- ZIO.fromEither(decoder.decodeJson(filteredJson))
        _ = ZIO.effect(println(map.toString))
      } yield assert(response)(isNonEmptyString)
    } //@@ ignore
  )

  def spec = suite("All tests")(suite1)

}

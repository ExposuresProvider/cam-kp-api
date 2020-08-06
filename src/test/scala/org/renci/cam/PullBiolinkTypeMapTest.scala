package org.renci.cam

import java.nio.file.{Files, Paths, StandardOpenOption}

import io.circe.generic.auto._
import io.circe.syntax._
import io.circe._
import io.circe.parser._
import org.http4s._
import org.http4s.headers._
import org.http4s.implicits._
import zio._
import zio.interop.catz._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

object PullBiolinkTypeMapTest extends DefaultRunnableSpec {

  val decoder = Decoder.decodeMap(KeyDecoder.decodeKeyString, Decoder.decodeString)

  def spec =
    suite("PullBiolinkTypeMapTestSpec")(
      testM("pull") {
        for {
          httpClient <- SPARQLQueryExecutor.makeHttpClient
          uri = uri"https://biolink.github.io/biolink-model/context.jsonld"
          request = Request[Task](Method.GET, uri).withHeaders(Accept(MediaType.application.`ld+json`),
                                                               `Content-Type`(MediaType.application.`ld+json`))
          response <- httpClient.use(_.expect[String](request))
          parsed <- Task.effect(parse(response).getOrElse(Json.Null))
          contextJson <- Task.effect(parsed.hcursor.downField("@context").focus.get)
          filteredJson <- Task.effect(contextJson.deepDropNullValues.mapObject(f =>
            f.filter(pred => pred._2.isString && pred._1 != "type" && pred._1 != "id" && pred._1 != "@vocab")))
          _ = Files.write(Paths.get("src/main/resources/prefixes.json"), filteredJson.toString().getBytes)
          map <- Task.effect(decoder.decodeJson(filteredJson).toOption.get)
          _ = println(map.toString)
        } yield assert(response)(isNonEmptyString)
      } //@@ ignore
    )

}

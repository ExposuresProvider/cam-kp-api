package org.renci.cam

import java.util.concurrent.Executors

import cats.effect.Blocker
import org.http4s._
import org.http4s.client._
import org.http4s.headers._
import org.http4s.implicits._
import zio.interop.catz._
import zio.test.Assertion._
import zio.test._
import zio.test.TestAspect._
import zio.{Runtime, Task, ZEnv}

object BlazegraphTest extends DefaultRunnableSpec {

  def spec =
    suite("BlazegraphTestSpec")(
      testM("bindings") {

        val query =
          """PREFIX bl: <https://w3id.org/biolink/vocab/>
              SELECT DISTINCT ?predicate WHERE { bl:has_participant <http://reasoner.renci.org/vocab/slot_mapping> ?predicate . }"""

        for {
          httpClient <- zio.ZIO.effect {
            val blockingPool = Executors.newFixedThreadPool(5)
            val blocker = Blocker.liftExecutorService(blockingPool)
            JavaNetClientBuilder[zio.Task](blocker).create
          }
          uri =
            uri"http://152.54.9.207:9999/blazegraph/sparql"
              .withQueryParam("query", query)
              .withQueryParam("format", "json")
          request = Request[Task](Method.POST, uri).withHeaders(Accept(MediaType.application.json),
                                                                `Content-Type`(MediaType.application.json))
          response <- httpClient.expect[String](request)
        } yield assert(response)(isNonEmptyString)
      } @@ ignore
    )

}

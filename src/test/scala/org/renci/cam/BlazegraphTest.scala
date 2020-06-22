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
import zio.{Runtime, Task, ZEnv}

object BlazegraphTest extends DefaultRunnableSpec {

  implicit val runtime: Runtime[ZEnv] = Runtime.default

  def spec =
    suite("BlazegraphTestSpec")(
      test("bindings") {

        val query =
          """PREFIX bl: <https://w3id.org/biolink/vocab/>
              SELECT DISTINCT ?predicate WHERE { bl:has_participant <http://reasoner.renci.org/vocab/slot_mapping> ?predicate . }"""

        val program = for {
          httpClient <- {
            val blockingPool = Executors.newFixedThreadPool(5)
            val blocker = Blocker.liftExecutorService(blockingPool)
            val httpClient: Client[zio.Task] = JavaNetClientBuilder[zio.Task](blocker).create
            zio.ZIO.effect(httpClient)
          }
          uri =
            uri"http://152.54.9.207:9999/blazegraph/sparql"
              .withQueryParam("query", query)
              .withQueryParam("format", "json")
          request = Request[Task](Method.POST, uri).withHeaders(Accept(MediaType.application.json),
                                                                `Content-Type`(MediaType.application.json))
          response <- httpClient.expect[String](request)
        } yield response

        val ret = runtime.unsafeRun(program)
        println("ret: " + ret)
        assert(ret)(isNonEmptyString)
      }
    )

}

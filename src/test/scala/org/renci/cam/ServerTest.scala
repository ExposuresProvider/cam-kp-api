package org.renci.cam

import java.util.concurrent.Executors

import cats.effect.Blocker

import org.http4s._
import org.http4s.client._
import org.http4s.headers._
import org.http4s.implicits._
import zio.{Runtime, Task, ZEnv}
import zio.interop.catz._
import zio.test.Assertion._
import zio.test._

object ServerTest extends DefaultRunnableSpec {

  implicit val runtime: Runtime[ZEnv] = Runtime.default

  def spec =
    suite("ServerSpec")(
      test("query service") {

        val body = """{
  "message": {
    "query_graph": {
      "nodes": [
        {
          "id": "n0",
          "type": "gene",
          "curie": "NCBIGENE:558"
        },
        {
          "id": "n1",
          "type": "biological_process"
        }
      ],
      "edges": [
        {
          "id": "e0",
          "source_id": "n1",
          "target_id": "n0",
          "type": "has_participant"
        }
      ]
    }
  }
}"""

        val program = for {
          httpClient <- {
            val blockingPool = Executors.newFixedThreadPool(5)
            val blocker = Blocker.liftExecutorService(blockingPool)
            val httpClient: Client[zio.Task] = JavaNetClientBuilder[zio.Task](blocker).create
            zio.ZIO.effect(httpClient)
          }
          uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1)
          request = Request[Task](Method.POST, uri)
            .withHeaders(Accept(MediaType.application.json))
            .withEntity(body)
          response <- httpClient.expect[String](request)
        } yield response

        val ret = runtime.unsafeRun(program)
        println("ret: " + ret)
        assert(ret)(isNonEmptyString)
      }
    )

}

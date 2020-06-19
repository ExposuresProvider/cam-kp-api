package org.renci.cam

import java.util.concurrent.Executors

import cats.effect.Blocker
import org.http4s._
import org.http4s.client._
import org.http4s.headers._
import org.http4s.implicits._
import zio.interop.catz._
import zio.test.Assertion.equalTo
import zio.test._
import zio.{Runtime, ZEnv}

import scala.concurrent.ExecutionContext

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

        val acceptHeader = Accept(MediaType.application.json)
        val contentTypeHeader = `Content-Type`(MediaType.application.json)

        val program = for {
          uri <- zio.ZIO.effect(uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1))
          request <- zio.ZIO.effect(
            Request[zio.Task](Method.POST, uri)
              .withHeaders(acceptHeader, contentTypeHeader)
              .withEntity(body))
          response <- {
            val blockingPool = Executors.newFixedThreadPool(5)
            val blocker = Blocker.liftExecutorService(blockingPool)
            val httpClient: Client[zio.Task] = JavaNetClientBuilder[zio.Task](blocker).create
            httpClient.expect[String](request)
          }
        } yield response

        val ret = runtime.unsafeRun(program)
        assert("")(equalTo(ret))

      }
    )

}

object TestHelpers {
  val blockingEC = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(5))
  val blocker = Blocker.liftExecutionContext(blockingEC)
}

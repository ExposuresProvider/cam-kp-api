package org.renci.cam

import cats.implicits._
import zio._
import zio.console._
import zio.test._
import zio.test.Assertion._
import zio.test.environment._
import io.circe._
import zio.interop.catz._
import zio.interop.catz.implicits._
import cats.effect.Blocker
import org.renci.cam.TestHelpers._
import org.http4s._
import org.http4s.headers._
import zio.test.Assertion.equalTo
import zio.test._
import org.http4s.client.blaze.BlazeClientBuilder
import scala.concurrent.ExecutionContext.Implicits
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import org.http4s.client.blaze._
import org.http4s.client._
import cats.effect.Blocker
import java.util.concurrent._

object ServerTest extends DefaultRunnableSpec {

  def spec =
    suite("ServerSpec")(
      testM("query service") {

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

        for {
          request <- ZIO.effect(
            Request[Task](Method.POST, Uri.uri("http://127.0.0.1:8080/query?limit=1&"))
              .withHeaders(Accept.parse("application/json").toOption.get,
                           `Content-Type`.parse("application/x-www-form-urlencoded").toOption.get)
              .withEntity(body)
          )
          response <- {
            val blockingPool = Executors.newFixedThreadPool(5)
            val blocker = Blocker.liftExecutorService(blockingPool)
            val httpClient: Client[Task] = JavaNetClientBuilder[Task](blocker).create
            httpClient.expect[String](request)
          }
        } yield assert(response)(equalTo(""))
      }
    )

}

object TestHelpers {
  val blockingEC = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(5))
  val blocker = Blocker.liftExecutionContext(blockingEC)
}

package org.renci.cam

import org.http4s._
import org.http4s.headers._
import org.http4s.implicits._
import zio._
import zio.interop.catz._
import zio.test.Assertion.equalTo
import zio.test._

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
          clientManaged <- QueryService.makeHttpClient
          request = Request[Task](Method.POST, uri"http://127.0.0.1:8080/query?limit=1&")
            .withHeaders(Accept(MediaType.application.json),
                         `Content-Type`(MediaType.application.`x-www-form-urlencoded`))
            .withEntity(body)
          response <- clientManaged.use(_.expect[String](request))
        } yield assert(response)(equalTo(""))
      }
    )

}

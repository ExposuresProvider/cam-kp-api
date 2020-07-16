package org.renci.cam

import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s._
import org.http4s.headers._
import org.http4s.implicits._
import org.renci.cam.domain._
import zio.Task
import zio.interop.catz._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

object QueryServiceTest extends DefaultRunnableSpec {

  def spec =
    suite("QueryServiceSpec")(
      test("testGetNodeTypes") {
        val n0Node = TranslatorQueryNode("n0", "gene", Some("NCBIGENE:558"))
        val n1Node = TranslatorQueryNode("n1", "biological_process", None)
        val e0Edge = TranslatorQueryEdge("e0", "n1", "n0", "has_participant")

        val queryGraph = TranslatorQueryGraph(List(n0Node, n1Node), List(e0Edge))

        val map = QueryService.getNodeTypes(queryGraph.nodes)
        map.foreach(a => printf("k: %s, v: %s%n", a._1, a._2))

        assert(map)(isNonEmpty)
      } @@ ignore,
      testM("query service") {

        val n0Node = TranslatorQueryNode("n0", "gene", None/*Some("NCBIGENE:558")*/)
        val n1Node = TranslatorQueryNode("n1", "biological_process", None)
        val e0Edge = TranslatorQueryEdge("e0", "n1", "n0", "has_participant")

        val queryGraph = TranslatorQueryGraph(List(n0Node, n1Node), List(e0Edge))
        val message = TranslatorMessage(None, Some(queryGraph), None)
        val requestBody = TranslatorQueryRequestBody(message)
        val encoded = requestBody.asJson.deepDropNullValues.noSpaces
        for {
          httpClient <- SPARQLQueryExecutor.makeHttpClient
          uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1)
          request = Request[Task](Method.POST, uri)
            .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
            .withEntity(encoded)
          response <- httpClient.use(_.expect[String](request))
          _ = println("response: " + response)
        } yield assert(response)(isNonEmptyString)
      } @@ ignore
    )

}

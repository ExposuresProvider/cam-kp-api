package org.renci.cam

import org.http4s._
import org.http4s.headers._
import org.http4s.implicits._
import zio.Task
import zio.interop.catz._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

object BlazegraphTest extends DefaultRunnableSpec {

  val suite1 = suite("BlazegraphTestSpec")(
    testM("bindings") {
      val query =
        """PREFIX bl: <https://w3id.org/biolink/vocab/>
              SELECT DISTINCT ?predicate WHERE { bl:has_participant <http://reasoner.renci.org/vocab/slot_mapping> ?predicate . }"""
      for {
        httpClient <- HttpClient.makeHttpClient
        uri =
          uri"http://152.54.9.207:9999/blazegraph/sparql"
            .withQueryParam("query", query)
            .withQueryParam("format", "json")
        request =
          Request[Task](Method.POST, uri).withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
        response <- httpClient.use(_.expect[String](request))
      } yield assert(response)(isNonEmptyString)
    } @@ ignore
  )

  def spec = suite("All tests")(suite1)

}

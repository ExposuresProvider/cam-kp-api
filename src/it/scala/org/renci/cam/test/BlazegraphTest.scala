package org.renci.cam.test

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
//      val query =
//        """PREFIX bl: <https://w3id.org/biolink/vocab/>
//              SELECT DISTINCT ?predicate WHERE { bl:has_participant <http://reasoner.renci.org/vocab/slot_mapping> ?predicate . }"""

      val query = """SELECT DISTINCT  ?qid ?kid ?biolinkSlot ?label
        WHERE
        { VALUES ( ?kid ?qid ) {
          ( <http://purl.obolibrary.org/obo/BFO_0000066> "e0000" )
          ( <http://purl.obolibrary.org/obo/RO_0002234> "e0001" )
          ( <http://purl.obolibrary.org/obo/BFO_0000050> "e0002" )
          ( <http://purl.obolibrary.org/obo/RO_0002233> "e0003" )
        }
          ?kid (<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>)+ ?biolinkSlot .
            ?biolinkSlot  a  <https://w3id.org/biolink/biolinkml/meta/types/SlotDefinition>
          OPTIONAL
          { ?kid  <http://www.w3.org/2000/01/rdf-schema#label>  ?label }
          FILTER NOT EXISTS { ?kid (<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>)+ ?other .
            ?other (<https://w3id.org/biolink/biolinkml/meta/is_a>)+/(<https://w3id.org/biolink/biolinkml/meta/mixins>)* ?biolinkSlot
          }
        }"""

      val uri = uri"https://stars-app.renci.org/cam/sparql"
        .withQueryParam("query", query)
        .withQueryParam("format", "json")
      val request =
        Request[Task](Method.POST, uri).withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
      for {
        response <- HttpClient.makeHttpClient.use(_.expect[String](request))
        _ = println(response)
      } yield assert(response)(isNonEmptyString)
    } @@ ignore
  )

  def spec = suite("All tests")(suite1)

}

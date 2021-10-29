package org.renci.cam.it

import org.http4s._
import org.http4s.headers._
import org.http4s.implicits._
import org.renci.cam._
import zio.Task
import zio.interop.catz._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.testEnvironment

object BlazegraphTest extends DefaultRunnableSpec {

  val testLayer = (testEnvironment ++ HttpClient.makeHttpClientLayer).mapError(TestFailure.die)

  val testBlazegraphServiceDirectly = suite("testBlazegraphServiceDirectly")(
    testM("test Blazegraph service directly") {
//      val query =
//        """PREFIX bl: <https://w3id.org/biolink/vocab/>
//              SELECT DISTINCT ?predicate WHERE { bl:has_participant <http://reasoner.renci.org/vocab/slot_mapping> ?predicate . }"""

//      val query = """SELECT DISTINCT  ?qid ?kid ?biolinkSlot ?label
//        WHERE
//        { VALUES ( ?kid ?qid ) {
//          ( <http://purl.obolibrary.org/obo/BFO_0000066> "e0000" )
//          ( <http://purl.obolibrary.org/obo/RO_0002234> "e0001" )
//          ( <http://purl.obolibrary.org/obo/BFO_0000050> "e0002" )
//          ( <http://purl.obolibrary.org/obo/RO_0002233> "e0003" )
//        }
//          ?kid (<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>)+ ?biolinkSlot .
//            ?biolinkSlot  a  <https://w3id.org/biolink/biolinkml/meta/types/SlotDefinition>
//          OPTIONAL
//          { ?kid  <http://www.w3.org/2000/01/rdf-schema#label>  ?label }
//          FILTER NOT EXISTS { ?kid (<http://www.w3.org/2000/01/rdf-schema#subPropertyOf>)+ ?other .
//            ?other (<https://w3id.org/biolink/biolinkml/meta/is_a>)+/(<https://w3id.org/biolink/biolinkml/meta/mixins>)* ?biolinkSlot
//          }
//        }"""

      val iriList = List(
        "<http://purl.obolibrary.org/obo/RO_0002233>",
        "<http://purl.obolibrary.org/obo/RO_0002333>",
        "<http://purl.obolibrary.org/obo/RO_0002296>",
        "<http://purl.obolibrary.org/obo/RO_0000057>",
        "<http://purl.obolibrary.org/obo/RO_0002298>",
        "<http://purl.obolibrary.org/obo/RO_0002234>",
        "<http://purl.obolibrary.org/obo/RO_0002299>",
        "<http://purl.obolibrary.org/obo/RO_0002590>",
        "<http://purl.obolibrary.org/obo/RO_0002588>",
        "<http://purl.obolibrary.org/obo/BFO_0000167>",
        "<http://purl.obolibrary.org/obo/RO_0002297>",
        "<http://purl.obolibrary.org/obo/RO_0004007>",
        "<http://purl.obolibrary.org/obo/RO_0004008>",
        "<http://purl.obolibrary.org/obo/RO_0004009>",
        "<http://purl.obolibrary.org/obo/RO_0004020>",
        "<http://purl.obolibrary.org/obo/RO_0004021>",
        "<http://purl.obolibrary.org/obo/OBI_0000293>",
        "<http://purl.obolibrary.org/obo/OBI_0000299>"
      )

      val iriListAsString = iriList.mkString(" ")
      val query = s"""SELECT DISTINCT  ?e0 ?n1 ?n0 ?n0_type ?n1_type
        WHERE
        { ?n0  <http://www.openrdf.org/schema/sesame#directType>  ?n0_type .
          ?n1  <http://www.openrdf.org/schema/sesame#directType>  ?n1_type
          VALUES ?e0 { $iriListAsString }
            ?n1  ?e0  ?n0 ;
            a    <https://w3id.org/biolink/vocab/BiologicalProcess> .
              ?n0  a    <https://w3id.org/biolink/vocab/Gene>
                }
                LIMIT   1"""

      for {
        httpClient <- HttpClient.client
        uri = uri"https://stars-app.renci.org/camdev/sparql"
          .withQueryParam("query", query)
          .withQueryParam("format", "json")
        request = Request[Task](Method.POST, uri).withHeaders(Accept(MediaType.application.json),
                                                              `Content-Type`(MediaType.application.json))
        response <- httpClient.expect[String](request)
        // _ = println(response)
      } yield assert(response)(isNonEmptyString)
    }
  )

  def spec = suite("Blazegraph tests")(testBlazegraphServiceDirectly).provideLayerShared(testLayer) @@ TestAspect.sequential

}

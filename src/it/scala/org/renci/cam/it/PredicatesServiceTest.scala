package org.renci.cam.it

import io.circe.{Decoder, Encoder, parser}
import org.http4s._
import org.http4s.headers.`Content-Type`
import org.http4s.implicits._
import org.renci.cam._
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate}
import zio.Task
import zio.blocking.Blocking
import zio.interop.catz._
import zio.test.Assertion._
import zio.test._

object PredicatesServiceTest extends DefaultRunnableSpec {

  val camkpapiTestLayer = Blocking.live >>> TestContainer.camkpapi
  val camkpapiLayer = HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer
  val testLayer = zio.test.environment.testEnvironment ++ camkpapiTestLayer ++ camkpapiLayer

  val predicatesServiceTest = suite("PredicatesService test")(
    testM("test parsing predicates and for the existence of IndividualOrganism") {
      val testCase = for {
        httpClient <- HttpClient.client
        biolinkData <- Biolink.biolinkData
        request = Request[Task](Method.GET, uri"http://127.0.0.1:8080/predicates").withHeaders(`Content-Type`(MediaType.application.json))
        response <- httpClient.expect[String](request)
      } yield {

        implicit val biolinkClassKeyDecoder = Implicits.biolinkClassKeyDecoder(biolinkData.classes)
        implicit val biolinkClassKeyEncoder = Implicits.biolinkClassKeyEncoder
        implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Implicits.biolinkPredicateEncoder
        implicit val biolinkPredicateDecoder: Decoder[BiolinkPredicate] = Implicits.biolinkPredicateDecoder(biolinkData.predicates)

        val parsed = parser.parse(response).toOption.get
        val map = parsed.as[Map[BiolinkClass, Map[BiolinkClass, List[BiolinkPredicate]]]]

        assert(map)(isRight) && assert(map.toOption.get.keys)(contains(BiolinkClass("IndividualOrganism")))
      }
      testCase.provideCustomLayer(testLayer)
    }
  )

  def spec = suite("PredicateService tests")(predicatesServiceTest)

}

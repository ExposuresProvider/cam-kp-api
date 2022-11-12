package org.renci.cam.it

import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe.{parser, Decoder, KeyDecoder, KeyEncoder}
import org.http4s._
import org.http4s.headers.{`Content-Type`, Accept}
import org.http4s.implicits._
import org.renci.cam._
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, MetaKnowledgeGraph}
import zio.Task
import zio.blocking.Blocking
import zio.interop.catz._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.testEnvironment

object MetaKnowledgeGraphServiceTest extends DefaultRunnableSpec with LazyLogging {

  val camkpapiTestLayer = (Blocking.live ++ Server.configLayer) >>> TestContainer.camkpapi
  val camkpapiLayer = HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer
  val testLayer = (testEnvironment ++ camkpapiTestLayer ++ camkpapiLayer).mapError(TestFailure.die)

  val metaKnowledgeGraphServiceTest = suite("MetaKnowledgeGraphService test")(
    testM("test parsing predicates and for the existence of IndividualOrganism") {
      for {
        httpClient <- HttpClient.client
        biolinkData <- Biolink.biolinkData
        request = Request[Task](Method.GET, uri"http://127.0.0.1:8080/meta_knowledge_graph")
          .withHeaders(Accept(MediaType.application.json))
        response <- httpClient.expect[String](request)
      } yield {
        import biolinkData.implicits._
        val parsed = parser.parse(response).toOption.get
//        logger.info("parsed: {}", parsed)
        val mkg = parsed.as[MetaKnowledgeGraph]
//        logger.info("mkg: {}", mkg)
        assert(mkg)(isRight) && assert(mkg.toOption.get.edges.map(a => a.subject))(contains(BiolinkClass("IndividualOrganism")))
      }
    }
  )

  def spec = suite("MetaKnowledgeGraphService tests")(metaKnowledgeGraphServiceTest).provideLayerShared(testLayer)

}

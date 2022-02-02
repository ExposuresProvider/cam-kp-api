package org.renci.cam.test

import org.http4s.Uri
import org.http4s.implicits._
import org.renci.cam.{AppConfig, HttpClient}
import zio.ZIO
import zio.ZIO.debug
import zio.config.getConfig
import zio.interop.catz._
import zio.test.Assertion.containsString
import zio.test._
import zio.test.environment.testEnvironment

object SwaggerTest extends DefaultRunnableSpec {
  val appConfig = getConfig[AppConfig]

  val testLayer = (testEnvironment ++ HttpClient.makeHttpClientLayer).mapError(TestFailure.die)

  def httpGetAsString(uri: Uri): ZIO[HttpClient.HttpClient, Throwable, String] = for {
    httpClient <- HttpClient.client
    response <- httpClient.expect[String](uri)
  } yield response

  val testSwaggerEndpoints = suite("Test the Swagger endpoints")(
    testM("Test whether we can access the Swagger endpoint") {
      val response = httpGetAsString(uri"http://127.0.0.1:8080/docs/")

      assertM(response)(containsString("help"))
    }
  )

  def spec = suite("Swagger endpoint tests")(
    testSwaggerEndpoints
  ).provideLayerShared(testLayer) @@ TestAspect.sequential
}

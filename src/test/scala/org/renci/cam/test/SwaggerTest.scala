package org.renci.cam.test

import org.http4s.LiteralSyntaxMacros.uri
import org.http4s.{Method, Request, Uri}
import org.http4s.implicits._
import org.renci.cam.HttpClient.{HttpClient, makeHttpClient}
import org.renci.cam.{AppConfig, HttpClient, QueryService, Server}
import zio.{RIO, ZIO}
import zio.ZIO.debug
import zio.config.getConfig
import zio.interop.catz._
import zio.test.Assertion.{containsString, forall}
import zio.test._
import org.http4s.dsl.io._
import org.renci.cam.Server.EndpointEnv
import zio.test.environment.testEnvironment

import java.net.URI

object SwaggerTest extends DefaultRunnableSpec {
  val appConfig = getConfig[AppConfig]

  val testLayer = (testEnvironment ++ HttpClient.makeHttpClientLayer).mapError(TestFailure.die)

  val testSwaggerEndpoints = suite("Test the Swagger endpoints")(
    testM("Test whether we can access the Swagger endpoint") {
      // val q = QueryService.run(20, false, )
      val r = makeHttpClient.asService
    }
  )

  def spec = suite("Swagger endpoint tests")(
    testSwaggerEndpoints
  ).provideLayerShared(testLayer) @@ TestAspect.sequential
}

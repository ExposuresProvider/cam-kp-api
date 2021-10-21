package org.renci.cam

import cats.effect.std.Dispatcher
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.client.Client
import zio._
import zio.interop.catz._
import zio.interop.catz.implicits._

import scala.concurrent.duration.{Duration, MINUTES}

object HttpClient {

  type HttpClient = Has[Client[Task]]

  def makeHttpClient: TaskManaged[Client[Task]] =
    Dispatcher.apply[Task].toManagedZIO.flatMap { implicit dispatcher =>
      BlazeClientBuilder[Task](zio.Runtime.default.platform.executor.asEC)
        .withConnectTimeout(Duration(5, MINUTES))
        .withIdleTimeout(Duration(4, MINUTES))
        .withRequestTimeout(3, MINUTES)
        .withResponseHeaderTimeout(2, MINUTES)
        .withMaxWaitQueueLimit(512)
        .resource
        .toManaged
    }

  def makeHttpClientLayer: TaskLayer[HttpClient] = makeHttpClient.toLayer

  val client: ZIO[HttpClient, Nothing, Client[Task]] = ZIO.service

}

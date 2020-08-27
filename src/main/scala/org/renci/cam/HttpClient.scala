package org.renci.cam

import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import zio._
import zio.interop.catz._

import scala.concurrent.duration.{Duration, MINUTES}

object HttpClient {

  type HttpClient = Has[Client[Task]]

  def makeHttpClient: UIO[TaskManaged[Client[Task]]] =
    ZIO.runtime[Any].map { implicit rts =>
      BlazeClientBuilder[Task](rts.platform.executor.asEC).withConnectTimeout(Duration(3, MINUTES)).resource.toManaged
    }

  def makeHttpClientLayer: UIO[TaskLayer[HttpClient]] = makeHttpClient.map(ZLayer.fromManaged)

  val client: ZIO[HttpClient, Nothing, Client[Task]] = ZIO.service

}

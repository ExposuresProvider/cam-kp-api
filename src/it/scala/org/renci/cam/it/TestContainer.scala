package org.renci.cam.it

import com.dimafeng.testcontainers.FixedHostPortGenericContainer
import org.testcontainers.containers.wait.strategy.Wait
import zio.blocking._
import zio.{Has, ZLayer, ZManaged}

object TestContainer {

  type CAMKPAPI = Has[FixedHostPortGenericContainer]

  def camkpapi: ZLayer[Blocking, Nothing, CAMKPAPI] =
    ZManaged.make {
      effectBlocking {
        val container = FixedHostPortGenericContainer("renciorg/cam-kp-api:0.1",
                                                      exposedHostPort = 8080,
                                                      exposedContainerPort = 8080,
                                                      waitStrategy = Wait.forHttp("/predicates"))
        container.start()
        container
      }.orDie
    }(container => effectBlocking(container.stop()).orDie).toLayer

}

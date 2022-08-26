package org.renci.cam.it

import com.dimafeng.testcontainers.FixedHostPortGenericContainer
import org.renci.cam.AppConfig
import org.testcontainers.containers.wait.strategy.Wait
import zio.blocking._
import zio._
import zio.config.ZConfig

object TestContainer {

  type CAMKPAPI = Has[FixedHostPortGenericContainer]

  def camkpapi: ZLayer[Blocking with ZConfig[AppConfig], Nothing, CAMKPAPI] =
    ZManaged.make {
      zio.config
        .getConfig[AppConfig]
        .flatMap { config =>
          effectBlocking {
            val container = FixedHostPortGenericContainer(s"renciorg/cam-kp-api:${config.version}",
                                                          exposedHostPort = 8080,
                                                          exposedContainerPort = 8080,
                                                          waitStrategy = Wait.forHttp("/meta_knowledge_graph"))
            container.start()
            container
          }
        }
        .orDie
    }(container => effectBlocking(container.stop()).orDie).toLayer

}

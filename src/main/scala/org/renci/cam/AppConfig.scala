package org.renci.cam

import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor.descriptor

final case class AppConfig(host: String, port: Int, `sparql-host`: String, `sparql-port`: Int)

object AppConfig {

  val config: ConfigDescriptor[AppConfig] = descriptor[AppConfig]

}

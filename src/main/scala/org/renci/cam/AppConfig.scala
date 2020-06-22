package org.renci.cam

import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor.descriptor

final case class AppConfig(host: String, port: Int, `sparql-endpoint`: String)

object AppConfig {

  val config: ConfigDescriptor[AppConfig] = descriptor[AppConfig]

}

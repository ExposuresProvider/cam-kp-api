package org.renci.cam

import org.http4s.Uri
import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor.{Descriptor, descriptor, _}

final case class AppConfig(host: String, port: Int, location: String, sparqlEndpoint: String, trapiVersion: String, maturity: String)

object AppConfig {

  val config: ConfigDescriptor[AppConfig] = descriptor[AppConfig].mapKey(toKebabCase)

}

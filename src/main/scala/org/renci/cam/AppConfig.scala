package org.renci.cam

import org.http4s.Uri
import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor.{descriptor, Descriptor, _}

final case class AppConfig(host: String, port: Int, location: String, sparqlEndpoint: Uri, trapiVersion: String, maturity: String)

object AppConfig {

  implicit val uriDescriptor: Descriptor[Uri] =
    Descriptor[String].transformEitherLeft(Uri.fromString)(_.toString)(_.getMessage)

  val config: ConfigDescriptor[AppConfig] = descriptor[AppConfig].mapKey(toKebabCase)

}

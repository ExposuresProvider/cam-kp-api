package org.renci.cam

import org.http4s.Uri
import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor.{Descriptor, descriptor, _}

final case class AppConfig(host: String, port: Int, sparqlEndpoint: Uri)

object AppConfig {

  implicit val uriDescriptor: Descriptor[Uri] =
    Descriptor[String].xmapEitherELeftPartial(Uri.fromString)(_.toString)( _.getMessage)

  val config: ConfigDescriptor[AppConfig] = descriptor[AppConfig].mapKey(camelToKebab)

}

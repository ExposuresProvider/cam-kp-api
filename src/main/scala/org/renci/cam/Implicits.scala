package org.renci.cam

import io.circe.Decoder.Result
import io.circe.{Decoder, Encoder, HCursor}
import org.apache.commons.lang3.StringUtils
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, IRI}

object Implicits {

  def iriDecoder(prefixesMap: Map[String, String]): Decoder[IRI] = new Decoder[IRI] {

    override def apply(c: HCursor): Result[IRI] = for {
      value <- c.value.as[String]
      ret =
        prefixesMap
          .filter(entry => value.startsWith(entry._1))
          .map { entry =>
            val remainder = value.replaceAll(s"${entry._1}:", "")
            s"${entry._2}$remainder"
          }
          .headOption
          .orElse(
            prefixesMap
              .filter(entry => value.startsWith(entry._2))
              .map { entry =>
                val remainder = value.substring(entry._2.length, value.length).replaceAll(s"${entry._1}:", "")
                s"${entry._2}$remainder"
              }
              .headOption
          ).getOrElse(throw new Exception(s"Could not parse IRI: $value"))
    } yield IRI(ret)

  }

  def iriEncoderOut(prefixesMap: Map[String, String]): Encoder[IRI] = Encoder.encodeString.contramap { iri =>
    val startsWith = prefixesMap.filter { case (_, namespace) => iri.value.startsWith(namespace) }
    if (startsWith.nonEmpty) {
      val (prefix, namespace) = startsWith.maxBy(_._2.length)
      StringUtils.prependIfMissing(iri.value.drop(namespace.length), s"$prefix:")
    } else iri.value
  }

  def iriEncoderIn(prefixesMap: Map[String, String]): Encoder[IRI] = Encoder.encodeString.contramap { iri =>
    val startsWith = prefixesMap.filter { case (prefix, namespace) => iri.value.startsWith(namespace) }
    if (startsWith.nonEmpty) {
      val (_, namespace) = startsWith.maxBy(_._2.length)
      s"$namespace${iri.value.drop(namespace.length)}"
    } else iri.value
  }

  def biolinkPredicateDecoder(biolinkPredicates: List[BiolinkPredicate]): Decoder[BiolinkPredicate] = Decoder.decodeString.emap { s =>
    biolinkPredicates.find(a => a.shorthand == s).toRight(s"BiolinkPredicate does not exist: $s")
  }

  def biolinkClassDecoder(biolinkClasses: List[BiolinkClass]): Decoder[BiolinkClass] = Decoder.decodeString.emap { s =>
    biolinkClasses.find(a => a.shorthand == s).toRight(s"BiolinkClass does not exist: $s")
  }

}

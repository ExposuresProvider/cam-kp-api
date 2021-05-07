package org.renci.cam

import com.google.common.base.CaseFormat
import io.circe.Decoder.Result
import io.circe._
import org.apache.commons.lang3.StringUtils
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, IRI}

object Implicits {

  private val Curie = "^([^:]*):(.*)$".r
  private val protocols = Set("http", "https", "ftp", "file", "mailto")
  private val BiolinkNamespace = "https://w3id.org/biolink/vocab/"

  def iriDecoder(prefixesMap: Map[String, String]): Decoder[IRI] = new Decoder[IRI] {

    override def apply(c: HCursor): Result[IRI] = for {
      value <- c.value.as[String]
      iri <- expandCURIEString(value, prefixesMap)
    } yield iri

  }

  def iriKeyDecoder(prefixesMap: Map[String, String]): KeyDecoder[IRI] = new KeyDecoder[IRI] {
    override def apply(key: String): Option[IRI] = expandCURIEString(key, prefixesMap).toOption
  }

  def expandCURIEString(curieString: String, prefixesMap: Map[String, String]): Either[DecodingFailure, IRI] =
    for {
      curie <- curieString match {
        case Curie(p, l) => Right((p, l))
        case _           => Left(DecodingFailure(s"CURIE is malformed: $curieString", Nil))
      }
      (prefix, local) = curie
      namespace <-
        if (protocols(prefix)) Right(s"$prefix:")
        else prefixesMap.get(prefix).toRight(DecodingFailure(s"No prefix expansion found for $prefix:$local", Nil))
    } yield IRI(s"$namespace$local")

  def iriEncoder(prefixesMap: Map[String, String]): Encoder[IRI] = Encoder.encodeString.contramap { iri =>
    compactIRIIfPossible(iri, prefixesMap)
  }

  def iriKeyEncoder(prefixesMap: Map[String, String]): KeyEncoder[IRI] = KeyEncoder.encodeKeyString.contramap { iri: IRI =>
    compactIRIIfPossible(iri, prefixesMap)
  }

  def compactIRIIfPossible(iri: IRI, prefixesMap: Map[String, String]): String = {
    val startsWith = prefixesMap.filter { case (_, namespace) => iri.value.startsWith(namespace) }
    if (startsWith.nonEmpty) {
      val (prefix, namespace) = startsWith.maxBy(_._2.length)
      StringUtils.prependIfMissing(iri.value.drop(namespace.length), s"$prefix:")
    } else {
      val httpsIRI = toHttps(iri.value)
      val httpsStartsWith = prefixesMap.filter { case (_, namespace) => httpsIRI.startsWith(namespace) }
      if (httpsStartsWith.nonEmpty) {
        val (prefix, namespace) = httpsStartsWith.maxBy(_._2.length)
        StringUtils.prependIfMissing(httpsIRI.drop(namespace.length), s"$prefix:")
      } else iri.value
    }
  }

  private def toHttps(uri: String): String = uri.replaceFirst("http:", "https:")

  def biolinkPredicateDecoder(biolinkPredicates: List[BiolinkPredicate]): Decoder[BiolinkPredicate] = Decoder.decodeString.map { s =>
    val localName = s.replace("biolink:", "")
    biolinkPredicates.find(a => a.iri.value.replace(BiolinkNamespace, "") == localName) match {
      case Some(pred) => pred
      case None       => BiolinkPredicate(s"$BiolinkNamespace$localName")
    }
  }

  def biolinkPredicateEncoder(prefixesMap: Map[String, String]): Encoder[BiolinkPredicate] = Encoder.encodeString.contramap { s =>
    compactIRIIfPossible(s.iri, prefixesMap)
  }

  def biolinkClassKeyEncoder: KeyEncoder[BiolinkClass] = KeyEncoder.encodeKeyString.contramap { bc: BiolinkClass =>
    bc.withBiolinkPrefix
  }

  def predicateOrPredicateListDecoder(biolinkPredicates: List[BiolinkPredicate]): Decoder[List[BiolinkPredicate]] =
    new Decoder[List[BiolinkPredicate]]() {
      implicit val biolinkPredicateDecoder: Decoder[BiolinkPredicate] = Implicits.biolinkPredicateDecoder(biolinkPredicates)

      override def apply(c: HCursor): Result[List[BiolinkPredicate]] =
        for {
          ret <- c.as[List[BiolinkPredicate]].orElse(c.as[BiolinkPredicate].map(_ :: Nil))
        } yield ret
    }

  def biolinkClassKeyDecoder(biolinkClasses: List[BiolinkClass]): KeyDecoder[BiolinkClass] = new KeyDecoder[BiolinkClass] {

    override def apply(key: String): Option[BiolinkClass] = {
      val localName = key.replace("biolink:", "")
      biolinkClasses.find(a => a.iri.value.replace(BiolinkNamespace, "") == localName).toRight(s"BiolinkClass does not exist: $key")
    }.toOption

  }

  def biolinkClassDecoder(biolinkClasses: List[BiolinkClass]): Decoder[BiolinkClass] = Decoder.decodeString.emap { s =>
    val localName = s.replace("biolink:", "")
    biolinkClasses.find(a => a.iri.value.replace(BiolinkNamespace, "") == localName).toRight(s"BiolinkClass does not exist: $s")
  }

  def biolinkClassEncoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap { s =>
    if (s.shorthand.contains("_")) {
      s"biolink:${CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, s.shorthand)}"
    } else {
      s"biolink:${StringUtils.capitalize(s.shorthand)}"
    }
  }

}

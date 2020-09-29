package org.renci.cam

import java.nio.charset.StandardCharsets

import com.typesafe.scalalogging.LazyLogging
import io.circe.parser.parse
import io.circe.{Decoder, Encoder, Json}
import org.apache.commons.io.IOUtils
import org.apache.commons.lang3.StringUtils
import org.renci.cam.domain.CURIEorIRI

import scala.util.Try

object Implicits extends LazyLogging {

  // FIXME should be getting this from the ZIO service in Utilities
  def readPrefixes: Map[String, String] = {
    val legacyPrefixes = IOUtils.resourceToString("legacy_prefixes.json", StandardCharsets.UTF_8, this.getClass.getClassLoader)
    val legacyJSON = parse(legacyPrefixes).getOrElse(Json.Null)
    val legacyMap = legacyJSON.as[Map[String, String]].getOrElse(Map.empty)
    val currentPrefixes = IOUtils.resourceToString("prefixes.json", StandardCharsets.UTF_8, this.getClass.getClassLoader)
    val currentJson = parse(currentPrefixes).getOrElse(Json.Null)
    val currentMap = currentJson.as[Map[String, String]].getOrElse(Map.empty)
    currentMap ++ legacyMap
  }

  implicit def inEncodeCURIEorIRI: Encoder[CURIEorIRI] = Encoder.encodeString.contramap[CURIEorIRI] { a =>
    a.prefix match {
      case Some(p) =>
        readPrefixes
          .filter(entry => entry._1.equalsIgnoreCase(p))
          .map(entry => s"${entry._2}${a.reference}")
          .headOption
          .getOrElse(s"$p:${a.reference}")
      case None =>
        readPrefixes
          .filter(entry => entry._1.equalsIgnoreCase("bl"))
          .map(entry => s"${entry._2}${a.reference}")
          .headOption
          .getOrElse(s"bl:${a.reference}")
    }
  }

  implicit def outEncodeCURIEorIRI: Encoder[CURIEorIRI] = Encoder.encodeString.contramap[CURIEorIRI] { a =>
    a.prefix match {
      case Some(p) =>
        readPrefixes
          .filter(entry => entry._1.equalsIgnoreCase(p))
          .map(entry => StringUtils.prependIfMissing(s"${a.reference.replace(" ", "_")}", s"${entry._1}:"))
          .headOption
          .getOrElse(s"$p:${a.reference}")
      case None =>
        readPrefixes
          .filter(entry => entry._1.equalsIgnoreCase("bl"))
          .map(entry => StringUtils.prependIfMissing(s"${a.reference.replace(" ", "_")}", s"${entry._1}:"))
          .headOption
          .getOrElse(s"bl:${a.reference}")
    }
  }

  implicit def decodeCURIEorIRI: Decoder[CURIEorIRI] = Decoder.decodeString.emapTry(value =>
    Try(
      {
        if (value.contains(":")) {
          if (value.startsWith("http")) {
            //it's an IRI
            readPrefixes
              .filter(entry => value.startsWith(entry._2))
              .map(entry => CURIEorIRI(Some(entry._1), value.substring(entry._2.length, value.length)))
              .head
          } else {
            //it's an CURIE
            val split = value.split(":")
            readPrefixes
              .filter(entry => split(0).equalsIgnoreCase(entry._1))
              .map(entry => CURIEorIRI(Some(entry._1), split(1)))
              .head
          }
        } else {
          CURIEorIRI(None, value)
        }
      }
    ))

}

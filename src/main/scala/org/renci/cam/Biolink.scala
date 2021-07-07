package org.renci.cam

import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.generic.auto._
import org.apache.commons.lang3.StringUtils
import org.http4s.headers.Accept
import org.http4s.implicits._
import org.http4s.{MediaType, Method, Request}
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate}
import zio._
import zio.blocking.{effectBlockingIO, Blocking}
import zio.interop.catz._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.io.Source

object Biolink extends LazyLogging {

  final case class BiolinkData(version: String,
                               prefixes: Map[String, String],
                               classes: List[BiolinkClass],
                               predicates: List[BiolinkPredicate])

  def makeUtilitiesLayer: ZLayer[HttpClient, Throwable, Has[BiolinkData]] = getBiolinkData.toLayer

  val biolinkData: URIO[Has[BiolinkData], BiolinkData] = ZIO.service

  def getBiolinkData: ZIO[HttpClient, Throwable, BiolinkData] = {
    val sourceManaged = for {
      fileStream <- Managed.fromAutoCloseable(Task.effect(getClass.getClassLoader.getResourceAsStream("biolink-data.json")))
      source <- Managed.fromAutoCloseable(Task.effect(Source.fromInputStream(fileStream)))
    } yield source

    implicit val biolinkClassDecoder: Decoder[BiolinkClass] = Decoder.decodeString.map(s => BiolinkClass(s))
    implicit val biolinkPredicateDecoder: Decoder[BiolinkPredicate] = Decoder.decodeString.map(s => BiolinkPredicate(s))

    for {
      biolinkDataString <- sourceManaged.use(source => ZIO.effect(source.getLines().mkString))
      biolinkDataJson <- Task.effect(io.circe.parser.parse(biolinkDataString).getOrElse(Json.Null))
      biolinkData <- ZIO.fromEither(biolinkDataJson.as[BiolinkData])
    } yield biolinkData
  }

}

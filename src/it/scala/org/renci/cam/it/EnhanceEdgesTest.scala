package org.renci.cam.it

import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import io.circe.parser._
import io.circe.syntax.{EncoderOps, _}
import org.http4s._
import org.http4s.circe.jsonDecoder
import org.http4s.headers.{Accept, `Content-Type`}
import org.http4s.implicits._
import org.renci.cam.Biolink.biolinkData
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam.domain.{BiolinkClass, BiolinkPredicate, IRI, MetaKnowledgeGraph, TRAPIAttribute, TRAPIMessage, TRAPIQuery, TRAPIQueryEdge, TRAPIQueryGraph, TRAPIQueryNode, TRAPIResponse}
import org.renci.cam.{AppConfig, Biolink, HttpClient, Implicits}
import zio.blocking.Blocking
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.interop.catz._
import zio.test.Assertion._
import zio.test._
import zio.{Layer, Task, ZIO}

import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import scala.jdk.CollectionConverters._

object EnhanceEdgesTest extends DefaultRunnableSpec with LazyLogging {
  /* Set up test environment */
  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val camkpapiLayer = Blocking.live >>> HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer
  val testLayer = (configLayer ++ camkpapiLayer).mapError(TestFailure.die)

  /* Configure tests */
  val exampleDir: Path = Paths.get("src/it/resources/enhance-edge-tests")
  val exampleResultsDir: Path = Paths.get("src/it/resources/example-results")

  val endpointToTest: Uri =
    sys.env.get("CAM_KP_ENDPOINT") match {
      case None      => uri"https://cam-kp-api.renci.org/1.3.0/query"
      case Some(str) => Uri.fromString(str).toOption.get
    }

  val endpointMetaKG: Uri = Uri.fromString(endpointToTest.toString.replaceFirst("/query$", "/meta_knowledge_graph")).toOption.get

  val endpointNodeNorm: Uri =
    sys.env.get("NODE_NORM_ENDPOINT") match {
      case None      => uri"https://nodenormalization-sri.renci.org/1.3/get_normalized_nodes"
      case Some(str) => Uri.fromString(str).toOption.get
    }

  val limit: Int = sys.env.getOrElse("CAM_KP_LIMIT", 1000).toString.toInt

  /* We will need the MetaKnowledgeGraph to normalize input nodes, so we load that here. */
  val zioRuntime = zio.Runtime.unsafeFromLayer(camkpapiLayer)
  val metaKG: MetaKnowledgeGraph = {
    logger.info("Retrieving MetaKnowledgeGraph")

    // TODO: replace with ZIO I guess.
    val metaKGSource = Source.fromURL(endpointMetaKG.toString())
    val metaKGString = metaKGSource.mkString
    metaKGSource.close()

    zioRuntime
      .unsafeRun(
        for {
          biolinkData <- biolinkData

          metaKG <- ZIO.fromEither(
            {
              implicit val decoderIRI: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
              implicit val keyDecoderIRI: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
              implicit val iriKeyDecoder: KeyDecoder[BiolinkClass] = Implicits.biolinkClassKeyDecoder(biolinkData.classes)
              implicit val decoderBiolinkClass: Decoder[BiolinkClass] = Implicits.biolinkClassDecoder(biolinkData.classes)
              implicit val decoderBiolinkPredicate: Decoder[BiolinkPredicate] =
                Implicits.biolinkPredicateDecoder(biolinkData.predicates)
              implicit lazy val decoderTRAPIAttribute: Decoder[TRAPIAttribute] = deriveDecoder[TRAPIAttribute]
              implicit lazy val decoderMetaKnowledgeGraph: Decoder[MetaKnowledgeGraph] = deriveDecoder[MetaKnowledgeGraph]

              decode[MetaKnowledgeGraph](metaKGString)
            }
          )
        } yield {
          metaKG
        }
      )
  }

  val acceptableIDs: Set[String] = metaKG.nodes.values.flatMap(_.id_prefixes).toSet

  def getCURIEPrefix(curie: String): String = {
    val cs = curie.split(':')
    if (cs.length == 0) curie
    else cs(0)
  }

  def getAcceptableCURIE(curie: String): Option[String] = {
    val prefix = getCURIEPrefix(curie)

    if (acceptableIDs.contains(prefix)) {
      logger.info(s"getAcceptableCURIE(${curie}): prefix ${prefix} acceptable")
      Some(curie)
    } else {
      logger.info(s"getAcceptableCURIE(${curie}): prefix ${prefix} not acceptable")

      case class NodeNormIdentifier(
        identifier: String,
        label: Option[String]
      )

      case class NodeNormResponse(
        id: Option[NodeNormIdentifier],
        equivalent_identifiers: List[NodeNormIdentifier],
        `type`: List[BiolinkClass],
        information_content: Option[Float]
      )

      // TODO: move to ZIO (I guess).
      val nodeNormSource = Source.fromURL(endpointNodeNorm.+?("curie", curie).toString())
      val nodeNormString = nodeNormSource.mkString
      nodeNormSource.close()

      val result = zioRuntime.unsafeRun(
          for {
            biolinkData <- biolinkData

            response <- ZIO.fromEither(decode[Json](nodeNormString))
            curieResult = (response \\ curie)
            result <- ZIO.fromOption(curieResult.headOption)
            nnResponse <- ZIO.fromEither(
              {
                implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
                implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
                implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder
                implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] =
                  Implicits.biolinkPredicateEncoder(biolinkData.prefixes)

                implicit val decoderIRI: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
                implicit val keyDecoderIRI: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
                implicit val decoderBiolinkClass: Decoder[BiolinkClass] = Implicits.biolinkClassDecoder(biolinkData.classes)
                implicit val decoderBiolinkPredicate: Decoder[BiolinkPredicate] =
                  Implicits.biolinkPredicateDecoder(biolinkData.predicates)
                implicit lazy val decoderTRAPIAttribute: Decoder[TRAPIAttribute] = deriveDecoder[TRAPIAttribute]

                result.as[NodeNormResponse]
              }
            )
          } yield {
            if (curieResult.isEmpty) {
              logger.warn(s"NodeNorm could not resolve ${curie}.")
              None
            } else nnResponse.equivalent_identifiers
              .find(id => {
                logger.info(s"Checking to see if ${id.identifier} (prefix: ${getCURIEPrefix(id.identifier)}) is in ${acceptableIDs}")
                acceptableIDs.contains(getCURIEPrefix(id.identifier))
              })
              .map(_.identifier)
          }
        )

      logger.info(s"getAcceptableCURIE(${curie}): replaced with ${result}")
      result
    }
  }

  def testEdge(subj: String,
               pred: String,
               obj: String): Spec[ZConfig[Biolink.BiolinkData] with HttpClient, TestFailure[Throwable], TestSuccess] =
    testM(s"Testing ${subj} ${pred} ${obj}") {
      for {
        biolinkData <- biolinkData
        httpClient <- HttpClient.client

        messageText: String = {
          implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
          implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
          implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder
          implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] =
            Implicits.biolinkPredicateEncoder(biolinkData.prefixes)

          val subjIRI = IRI(getAcceptableCURIE(subj).getOrElse(subj))
          val objIRI = IRI(getAcceptableCURIE(obj).getOrElse(obj))

          logger.info(s"Querying CAM-KP-API with subj=${subjIRI}, objIRI=${objIRI}")

          val queryGraph = TRAPIQueryGraph(
            nodes = Map(
              "n0" -> TRAPIQueryNode(ids = Some(List(subjIRI)), None, None, None),
              "n1" -> TRAPIQueryNode(ids = Some(List(objIRI)), None, None, None)
            ),
            edges = Map(
              "e0" -> TRAPIQueryEdge(
                predicates = Some(List(BiolinkPredicate("related_to"))),
                subject = "n0",
                `object` = "n1"
              ))
          )
          val message = TRAPIMessage(Some(queryGraph), None, None)

          TRAPIQuery(message = message, log_level = None).asJson.deepDropNullValues.noSpaces
        }

        request = Request[Task](Method.POST, endpointToTest.withQueryParam("limit", limit.toString))
          .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
          .withEntity(messageText)
        response <- httpClient.expect[Json](request)
        trapiResponse <- ZIO.fromEither(
          {
            implicit val decoderIRI: Decoder[IRI] = Implicits.iriDecoder(biolinkData.prefixes)
            implicit val keyDecoderIRI: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
            implicit val decoderBiolinkClass: Decoder[BiolinkClass] = Implicits.biolinkClassDecoder(biolinkData.classes)
            implicit val decoderBiolinkPredicate: Decoder[BiolinkPredicate] =
              Implicits.biolinkPredicateDecoder(biolinkData.predicates)
            implicit lazy val decoderTRAPIAttribute: Decoder[TRAPIAttribute] = deriveDecoder[TRAPIAttribute]

            response.as[TRAPIResponse]
          }
        )
        results = trapiResponse.message.results
        resultsOrEmpty = results.getOrElse(Seq())
        _ = logger.info(s"Querying CAM-KP-API with query ${messageText} results in ${resultsOrEmpty.size} results: ${resultsOrEmpty}")
      } yield assert(results)(Assertion.isSome(Assertion.isNonEmpty))
    }

  // Let's test the acceptable CURIE lookup system.
  val testCURIEs = suite("Test CURIEs to ensure we can convert them into acceptable IDs") {
    val curiesToTest = Map(
      "CHEMBL.COMPOUND:CHEMBL1201129" -> "CHEBI:50131",
      "UniProtKB:P56856" -> "UniProtKB:P56856"
    )

    curiesToTest.map({ case (id, acceptableId) =>
      test(s"Test whether ${id} can be converted into acceptable ID ${acceptableId}") {
        assert(getAcceptableCURIE(id))(Assertion.isSome(Assertion.equalTo(acceptableId)))
      }
    }).reduce(_ + _)
  }

  val testEachExampleFile = {
    // List of example files to process.
    val exampleFiles = Files
      .walk(exampleDir)
      .iterator()
      .asScala
      .filter(Files.isRegularFile(_))
      .filter(_.toString.toLowerCase.endsWith(".tsv"))
      .toSeq

    suite("Test example files in the src/it/resources/examples directory") {
      exampleFiles.map(exampleFile =>
        suite(s"Testing ${exampleDir.relativize(exampleFile)}") {
          val outputFile = exampleDir.resolve(exampleFile.getFileName.toString.replaceFirst(".tsv$", ".txt"))

          val exampleText = {
            val source = Source.fromFile(exampleFile.toFile)
            source.getLines()
          }

          val tests = for {
            // Read the example JSON file.
            line <- exampleText
            cols = line.split("\t")
          } yield {
            if (line.trim.isEmpty) None
            else Some(testEdge(cols(0), cols(1), cols(2)))
          }

          tests.flatten.reduce(_ + _)
        }
      ).reduce(_ + _)
    }
  }

  def spec: Spec[environment.TestEnvironment, TestFailure[Throwable], TestSuccess] = suite("EnhanceEdgesTest")(
    testCURIEs,
    testEachExampleFile
  ).provideCustomLayer(testLayer)
}

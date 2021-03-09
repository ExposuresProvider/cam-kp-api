package org.renci.cam.it

import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.{KeyDecoder, KeyEncoder}
import org.renci.cam.domain._
import org.renci.cam.{AppConfig, Biolink, HttpClient, Implicits}
import zio.ZIO
import zio.config.typesafe.TypesafeConfig
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test.{testM, _}

object ImplicitsTest extends DefaultRunnableSpec with LazyLogging {

  val testLayer = HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer >+> TypesafeConfig.fromDefaultLoader(AppConfig.config)

  val testIRIEncoder = suite("testIRIEncoder")(
    testM("test Implicits.iriEncoder") {
      val iri = IRI("http://identifiers.org/ncbigene/558")
      val testCase = for {
        biolinkData <- Biolink.biolinkData
        json = iri.asJson(Implicits.iriEncoder(biolinkData.prefixes)).deepDropNullValues.noSpaces.replace("\"", "")
      } yield assert(json)(equalTo("NCBIGENE:558"))
      testCase.provideCustomLayer(testLayer)
    }
  )

  val testCompactIRIIfPossible = suite("testCompactIRIIfPossible")(
    testM("test Implicits.compactIRIIfPossible with known prefix") {
      val testCase = for {
        biolinkData <- Biolink.biolinkData
        json = Implicits.compactIRIIfPossible(IRI("http://identifiers.org/ncbigene/558"), biolinkData.prefixes)
      } yield assert(json)(equalTo("NCBIGENE:558"))
      testCase.provideCustomLayer(testLayer)
    },
    testM("test Implicits.compactIRIIfPossible with unknown prefix") {
      val testCase = for {
        biolinkData <- Biolink.biolinkData
        json = Implicits.compactIRIIfPossible(IRI("http://asdfasdf/gene/558"), biolinkData.prefixes)
      } yield assert(json)(equalTo("http://asdfasdf/gene/558"))
      testCase.provideCustomLayer(testLayer)
    },
    testM("test Implicits.compactIRIIfPossible with already compacted iri") {
      val testCase = for {
        biolinkData <- Biolink.biolinkData
        json = Implicits.compactIRIIfPossible(IRI("NCBIGENE:558"), biolinkData.prefixes)
      } yield assert(json)(equalTo("NCBIGENE:558"))
      testCase.provideCustomLayer(testLayer)
    }
  )

  val testExpandCURIEString = suite("testExpandCURIEString")(
    testM("test Implicits.expandCURIEString") {
      val testCase = for {
        biolinkData <- Biolink.biolinkData
        json = Implicits.expandCURIEString("NCBIGENE:558", biolinkData.prefixes).toOption.get
      } yield assert(json.value)(equalTo("http://identifiers.org/ncbigene/558"))
      testCase.provideCustomLayer(testLayer)
    },
    testM("test Implicits.expandCURIEString malformed") {
      val testCase = for {
        biolinkData <- Biolink.biolinkData
        results <- ZIO.fromEither(Implicits.expandCURIEString("asdf:558", biolinkData.prefixes))
      } yield assert(results)(equalTo(IRI("asdf:558")))
      testCase.provideCustomLayer(testLayer)
    } @@ failing
  )

  val testBiolinkPredicateDecoder = suite("testBiolinkPredicateDecoder")(
    testM("test Implicits.biolinkPredicateDecoder") {
      val testCase = for {
        biolinkData <- Biolink.biolinkData
      } yield {
        val predicate = BiolinkPredicate("related_to")
        val predicateJson = predicate.asJson(Implicits.biolinkPredicateEncoder)
        val decoded = predicateJson.as[BiolinkPredicate](Implicits.biolinkPredicateDecoder(biolinkData.predicates)).toOption.get
        assert(predicate)(equalTo(decoded))
      }
      testCase.provideCustomLayer(testLayer)
    }
  )

  val testBiolinkClassDecoder = suite("testBiolinkClassDecoder")(
    testM("test Implicits.biolinkClassDecoder") {
      val testCase = for {
        biolinkData <- Biolink.biolinkData
      } yield {
        val bc = BiolinkClass("MacromolecularMachine")
        val bcJson = bc.asJson(Implicits.biolinkClassEncoder)
        val decoded = bcJson.as[BiolinkClass](Implicits.biolinkClassDecoder(biolinkData.classes)).toOption.get
        assert(bc)(equalTo(decoded))
      }
      testCase.provideCustomLayer(testLayer)
    }
  )

  val testIRIDecoder = suite("testIRIDecoder")(
    testM("test Implicits.iriDecoder") {
      val testCase = for {
        biolinkData <- Biolink.biolinkData
      } yield {
        val iri = IRI("http://identifiers.org/ncbigene/558")
        val iriJson = iri.asJson(Implicits.iriEncoder(biolinkData.prefixes))
        val decoded = iriJson.as[IRI](Implicits.iriDecoder(biolinkData.prefixes)).toOption.get
        assert(iri)(equalTo(decoded))
      }
      testCase.provideCustomLayer(testLayer)
    }
  )

  val testIRIKeyEncoder = suite("testIRIKeyEncoder")(
    testM("test Implicits.iriKeyEncoder") {
      val testCase = for {
        biolinkData <- Biolink.biolinkData
      } yield {
        implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
        val nodeMap: Map[IRI, TRAPINode] = Map((IRI("http://identifiers.org/ncbigene/558"), TRAPINode(Some("asdf"), Some(List(BiolinkClass("Gene"))), None)))
        val edgeMap: Map[String, TRAPIEdge] = Map(("zxcv", TRAPIEdge(IRI("zxcv-sub"), IRI("zxcv-obj"), None, Some(BiolinkPredicate("related_to")), None)))
        val knowledgeGraph = TRAPIKnowledgeGraph(nodeMap, edgeMap)
        val json = knowledgeGraph.asJson.deepDropNullValues.noSpaces
        assert(json)(containsString("\"NCBIGENE:558\":"))
      }
      testCase.provideCustomLayer(testLayer)
    }
  )

  val testIRIKeyDecoder = suite("testIRIKeyDecoder")(
    testM("test Implicits.iriKeyDecoder") {
      val testCase = for {
        biolinkData <- Biolink.biolinkData
      } yield {
        val jsonString = "{\"nodes\":{\"NCBIGENE:558\":{\"name\":\"asdf\",\"category\":[{\"shorthand\":\"Gene\",\"iri\":{\"value\":\"https://w3id.org/biolink/vocab/Gene\"}}]}},\"edges\":{\"zxcv\":{\"subject\":{\"value\":\"zxcv-sub\"},\"object\":{\"value\":\"zxcv-obj\"},\"predicate\":{\"shorthand\":\"related_to\",\"iri\":{\"value\":\"https://w3id.org/biolink/vocab/related_to\"}}}}}"
        implicit val iriKeyDecoder: KeyDecoder[IRI] = Implicits.iriKeyDecoder(biolinkData.prefixes)
        val kg = decode[TRAPIKnowledgeGraph](jsonString)
        assert(kg.map(a => a.nodes.keys).toOption.get)(contains(IRI("http://identifiers.org/ncbigene/558")))
      }
      testCase.provideCustomLayer(testLayer)
    }
  )

  def spec = suite("Implicits tests")(
    testIRIEncoder,
    testCompactIRIIfPossible,
    testExpandCURIEString,
    testBiolinkPredicateDecoder,
    testBiolinkClassDecoder,
    testIRIDecoder,
    testIRIKeyEncoder,
    testIRIKeyDecoder
  )

}

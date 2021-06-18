package org.renci.cam.it

import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Encoder, KeyEncoder}
import org.http4s._
import org.http4s.headers._
import org.http4s.implicits._
import org.renci.cam.Biolink.BiolinkData
import org.renci.cam.HttpClient.HttpClient
import org.renci.cam._
import org.renci.cam.domain._
import zio.blocking.Blocking
import zio.interop.catz._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.testEnvironment
import zio.{Has, RIO, Task}

import java.nio.file.{Files, Paths}

object QueryServiceTest extends DefaultRunnableSpec {

  def runTest(trapiQuery: TRAPIQuery): RIO[HttpClient with Has[BiolinkData], String] =
    for {
      httpClient <- HttpClient.client
      biolinkData <- Biolink.biolinkData
      encoded = {
        implicit val iriEncoder: Encoder[IRI] = Implicits.iriEncoder(biolinkData.prefixes)
        implicit val iriKeyEncoder: KeyEncoder[IRI] = Implicits.iriKeyEncoder(biolinkData.prefixes)
        implicit val biolinkClassEncoder: Encoder[BiolinkClass] = Implicits.biolinkClassEncoder
        implicit val biolinkPredicateEncoder: Encoder[BiolinkPredicate] = Implicits.biolinkPredicateEncoder(biolinkData.prefixes)
        trapiQuery.asJson.deepDropNullValues.noSpaces
      }
      _ = println("encoded: " + encoded)
      uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1) //.withQueryParam("include_extra_edges", true)
      request = Request[Task](Method.POST, uri)
        .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
        .withEntity(encoded)
      response <- httpClient.expect[String](request)
      //        _ = println("response: " + response)
    } yield response

  val testSimpleQuery = suite("testSimpleQuery")(
    testM("test simple query") {
      val n0Node = TRAPIQueryNode(None, Some(List(BiolinkClass("GeneOrGeneProduct"))), None)
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("BiologicalProcess"))), None)
      val e0Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("has_participant"))), None, "n1", "n0", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        response <- runTest(requestBody)
        _ = Files.writeString(Paths.get("src/it/resources/test-simple-query.json"), response)
      } yield assert(response)(isNonEmptyString)
    }
  )

  val testWIKIQueryExample = suite("testWIKIQueryExample")(
    testM("test WIKIQueryExample") {
      val n0Node = TRAPIQueryNode(None, Some(List(BiolinkClass("GeneOrGeneProduct"))), None)
      val n1Node =
        TRAPIQueryNode(Some(List(IRI("GO:0005634"))), Some(List(BiolinkClass("AnatomicalEntity"))), None)
      val e0Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("part_of"))), None, "n0", "n1", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        response <- runTest(requestBody)
        _ = Files.writeString(Paths.get("src/it/resources/test-wiki-query-example.json"), response)
      } yield assert(response)(isNonEmptyString)
    }
  )

  val testChemicalSubstanceKCNMA1 = suite("testChemicalSubstanceKCNMA1")(
    testM("test ChemicalSubstanceKCNMA1") {
      val n0Node = TRAPIQueryNode(None, Some(List(BiolinkClass("ChemicalSubstance"))), None)
      val n1Node =
        TRAPIQueryNode(Some(List(IRI("HGNC:6284"))), Some(List(BiolinkClass("Gene"))), None)
      val e0Edge = TRAPIQueryEdge(None, None, "n0", "n1", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        response <- runTest(requestBody)
        _ = Files.writeString(Paths.get("src/it/resources/test-ChemicalSubstanceKCNMA1.json"), response)
      } yield assert(response)(isNonEmptyString)
    }
  )

  val testChemicalToGeneOrGeneProduct = suite("testChemicalToGeneOrGeneProduct")(
    testM("test ChemicalToGeneOrGeneProduct") {
      val n0Node = TRAPIQueryNode(Some(List(IRI("CHEBI:17754"))), None, None)
      val n1Node =
        TRAPIQueryNode(None, Some(List(BiolinkClass("GeneOrGeneProduct"))), None)
      val e0Edge = TRAPIQueryEdge(None, None, "n0", "n1", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        response <- runTest(requestBody)
        _ = Files.writeString(Paths.get("src/it/resources/test-ChemicalToGeneOrGeneProduct.json"), response)
      } yield assert(response)(isNonEmptyString)
    }
  )

  val testKCNMA1DiseasePhenotypicFeat = suite("testKCNMA1DiseasePhenotypicFeat")(
    testM("test KCNMA1 Disease Phenotypic Feat") {
      val n0Node = TRAPIQueryNode(Some(List(IRI("HGNC:6284"))), Some(List(BiolinkClass("Gene"))), None)
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("Disease"))), None)
      val n2Node = TRAPIQueryNode(None, Some(List(BiolinkClass("PhenotypicFeature"))), None)
      val e0Edge = TRAPIQueryEdge(None, None, "n0", "n1", None)
      val e1Edge = TRAPIQueryEdge(None, None, "n1", "n2", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node, "n2" -> n2Node), Map("e0" -> e0Edge, "e1" -> e1Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        response <- runTest(requestBody)
        _ = Files.writeString(Paths.get("src/it/resources/test-KCNMA1DiseasePhenotypicFeat.json"), response)
      } yield assert(response)(isNonEmptyString)
    }
  )

  val testFindGenesEnablingAnyKindOfCatalyticActivity = suite("testFindGenesEnablingAnyKindOfCatalyticActivity")(
    testM("find genes enabling any kind of catalytic activity") {
      val n0Node = TRAPIQueryNode(None, Some(List(BiolinkClass("GeneOrGeneProduct"))), None)
      val n1Node =
        TRAPIQueryNode(Some(List(IRI("http://purl.obolibrary.org/obo/GO_0003824"))), Some(List(BiolinkClass("MolecularActivity"))), None)
      val e0Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("enabled_by"))), None, "n1", "n0", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        response <- runTest(requestBody)
        _ = Files.writeString(Paths.get("src/it/resources/test-find-genes-enabling-catalytic-activity.json"), response)
      } yield assert(response)(isNonEmptyString)
    }
  )

  val testGene2Process2Process2Gene = suite("testGene2Process2Process2Gene")(
    testM("test gene to process to process to gene") {
      val n0Node = TRAPIQueryNode(Some(List(IRI("http://identifiers.org/uniprot/P30530"))), Some(List(BiolinkClass("Gene"))), None)
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("BiologicalProcess"))), None)
      val n2Node = TRAPIQueryNode(None, Some(List(BiolinkClass("BiologicalProcess"))), None)
      val n3Node = TRAPIQueryNode(None, Some(List(BiolinkClass("Gene"))), None)
      val e0Edge = TRAPIQueryEdge(None, None, "n1", "n0", None)
      val e1Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("enabled_by"))), None, "n1", "n2", None)
      val e2Edge = TRAPIQueryEdge(None, None, "n2", "n3", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node, "n2" -> n2Node, "n3" -> n3Node),
                                       Map("e0" -> e0Edge, "e1" -> e1Edge, "e2" -> e2Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        response <- runTest(requestBody)
        _ = Files.writeString(Paths.get("src/it/resources/test-gene-to-process-to-process-to-gene.json"), response)
      } yield assert(response)(isNonEmptyString)
    }
  )

  val testNegativeRegulationChaining = suite("testNegativeRegulationChaining")(
    testM("negative regulation chaining") {
      val n0Node = TRAPIQueryNode(Some(List(IRI("http://purl.obolibrary.org/obo/GO_0004252"))),
                                  Some(List(BiolinkClass("BiologicalProcessOrActivity"))),
                                  None)
      val n1Node = TRAPIQueryNode(Some(List(IRI("http://purl.obolibrary.org/obo/GO_0003810"))),
                                  Some(List(BiolinkClass("BiologicalProcessOrActivity"))),
                                  None)
      val n2Node = TRAPIQueryNode(None, Some(List(BiolinkClass("GeneOrGeneProduct"))), None)
      val e0Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("positively_regulates"))), None, "n0", "n1", None)
      val e1Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("enabled_by"))), None, "n1", "n2", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node, "n2" -> n2Node), Map("e0" -> e0Edge, "e1" -> e1Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        response <- runTest(requestBody)
        _ = Files.writeString(Paths.get("src/it/resources/test-negative-regulation-chaining.json"), response)
      } yield assert(response)(isNonEmptyString)
    }
  )

  val testAcrocyanosis = suite("testAcrocyanosis")(
    testM("acrocyanosis") {
      val n0Node = TRAPIQueryNode(Some(List(IRI("UMLS:C0221347"))), Some(List(BiolinkClass("PhenotypicFeature"))), None)
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("NamedThing"))), None)
      val e0Edge = TRAPIQueryEdge(None, None, "n0", "n1", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        response <- runTest(requestBody)
        _ = Files.writeString(Paths.get("src/it/resources/test-acrocyanosis.json"), response)
      } yield assert(response)(isNonEmptyString)
    }
  )

  val testBeclomethasone = suite("testBeclomethasone")(
    testM("beclomethasone") {
      val n0Node = TRAPIQueryNode(Some(List(IRI("DRUGBANK:DB00394"))), Some(List(BiolinkClass("Drug"))), None)
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("Disease"))), None)
      val e0Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("treated_by"))), None, "n0", "n1", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        response <- runTest(requestBody)
        _ = Files.writeString(Paths.get("src/it/resources/test-beclomethasone.json"), response)
      } yield assert(response)(isNonEmptyString)
    }
  )

  val testCorrelatedWith = suite("testCorrelatedWith")(
    testM("correlatedWith") {
      val n0Node = TRAPIQueryNode(Some(List(IRI("MONDO:0004979"))), Some(List(BiolinkClass("Disease"))), None)
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("ChemicalSubstance"))), None)
      val e0Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("correlated_with"))), None, "n0", "n1", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        response <- runTest(requestBody)
        _ = Files.writeString(Paths.get("src/it/resources/test-correlated-with.json"), response)
      } yield assert(response)(isNonEmptyString)
    }
  )

  val testPathway = suite("testPathway")(
    testM("pathway") {
      val n0Node = TRAPIQueryNode(Some(List(IRI("NCBIGene:1017"))), Some(List(BiolinkClass("Gene"))), None)
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("Pathway"))), None)
      val e0Edge = TRAPIQueryEdge(None, None, "n0", "n1", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        response <- runTest(requestBody)
        _ = Files.writeString(Paths.get("src/it/resources/test-pathway.json"), response)
      } yield assert(response)(isNonEmptyString)
    }
  )

  val testSpmsyChemicals = suite("testSpmsyChemicals")(
    testM("spmsyChemicals") {
      val n0Node = TRAPIQueryNode(Some(List(IRI("UniProtKB:P52788"))), Some(List(BiolinkClass("Gene"))), None)
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("ChemicalSubstance"))), None)
      val e0Edge = TRAPIQueryEdge(None, None, "n0", "n1", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        response <- runTest(requestBody)
        _ = Files.writeString(Paths.get("src/it/resources/test-spmsy-chemicals.json"), response)
      } yield assert(response)(isNonEmptyString)
    }
  )

  val testILSixDownRegulators = suite("testILSixDownRegulators")(
    testM("IL-6DownRegulators") {
      val n0Node = TRAPIQueryNode(Some(List(IRI("HGNC:6018"))), None, None)
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("ChemicalSubstance"))), None)
      val predicateList = List(
        BiolinkPredicate("prevents"),
        BiolinkPredicate("negatively_regulates"),
        BiolinkPredicate("decreases_secretion_of"),
        BiolinkPredicate("decreases_transport_of"),
        BiolinkPredicate("decreases_activity_of"),
        BiolinkPredicate("decreases_synthesis_of"),
        BiolinkPredicate("decreases_expression_of"),
        BiolinkPredicate("increases_degradation_of"),
        BiolinkPredicate("negatively_regulates__entity_to_entity"),
        BiolinkPredicate("disrupts"),
        BiolinkPredicate("directly_negatively_regulates"),
        BiolinkPredicate("inhibits"),
        BiolinkPredicate("inhibitor"),
        BiolinkPredicate("channel_blocker"),
        BiolinkPredicate("may_inhibit_effect_of")
      )
      val e0Edge = TRAPIQueryEdge(Some(predicateList), None, "n1", "n0", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        response <- runTest(requestBody)
        _ = Files.writeString(Paths.get("src/it/resources/test-ILSixDownRegulators.json"), response)
      } yield assert(response)(isNonEmptyString)
    }
  )

  val testERAD = suite("testERAD")(
    testM("erad") {
      val n0Node = TRAPIQueryNode(Some(List(IRI("GO:0036503"))), Some(List(BiolinkClass("BiologicalProcess"))), None)
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("GeneOrGeneProduct"))), None)
      val e0Edge = TRAPIQueryEdge(None, None, "n1", "n0", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        response <- runTest(requestBody)
        _ = Files.writeString(Paths.get("src/it/resources/test-erad.json"), response)
      } yield assert(response)(isNonEmptyString)
    }
  )

  val testDILIGeneList = suite("testDILIGeneList")(
    testM("DILIGeneList") {
      val n0Node = TRAPIQueryNode(
        Some(
          List(
            IRI("NCBIGene:2036"),
            IRI("NCBIGene:1431"),
            IRI("NCBIGene:79132"),
            IRI("NCBIGene:3633"),
            IRI("NCBIGene:6262"),
            IRI("NCBIGene:283659"),
            IRI("NCBIGene:89870"),
            IRI("NCBIGene:9487"),
            IRI("NCBIGene:7802"),
            IRI("NCBIGene:57787"),
            IRI("NCBIGene:255738"),
            IRI("NCBIGene:2057"),
            IRI("NCBIGene:144717"),
            IRI("NCBIGene:10019"),
            IRI("NCBIGene:2335"),
            IRI("NCBIGene:23076"),
            IRI("NCBIGene:7917"),
            IRI("NCBIGene:64288"),
            IRI("NCBIGene:11018"),
            IRI("NCBIGene:8343"),
            IRI("NCBIGene:10428"),
            IRI("NCBIGene:10384"),
            IRI("NCBIGene:84081"),
            IRI("NCBIGene:25980"),
            IRI("NCBIGene:6521"),
            IRI("NCBIGene:7155"),
            IRI("NCBIGene:4855"),
            IRI("NCBIGene:993"),
            IRI("NCBIGene:163486"),
            IRI("NCBIGene:4088"),
            IRI("NCBIGene:2275"),
            IRI("NCBIGene:5926"),
            IRI("NCBIGene:27030"),
            IRI("NCBIGene:1674"),
            IRI("NCBIGene:256364"),
            IRI("NCBIGene:51385"),
            IRI("NCBIGene:440603"),
            IRI("NCBIGene:6941"),
            IRI("NCBIGene:57188"),
            IRI("NCBIGene:375056"),
            IRI("NCBIGene:54887"),
            IRI("NCBIGene:783"),
            IRI("NCBIGene:84217"),
            IRI("NCBIGene:55204"),
            IRI("NCBIGene:6546"),
            IRI("NCBIGene:60678"),
            IRI("NCBIGene:152273"),
            IRI("NCBIGene:10203"),
            IRI("NCBIGene:100507679"),
            IRI("NCBIGene:1021"),
            IRI("NCBIGene:11060"),
            IRI("NCBIGene:57057"),
            IRI("NCBIGene:7456"),
            IRI("NCBIGene:149345"),
            IRI("NCBIGene:4952"),
            IRI("NCBIGene:23132"),
            IRI("NCBIGene:22852"),
            IRI("NCBIGene:1605"),
            IRI("NCBIGene:79800"),
            IRI("NCBIGene:90324"),
            IRI("NCBIGene:219469"),
            IRI("NCBIGene:8542"),
            IRI("NCBIGene:8927"),
            IRI("NCBIGene:2250"),
            IRI("NCBIGene:144100"),
            IRI("NCBIGene:10642"),
            IRI("NCBIGene:80304"),
            IRI("NCBIGene:57679"),
            IRI("NCBIGene:3621"),
            IRI("NCBIGene:25894"),
            IRI("NCBIGene:57176"),
            IRI("NCBIGene:2122"),
            IRI("NCBIGene:23499"),
            IRI("NCBIGene:4439"),
            IRI("NCBIGene:5175"),
            IRI("NCBIGene:641649"),
            IRI("NCBIGene:80736"),
            IRI("NCBIGene:25865"),
            IRI("NCBIGene:1460"),
            IRI("NCBIGene:1029"),
            IRI("NCBIGene:4485"),
            IRI("NCBIGene:1138"),
            IRI("NCBIGene:130026"),
            IRI("NCBIGene:80345"),
            IRI("NCBIGene:284058"),
            IRI("NCBIGene:5350"),
            IRI("NCBIGene:629"),
            IRI("NCBIGene:119032"),
            IRI("NCBIGene:79893"),
            IRI("NCBIGene:56658"),
            IRI("NCBIGene:162540"),
            IRI("NCBIGene:1244"),
            IRI("NCBIGene:23403"),
            IRI("NCBIGene:7871"),
            IRI("NCBIGene:9564"),
            IRI("NCBIGene:4277"),
            IRI("NCBIGene:8654"),
            IRI("NCBIGene:2321"),
            IRI("NCBIGene:54205"),
            IRI("NCBIGene:55297"),
            IRI("NCBIGene:349149"),
            IRI("NCBIGene:84811"),
            IRI("NCBIGene:23047"),
            IRI("NCBIGene:148362"),
            IRI("NCBIGene:64853"),
            IRI("NCBIGene:9457"),
            IRI("NCBIGene:23149"),
            IRI("NCBIGene:114971"),
            IRI("NCBIGene:3360"),
            IRI("NCBIGene:10291"),
            IRI("NCBIGene:23328"),
            IRI("NCBIGene:7586"),
            IRI("NCBIGene:23245"),
            IRI("NCBIGene:4023"),
            IRI("NCBIGene:55785"),
            IRI("NCBIGene:51168"),
            IRI("NCBIGene:165082"),
            IRI("NCBIGene:135"),
            IRI("NCBIGene:146212"),
            IRI("NCBIGene:9785"),
            IRI("NCBIGene:8407"),
            IRI("NCBIGene:135656"),
            IRI("NCBIGene:7726"),
            IRI("NCBIGene:79991"),
            IRI("NCBIGene:643314"),
            IRI("NCBIGene:10456"),
            IRI("NCBIGene:4625"),
            IRI("NCBIGene:101929256"),
            IRI("NCBIGene:401250"),
            IRI("NCBIGene:84332"),
            IRI("NCBIGene:563"),
            IRI("NCBIGene:643394"),
            IRI("NCBIGene:81697"),
            IRI("NCBIGene:55733"),
            IRI("NCBIGene:4482"),
            IRI("NCBIGene:7916"),
            IRI("NCBIGene:63892"),
            IRI("NCBIGene:80309"),
            IRI("NCBIGene:79875"),
            IRI("NCBIGene:79807"),
            IRI("NCBIGene:6597"),
            IRI("NCBIGene:55823"),
            IRI("NCBIGene:57153"),
            IRI("NCBIGene:26133"),
            IRI("NCBIGene:3920"),
            IRI("NCBIGene:92579"),
            IRI("NCBIGene:10713"),
            IRI("NCBIGene:10806"),
            IRI("NCBIGene:23053"),
            IRI("NCBIGene:516"),
            IRI("NCBIGene:83874"),
            IRI("NCBIGene:2169"),
            IRI("NCBIGene:8829"),
            IRI("NCBIGene:1284"),
            IRI("NCBIGene:23316"),
            IRI("NCBIGene:2038"),
            IRI("NCBIGene:4739"),
            IRI("NCBIGene:169792"),
            IRI("NCBIGene:2983"),
            IRI("NCBIGene:9575"),
            IRI("NCBIGene:10786"),
            IRI("NCBIGene:1909"),
            IRI("NCBIGene:84072"),
            IRI("NCBIGene:9076"),
            IRI("NCBIGene:54622"),
            IRI("NCBIGene:10020"),
            IRI("NCBIGene:2982"),
            IRI("NCBIGene:1136"),
            IRI("NCBIGene:11118"),
            IRI("NCBIGene:9260"),
            IRI("NCBIGene:3240"),
            IRI("NCBIGene:10107"),
            IRI("NCBIGene:6188"),
            IRI("NCBIGene:348938"),
            IRI("NCBIGene:221692"),
            IRI("NCBIGene:2628"),
            IRI("NCBIGene:81577"),
            IRI("NCBIGene:9570"),
            IRI("NCBIGene:56241"),
            IRI("NCBIGene:5684"),
            IRI("NCBIGene:81619"),
            IRI("NCBIGene:7078"),
            IRI("NCBIGene:165215"),
            IRI("NCBIGene:7056"),
            IRI("NCBIGene:3156"),
            IRI("NCBIGene:284076"),
            IRI("NCBIGene:29113"),
            IRI("NCBIGene:91754"),
            IRI("NCBIGene:54828"),
            IRI("NCBIGene:54535"),
            IRI("NCBIGene:54658"),
            IRI("NCBIGene:11261"),
            IRI("NCBIGene:57211"),
            IRI("NCBIGene:54454"),
            IRI("NCBIGene:338"),
            IRI("NCBIGene:10878"),
            IRI("NCBIGene:245812"),
            IRI("NCBIGene:6314"),
            IRI("NCBIGene:646"),
            IRI("NCBIGene:23384"),
            IRI("NCBIGene:5048"),
            IRI("NCBIGene:7168"),
            IRI("NCBIGene:6444"),
            IRI("NCBIGene:80737"),
            IRI("NCBIGene:100631383"),
            IRI("NCBIGene:8356"),
            IRI("NCBIGene:9938"),
            IRI("NCBIGene:143903"),
            IRI("NCBIGene:5795"),
            IRI("NCBIGene:84722"),
            IRI("NCBIGene:64761"),
            IRI("NCBIGene:10466"),
            IRI("NCBIGene:6128"),
            IRI("NCBIGene:2022"),
            IRI("NCBIGene:8508"),
            IRI("NCBIGene:123688"),
            IRI("NCBIGene:9674"),
            IRI("NCBIGene:144348"),
            IRI("NCBIGene:9531"),
            IRI("NCBIGene:2793"),
            IRI("NCBIGene:7369"),
            IRI("NCBIGene:3609"),
            IRI("NCBIGene:342618"),
            IRI("NCBIGene:401152"),
            IRI("NCBIGene:91624"),
            IRI("NCBIGene:1824"),
            IRI("NCBIGene:649"),
            IRI("NCBIGene:7122"),
            IRI("NCBIGene:4624"),
            IRI("NCBIGene:28964"),
            IRI("NCBIGene:128611"),
            IRI("NCBIGene:114821"),
            IRI("NCBIGene:3913"),
            IRI("NCBIGene:79029"),
            IRI("NCBIGene:2070"),
            IRI("NCBIGene:57419"),
            IRI("NCBIGene:7718"),
            IRI("NCBIGene:602"),
            IRI("NCBIGene:255809"),
            IRI("NCBIGene:80864"),
            IRI("NCBIGene:6048"),
            IRI("NCBIGene:9618"),
            IRI("NCBIGene:4000"),
            IRI("NCBIGene:84196"),
            IRI("NCBIGene:3105"),
            IRI("NCBIGene:57608"),
            IRI("NCBIGene:8315"),
            IRI("NCBIGene:81027"),
            IRI("NCBIGene:3078"),
            IRI("NCBIGene:23376"),
            IRI("NCBIGene:171024"),
            IRI("NCBIGene:268"),
            IRI("NCBIGene:3009"),
            IRI("NCBIGene:5819"),
            IRI("NCBIGene:1788"),
            IRI("NCBIGene:28"),
            IRI("NCBIGene:80790"),
            IRI("NCBIGene:10933"),
            IRI("NCBIGene:7918"),
            IRI("NCBIGene:7940"),
            IRI("NCBIGene:10524"),
            IRI("NCBIGene:79068"),
            IRI("NCBIGene:8395"),
            IRI("NCBIGene:64858"),
            IRI("NCBIGene:7145"),
            IRI("NCBIGene:2625"),
            IRI("NCBIGene:85366"),
            IRI("NCBIGene:7741"),
            IRI("NCBIGene:4166"),
            IRI("NCBIGene:63976"),
            IRI("NCBIGene:286"),
            IRI("NCBIGene:11044"),
            IRI("NCBIGene:51322"),
            IRI("NCBIGene:27332"),
            IRI("NCBIGene:79696"),
            IRI("NCBIGene:80321"),
            IRI("NCBIGene:100528030"),
            IRI("NCBIGene:2677"),
            IRI("NCBIGene:7112"),
            IRI("NCBIGene:11077"),
            IRI("NCBIGene:1729"),
            IRI("NCBIGene:56895"),
            IRI("NCBIGene:80018"),
            IRI("NCBIGene:51129"),
            IRI("NCBIGene:8048"),
            IRI("NCBIGene:23293"),
            IRI("NCBIGene:53339"),
            IRI("NCBIGene:6569"),
            IRI("NCBIGene:55179"),
            IRI("NCBIGene:5335"),
            IRI("NCBIGene:3482"),
            IRI("NCBIGene:97"),
            IRI("NCBIGene:4015"),
            IRI("NCBIGene:717"),
            IRI("NCBIGene:85464"),
            IRI("NCBIGene:58476"),
            IRI("NCBIGene:406"),
            IRI("NCBIGene:7273"),
            IRI("NCBIGene:80724"),
            IRI("NCBIGene:26707"),
            IRI("NCBIGene:6499"),
            IRI("NCBIGene:440275"),
            IRI("NCBIGene:3250"),
            IRI("NCBIGene:29128"),
            IRI("NCBIGene:7414"),
            IRI("NCBIGene:1026"),
            IRI("NCBIGene:84547"),
            IRI("NCBIGene:7746"),
            IRI("NCBIGene:80776"),
            IRI("NCBIGene:3897"),
            IRI("NCBIGene:8348"),
            IRI("NCBIGene:129303"),
            IRI("NCBIGene:9862"),
            IRI("NCBIGene:5878"),
            IRI("NCBIGene:137682"),
            IRI("NCBIGene:5781"),
            IRI("NCBIGene:57644"),
            IRI("NCBIGene:389118"),
            IRI("NCBIGene:55249"),
            IRI("NCBIGene:8354"),
            IRI("NCBIGene:150864"),
            IRI("NCBIGene:727"),
            IRI("NCBIGene:6636"),
            IRI("NCBIGene:57819"),
            IRI("NCBIGene:10919"),
            IRI("NCBIGene:644"),
            IRI("NCBIGene:2120"),
            IRI("NCBIGene:7703"),
            IRI("NCBIGene:9837"),
            IRI("NCBIGene:7745"),
            IRI("NCBIGene:1762"),
            IRI("NCBIGene:7840"),
            IRI("NCBIGene:64240"),
            IRI("NCBIGene:5978"),
            IRI("NCBIGene:4627"),
            IRI("NCBIGene:221955"),
            IRI("NCBIGene:29956"),
            IRI("NCBIGene:10211"),
            IRI("NCBIGene:10554"),
            IRI("NCBIGene:3728"),
            IRI("NCBIGene:3122"),
            IRI("NCBIGene:3949"),
            IRI("NCBIGene:3236"),
            IRI("NCBIGene:9869"),
            IRI("NCBIGene:10665"),
            IRI("NCBIGene:55101"),
            IRI("NCBIGene:3930"),
            IRI("NCBIGene:10877"),
            IRI("NCBIGene:6441"),
            IRI("NCBIGene:4829"),
            IRI("NCBIGene:65264"),
            IRI("NCBIGene:1394"),
            IRI("NCBIGene:25802"),
            IRI("NCBIGene:23255"),
            IRI("NCBIGene:53916"),
            IRI("NCBIGene:780"),
            IRI("NCBIGene:2762"),
            IRI("NCBIGene:79147"),
            IRI("NCBIGene:8882"),
            IRI("NCBIGene:2811"),
            IRI("NCBIGene:2948"),
            IRI("NCBIGene:51232"),
            IRI("NCBIGene:4781"),
            IRI("NCBIGene:6097"),
            IRI("NCBIGene:54664"),
            IRI("NCBIGene:55167"),
            IRI("NCBIGene:1012"),
            IRI("NCBIGene:6494"),
            IRI("NCBIGene:50863"),
            IRI("NCBIGene:57553"),
            IRI("NCBIGene:5568"),
            IRI("NCBIGene:55022"),
            IRI("NCBIGene:64771"),
            IRI("NCBIGene:23051"),
            IRI("NCBIGene:55037"),
            IRI("NCBIGene:8224"),
            IRI("NCBIGene:51422"),
            IRI("NCBIGene:84159"),
            IRI("NCBIGene:29922"),
            IRI("NCBIGene:80863"),
            IRI("NCBIGene:53335"),
            IRI("NCBIGene:490"),
            IRI("NCBIGene:118663"),
            IRI("NCBIGene:10060"),
            IRI("NCBIGene:57404"),
            IRI("NCBIGene:1282"),
            IRI("NCBIGene:7472"),
            IRI("NCBIGene:2017"),
            IRI("NCBIGene:6581"),
            IRI("NCBIGene:1071"),
            IRI("NCBIGene:166815"),
            IRI("NCBIGene:3119"),
            IRI("NCBIGene:84273"),
            IRI("NCBIGene:10287"),
            IRI("NCBIGene:6708"),
            IRI("NCBIGene:222962"),
            IRI("NCBIGene:221527"),
            IRI("NCBIGene:6891"),
            IRI("NCBIGene:6238"),
            IRI("NCBIGene:7139"),
            IRI("NCBIGene:5699"),
            IRI("NCBIGene:51686"),
            IRI("NCBIGene:346"),
            IRI("NCBIGene:7181"),
            IRI("NCBIGene:10960"),
            IRI("NCBIGene:63826"),
            IRI("NCBIGene:10475"),
            IRI("NCBIGene:22907"),
            IRI("NCBIGene:199"),
            IRI("NCBIGene:222696"),
            IRI("NCBIGene:84315"),
            IRI("NCBIGene:10677"),
            IRI("NCBIGene:2812"),
            IRI("NCBIGene:63891"),
            IRI("NCBIGene:22978"),
            IRI("NCBIGene:2119"),
            IRI("NCBIGene:201176"),
            IRI("NCBIGene:4128"),
            IRI("NCBIGene:88"),
            IRI("NCBIGene:4237"),
            IRI("NCBIGene:10452"),
            IRI("NCBIGene:7043"),
            IRI("NCBIGene:659"),
            IRI("NCBIGene:2845"),
            IRI("NCBIGene:81846"),
            IRI("NCBIGene:54814"),
            IRI("NCBIGene:221545"),
            IRI("NCBIGene:10498"),
            IRI("NCBIGene:2590"),
            IRI("NCBIGene:6457"),
            IRI("NCBIGene:5089"),
            IRI("NCBIGene:3171"),
            IRI("NCBIGene:55973"),
            IRI("NCBIGene:5664"),
            IRI("NCBIGene:534"),
            IRI("NCBIGene:79890"),
            IRI("NCBIGene:374907"),
            IRI("NCBIGene:3910"),
            IRI("NCBIGene:23400"),
            IRI("NCBIGene:408263"),
            IRI("NCBIGene:55937"),
            IRI("NCBIGene:8563"),
            IRI("NCBIGene:10791"),
            IRI("NCBIGene:10293"),
            IRI("NCBIGene:147912"),
            IRI("NCBIGene:5076"),
            IRI("NCBIGene:23054"),
            IRI("NCBIGene:1388"),
            IRI("NCBIGene:8341"),
            IRI("NCBIGene:6311"),
            IRI("NCBIGene:2029"),
            IRI("NCBIGene:124930"),
            IRI("NCBIGene:29777"),
            IRI("NCBIGene:1757"),
            IRI("NCBIGene:6331"),
            IRI("NCBIGene:8645"),
            IRI("NCBIGene:6927"),
            IRI("NCBIGene:4633"),
            IRI("NCBIGene:3075"),
            IRI("NCBIGene:64241"),
            IRI("NCBIGene:9939"),
            IRI("NCBIGene:1184"),
            IRI("NCBIGene:84276"),
            IRI("NCBIGene:124491"),
            IRI("NCBIGene:8673"),
            IRI("NCBIGene:8340"),
            IRI("NCBIGene:388553"),
            IRI("NCBIGene:11155"),
            IRI("NCBIGene:283373"),
            IRI("NCBIGene:1991"),
            IRI("NCBIGene:50854"),
            IRI("NCBIGene:6301"),
            IRI("NCBIGene:204474"),
            IRI("NCBIGene:154150"),
            IRI("NCBIGene:7473"),
            IRI("NCBIGene:80317"),
            IRI("NCBIGene:9374"),
            IRI("NCBIGene:8368"),
            IRI("NCBIGene:662"),
            IRI("NCBIGene:51112"),
            IRI("NCBIGene:2648"),
            IRI("NCBIGene:2118"),
            IRI("NCBIGene:3753"),
            IRI("NCBIGene:4179"),
            IRI("NCBIGene:87"),
            IRI("NCBIGene:6885"),
            IRI("NCBIGene:79154"),
            IRI("NCBIGene:10385"),
            IRI("NCBIGene:535"),
            IRI("NCBIGene:79990"),
            IRI("NCBIGene:3672"),
            IRI("NCBIGene:53346"),
            IRI("NCBIGene:51144"),
            IRI("NCBIGene:81544"),
            IRI("NCBIGene:10393"),
            IRI("NCBIGene:9924"),
            IRI("NCBIGene:6532"),
            IRI("NCBIGene:6714"),
            IRI("NCBIGene:91662"),
            IRI("NCBIGene:177"),
            IRI("NCBIGene:54726"),
            IRI("NCBIGene:4012"),
            IRI("NCBIGene:55705"),
            IRI("NCBIGene:4650"),
            IRI("NCBIGene:2876"),
            IRI("NCBIGene:716"),
            IRI("NCBIGene:78992"),
            IRI("NCBIGene:152485"),
            IRI("NCBIGene:3685"),
            IRI("NCBIGene:5136"),
            IRI("NCBIGene:1819"),
            IRI("NCBIGene:123722"),
            IRI("NCBIGene:254102"),
            IRI("NCBIGene:10858"),
            IRI("NCBIGene:55603"),
            IRI("NCBIGene:55282"),
            IRI("NCBIGene:2158"),
            IRI("NCBIGene:23301"),
            IRI("NCBIGene:4286"),
            IRI("NCBIGene:10390"),
            IRI("NCBIGene:50810"),
            IRI("NCBIGene:11311"),
            IRI("NCBIGene:134549"),
            IRI("NCBIGene:10106"),
            IRI("NCBIGene:55201"),
            IRI("NCBIGene:6439"),
            IRI("NCBIGene:1080"),
            IRI("NCBIGene:83450"),
            IRI("NCBIGene:84651"),
            IRI("NCBIGene:23075"),
            IRI("NCBIGene:51673"),
            IRI("NCBIGene:4143"),
            IRI("NCBIGene:275"),
            IRI("NCBIGene:861"),
            IRI("NCBIGene:3769"),
            IRI("NCBIGene:3308"),
            IRI("NCBIGene:343263"),
            IRI("NCBIGene:56911"),
            IRI("NCBIGene:9807"),
            IRI("NCBIGene:8613"),
            IRI("NCBIGene:79961"),
            IRI("NCBIGene:7454"),
            IRI("NCBIGene:222546"),
            IRI("NCBIGene:5685"),
            IRI("NCBIGene:347736"),
            IRI("NCBIGene:1143"),
            IRI("NCBIGene:11267"),
            IRI("NCBIGene:57158"),
            IRI("NCBIGene:3117"),
            IRI("NCBIGene:57167"),
            IRI("NCBIGene:57580"),
            IRI("NCBIGene:65065"),
            IRI("NCBIGene:2815"),
            IRI("NCBIGene:2696"),
            IRI("NCBIGene:56913"),
            IRI("NCBIGene:259215"),
            IRI("NCBIGene:2626"),
            IRI("NCBIGene:7551"),
            IRI("NCBIGene:55062"),
            IRI("NCBIGene:100129583"),
            IRI("NCBIGene:7919"),
            IRI("NCBIGene:10221"),
            IRI("NCBIGene:389376"),
            IRI("NCBIGene:55741"),
            IRI("NCBIGene:8728"),
            IRI("NCBIGene:1816"),
            IRI("NCBIGene:116519"),
            IRI("NCBIGene:6484"),
            IRI("NCBIGene:10443"),
            IRI("NCBIGene:2695"),
            IRI("NCBIGene:3207"),
            IRI("NCBIGene:51196"),
            IRI("NCBIGene:2316"),
            IRI("NCBIGene:6720"),
            IRI("NCBIGene:9577"),
            IRI("NCBIGene:6239"),
            IRI("NCBIGene:4629"),
            IRI("NCBIGene:3596"),
            IRI("NCBIGene:6560"),
            IRI("NCBIGene:79652"),
            IRI("NCBIGene:6988"),
            IRI("NCBIGene:7040"),
            IRI("NCBIGene:80212"),
            IRI("NCBIGene:400823"),
            IRI("NCBIGene:7407"),
            IRI("NCBIGene:84624"),
            IRI("NCBIGene:5017"),
            IRI("NCBIGene:558"),
            IRI("NCBIGene:2160"),
            IRI("NCBIGene:3106"),
            IRI("NCBIGene:85004"),
            IRI("NCBIGene:4085"),
            IRI("NCBIGene:23414"),
            IRI("NCBIGene:8607"),
            IRI("NCBIGene:29800"),
            IRI("NCBIGene:57117"),
            IRI("NCBIGene:54848"),
            IRI("NCBIGene:7290"),
            IRI("NCBIGene:6389"),
            IRI("NCBIGene:5654"),
            IRI("NCBIGene:57412"),
            IRI("NCBIGene:64978"),
            IRI("NCBIGene:8557"),
            IRI("NCBIGene:55843"),
            IRI("NCBIGene:11173"),
            IRI("NCBIGene:4905"),
            IRI("NCBIGene:5866"),
            IRI("NCBIGene:340260"),
            IRI("NCBIGene:285489"),
            IRI("NCBIGene:341640"),
            IRI("NCBIGene:10743"),
            IRI("NCBIGene:25974"),
            IRI("NCBIGene:8638"),
            IRI("NCBIGene:342372"),
            IRI("NCBIGene:1410"),
            IRI("NCBIGene:83938"),
            IRI("NCBIGene:54897"),
            IRI("NCBIGene:6945"),
            IRI("NCBIGene:9839"),
            IRI("NCBIGene:2146"),
            IRI("NCBIGene:6469"),
            IRI("NCBIGene:201294"),
            IRI("NCBIGene:79660"),
            IRI("NCBIGene:4598"),
            IRI("NCBIGene:4352"),
            IRI("NCBIGene:10181"),
            IRI("NCBIGene:84419"),
            IRI("NCBIGene:170954"),
            IRI("NCBIGene:85465"),
            IRI("NCBIGene:26959"),
            IRI("NCBIGene:5970"),
            IRI("NCBIGene:26084"),
            IRI("NCBIGene:1785"),
            IRI("NCBIGene:55182"),
            IRI("NCBIGene:1952"),
            IRI("NCBIGene:718"),
            IRI("NCBIGene:284119"),
            IRI("NCBIGene:9859"),
            IRI("NCBIGene:23143"),
            IRI("NCBIGene:3097"),
            IRI("NCBIGene:3107"),
            IRI("NCBIGene:84168"),
            IRI("NCBIGene:8509"),
            IRI("NCBIGene:85302"),
            IRI("NCBIGene:55759"),
            IRI("NCBIGene:7287"),
            IRI("NCBIGene:2218"),
            IRI("NCBIGene:10152"),
            IRI("NCBIGene:6580"),
            IRI("NCBIGene:255743"),
            IRI("NCBIGene:10636"),
            IRI("NCBIGene:346171"),
            IRI("NCBIGene:27125"),
            IRI("NCBIGene:51534"),
            IRI("NCBIGene:65059"),
            IRI("NCBIGene:8546"),
            IRI("NCBIGene:10867"),
            IRI("NCBIGene:4324"),
            IRI("NCBIGene:26058"),
            IRI("NCBIGene:405"),
            IRI("NCBIGene:111"),
            IRI("NCBIGene:25959"),
            IRI("NCBIGene:4144"),
            IRI("NCBIGene:9015"),
            IRI("NCBIGene:282809"),
            IRI("NCBIGene:183"),
            IRI("NCBIGene:3988"),
            IRI("NCBIGene:2686"),
            IRI("NCBIGene:22834"),
            IRI("NCBIGene:11062"),
            IRI("NCBIGene:170679"),
            IRI("NCBIGene:55471"),
            IRI("NCBIGene:202658"),
            IRI("NCBIGene:9686"),
            IRI("NCBIGene:4846"),
            IRI("NCBIGene:871"),
            IRI("NCBIGene:348"),
            IRI("NCBIGene:217"),
            IRI("NCBIGene:8892"),
            IRI("NCBIGene:5096"),
            IRI("NCBIGene:949"),
            IRI("NCBIGene:5336"),
            IRI("NCBIGene:3784"),
            IRI("NCBIGene:8328"),
            IRI("NCBIGene:3090"),
            IRI("NCBIGene:11120"),
            IRI("NCBIGene:4507"),
            IRI("NCBIGene:8872"),
            IRI("NCBIGene:5663"),
            IRI("NCBIGene:29842"),
            IRI("NCBIGene:22954"),
            IRI("NCBIGene:57619"),
            IRI("NCBIGene:4137"),
            IRI("NCBIGene:26112"),
            IRI("NCBIGene:55152"),
            IRI("NCBIGene:7148"),
            IRI("NCBIGene:4669"),
            IRI("NCBIGene:146691"),
            IRI("NCBIGene:4607"),
            IRI("NCBIGene:253461"),
            IRI("NCBIGene:8332"),
            IRI("NCBIGene:79188"),
            IRI("NCBIGene:135644"),
            IRI("NCBIGene:84171"),
            IRI("NCBIGene:54805"),
            IRI("NCBIGene:1723"),
            IRI("NCBIGene:23118"),
            IRI("NCBIGene:9258"),
            IRI("NCBIGene:27436"),
            IRI("NCBIGene:282996"),
            IRI("NCBIGene:84439"),
            IRI("NCBIGene:10097"),
            IRI("NCBIGene:3696"),
            IRI("NCBIGene:83988"),
            IRI("NCBIGene:201292"),
            IRI("NCBIGene:554223"),
            IRI("NCBIGene:348093"),
            IRI("NCBIGene:27063"),
            IRI("NCBIGene:6943"),
            IRI("NCBIGene:6497"),
            IRI("NCBIGene:4634"),
            IRI("NCBIGene:9469"),
            IRI("NCBIGene:374786"),
            IRI("NCBIGene:56915"),
            IRI("NCBIGene:4744"),
            IRI("NCBIGene:7035"),
            IRI("NCBIGene:64897"),
            IRI("NCBIGene:1801"),
            IRI("NCBIGene:10972"),
            IRI("NCBIGene:10204"),
            IRI("NCBIGene:7173"),
            IRI("NCBIGene:6631"),
            IRI("NCBIGene:54497"),
            IRI("NCBIGene:7150"),
            IRI("NCBIGene:63940"),
            IRI("NCBIGene:56986"),
            IRI("NCBIGene:344758"),
            IRI("NCBIGene:2242"),
            IRI("NCBIGene:26999"),
            IRI("NCBIGene:9842"),
            IRI("NCBIGene:284106"),
            IRI("NCBIGene:593"),
            IRI("NCBIGene:203068"),
            IRI("NCBIGene:10274"),
            IRI("NCBIGene:6584"),
            IRI("NCBIGene:22808"),
            IRI("NCBIGene:83850"),
            IRI("NCBIGene:10600"),
            IRI("NCBIGene:859"),
            IRI("NCBIGene:11074"),
            IRI("NCBIGene:5045"),
            IRI("NCBIGene:387032"),
            IRI("NCBIGene:1829"),
            IRI("NCBIGene:165324"),
            IRI("NCBIGene:19"),
            IRI("NCBIGene:54957"),
            IRI("NCBIGene:387"),
            IRI("NCBIGene:9753"),
            IRI("NCBIGene:627"),
            IRI("NCBIGene:3570"),
            IRI("NCBIGene:55815"),
            IRI("NCBIGene:28987"),
            IRI("NCBIGene:3426"),
            IRI("NCBIGene:7137"),
            IRI("NCBIGene:81492"),
            IRI("NCBIGene:256764"),
            IRI("NCBIGene:3112"),
            IRI("NCBIGene:550631"),
            IRI("NCBIGene:163115"),
            IRI("NCBIGene:94086"),
            IRI("NCBIGene:54532"),
            IRI("NCBIGene:79650"),
            IRI("NCBIGene:3658"),
            IRI("NCBIGene:127002"),
            IRI("NCBIGene:3305"),
            IRI("NCBIGene:54790"),
            IRI("NCBIGene:1832"),
            IRI("NCBIGene:440387"),
            IRI("NCBIGene:10717"),
            IRI("NCBIGene:387103"),
            IRI("NCBIGene:10906"),
            IRI("NCBIGene:84940"),
            IRI("NCBIGene:283450"),
            IRI("NCBIGene:1436"),
            IRI("NCBIGene:70"),
            IRI("NCBIGene:10327"),
            IRI("NCBIGene:6059"),
            IRI("NCBIGene:27086"),
            IRI("NCBIGene:665"),
            IRI("NCBIGene:1030"),
            IRI("NCBIGene:481"),
            IRI("NCBIGene:4059"),
            IRI("NCBIGene:5460"),
            IRI("NCBIGene:327"),
            IRI("NCBIGene:1760"),
            IRI("NCBIGene:8987"),
            IRI("NCBIGene:4018"),
            IRI("NCBIGene:5340"),
            IRI("NCBIGene:8500"),
            IRI("NCBIGene:8550"),
            IRI("NCBIGene:2672"),
            IRI("NCBIGene:153090"),
            IRI("NCBIGene:4093"),
            IRI("NCBIGene:339965"),
            IRI("NCBIGene:8859"),
            IRI("NCBIGene:11100"),
            IRI("NCBIGene:84071"),
            IRI("NCBIGene:57610"),
            IRI("NCBIGene:100533183"),
            IRI("NCBIGene:170392"),
            IRI("NCBIGene:8761"),
            IRI("NCBIGene:6710"),
            IRI("NCBIGene:2697"),
            IRI("NCBIGene:60626"),
            IRI("NCBIGene:91056"),
            IRI("NCBIGene:6599"),
            IRI("NCBIGene:79012"),
            IRI("NCBIGene:170506"),
            IRI("NCBIGene:6582"),
            IRI("NCBIGene:30835"),
            IRI("NCBIGene:79018"),
            IRI("NCBIGene:7134"),
            IRI("NCBIGene:7005"),
            IRI("NCBIGene:89894"),
            IRI("NCBIGene:5245"),
            IRI("NCBIGene:81545"),
            IRI("NCBIGene:7589"),
            IRI("NCBIGene:143241"),
            IRI("NCBIGene:246744"),
            IRI("NCBIGene:51778"),
            IRI("NCBIGene:1456"),
            IRI("NCBIGene:143872"),
            IRI("NCBIGene:10144"),
            IRI("NCBIGene:6660"),
            IRI("NCBIGene:80036"),
            IRI("NCBIGene:8997"),
            IRI("NCBIGene:11164"),
            IRI("NCBIGene:1586"),
            IRI("NCBIGene:1728"),
            IRI("NCBIGene:5580"),
            IRI("NCBIGene:26281"),
            IRI("NCBIGene:81797"),
            IRI("NCBIGene:5523"),
            IRI("NCBIGene:399909"),
            IRI("NCBIGene:10961"),
            IRI("NCBIGene:84522"),
            IRI("NCBIGene:9153"),
            IRI("NCBIGene:2768"),
            IRI("NCBIGene:25885"),
            IRI("NCBIGene:540"),
            IRI("NCBIGene:5318"),
            IRI("NCBIGene:284485"),
            IRI("NCBIGene:8693"),
            IRI("NCBIGene:64778"),
            IRI("NCBIGene:341"),
            IRI("NCBIGene:91647"),
            IRI("NCBIGene:678")
          )
        ),
        Some(List(BiolinkClass("GeneOrGeneProduct"))),
        None
      )
      val n1Node = TRAPIQueryNode(None, Some(List(BiolinkClass("GeneOrGeneProduct"))), None)
      val e0Edge = TRAPIQueryEdge(Some(List(BiolinkPredicate("affects"))), None, "n0", "n1", None)
      val queryGraph = TRAPIQueryGraph(Map("n0" -> n0Node, "n1" -> n1Node), Map("e0" -> e0Edge))
      val message = TRAPIMessage(Some(queryGraph), None, None)
      val requestBody = TRAPIQuery(message, None)
      for {
        response <- runTest(requestBody)
        _ = Files.writeString(Paths.get("src/it/resources/test-diligenelist.json"), response)
      } yield assert(response)(isNonEmptyString)
    }
  )

  val testSimpleQueryRawWithSinglePredicate = suite("testSimpleQueryRawWithSinglePredicate")(
    testM("simple query raw with single predicate") {
      val message =
        """{"message":{"query_graph":{"nodes":{"n0":{"categories":["biolink:GeneOrGeneProduct"]},"n1":{"categories":["biolink:BiologicalProcess"]}},"edges":{"e0":{"predicates":"biolink:has_participant","subject":"n1","object":"n0"}}}}}"""
      for {
        httpClient <- HttpClient.client
        uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1) // scala
        request = Request[Task](Method.POST, uri)
          .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
          .withEntity(message)
        response <- httpClient.expect[String](request)
      } yield assert(response)(isNonEmptyString)
    }
  )

  val testSimpleQueryRaw = suite("testSimpleQueryRaw")(
    testM("simple query raw") {
      val message =
        """{"message":{"query_graph":{"nodes":{"n0":{"categories":["biolink:GeneOrGeneProduct"]},"n1":{"categories":["biolink:BiologicalProcess"]}},"edges":{"e0":{"predicates":["biolink:has_participant"],"subject":"n1","object":"n0"}}}}}"""
      for {
        httpClient <- HttpClient.client
        uri = uri"http://127.0.0.1:8080/query".withQueryParam("limit", 1) // scala
        request = Request[Task](Method.POST, uri)
          .withHeaders(Accept(MediaType.application.json), `Content-Type`(MediaType.application.json))
          .withEntity(message)
        response <- httpClient.expect[String](request)
      } yield assert(response)(isNonEmptyString)
    }
  )

  val camkpapiTestLayer = Blocking.live >>> TestContainer.camkpapi
  val camkpapiLayer = HttpClient.makeHttpClientLayer >+> Biolink.makeUtilitiesLayer
  val testLayer = (testEnvironment ++ camkpapiTestLayer ++ camkpapiLayer).mapError(TestFailure.die)

  def spec = suite("QueryService tests")(
    testFindGenesEnablingAnyKindOfCatalyticActivity,
    testNegativeRegulationChaining,
    testBeclomethasone,
    testCorrelatedWith,
    testSpmsyChemicals,
    testILSixDownRegulators,
    testGene2Process2Process2Gene,
    testAcrocyanosis,
    testPathway,
    testERAD,
    testSimpleQuery,
    testWIKIQueryExample,
    testSimpleQueryRawWithSinglePredicate,
    testSimpleQueryRaw,
    testKCNMA1DiseasePhenotypicFeat,
    testChemicalToGeneOrGeneProduct,
    testChemicalSubstanceKCNMA1,
    testDILIGeneList
  ).provideLayerShared(testLayer) @@ TestAspect.sequential

}

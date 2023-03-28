package org.renci.cam.test

import com.typesafe.scalalogging.LazyLogging
import org.renci.cam._
import org.renci.cam.domain._
import zio._
import zio.config.ZConfig
import zio.config.typesafe.TypesafeConfig
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._

/** This test suite tests that CAM-KP can handle Biolink 3 queries. Biolink 3 shifted from having single predicates to qualified predicates.
  * The examples in this file are based on the example queries provided in the GitHub issues listed in the references and inline comments
  * below.
  *
  * References:
  *   - https://github.com/biolink/biolink-model/blob/ac69bb2dc94d62d50f5cfab3fa07414b0ca092b1/Migration_3.0_Guide.md
  *   - https://github.com/biolink/biolink-model/blob/ac69bb2dc94d62d50f5cfab3fa07414b0ca092b1/predicate_mapping.yaml
  *   - https://github.com/NCATSTranslator/TranslatorArchitecture/issues/79
  *   - https://github.com/NCATSTranslator/TranslatorArchitecture/issues/80
  *   - https://github.com/NCATSTranslator/TranslatorArchitecture/issues/81
  *   - https://github.com/NCATSTranslator/TranslatorArchitecture/issues/82
  */

object Biolink3Test extends DefaultRunnableSpec with LazyLogging {

  val biolink3conversions = {
    case class ConversionTest(
      biolinkPredicates: Option[List[BiolinkPredicate]],
      trapiQualifierConstraints: Option[List[TRAPIQualifierConstraint]],
      predicates: Set[IRI]
    )

    val biolink3conversions: Seq[ConversionTest] = Seq(
      // related_to should include all possible predicates.
      ConversionTest(
        Some(List(BiolinkPredicate("related_to"))),
        None,
        Set(
          IRI("http://purl.obolibrary.org/obo/RO_0002565"),
          IRI("http://purl.obolibrary.org/obo/RO_0000057"),
          IRI("http://purl.obolibrary.org/obo/RO_0002224"),
          IRI("http://purl.obolibrary.org/obo/RO_0002087"),
          IRI("http://purl.obolibrary.org/obo/RO_0002500"),
          IRI("http://purl.obolibrary.org/obo/RO_0004007"),
          IRI("http://purl.obolibrary.org/obo/RO_0002131"),
          IRI("http://purl.obolibrary.org/obo/RO_0002436"),
          IRI("http://purl.obolibrary.org/obo/RO_0002160"),
          IRI("http://purl.obolibrary.org/obo/RO_0001025"),
          IRI("http://purl.obolibrary.org/obo/RO_0002093"),
          IRI("http://purl.obolibrary.org/obo/RO_0002356"),
          IRI("http://purl.obolibrary.org/obo/RO_0002216"),
          IRI("http://purl.obolibrary.org/obo/BFO_0000063"),
          IRI("http://purl.obolibrary.org/obo/RO_0002432"),
          IRI("http://purl.obolibrary.org/obo/BFO_0000051"),
          IRI("http://purl.obolibrary.org/obo/RO_0002349"),
          IRI("http://purl.obolibrary.org/obo/RO_0002488"),
          IRI("http://purl.obolibrary.org/obo/RO_0004034"),
          IRI("http://purl.obolibrary.org/obo/RO_0002497"),
          IRI("http://purl.obolibrary.org/obo/RO_0004033"),
          IRI("http://purl.obolibrary.org/obo/RO_0001015"),
          IRI("http://purl.obolibrary.org/obo/RO_0002434"),
          IRI("http://purl.obolibrary.org/obo/RO_0002263"),
          IRI("http://purl.obolibrary.org/obo/RO_0002496"),
          IRI("http://purl.obolibrary.org/obo/RO_0002331"),
          IRI("http://purl.obolibrary.org/obo/RO_0002608"),
          IRI("http://purl.obolibrary.org/obo/RO_0002333"),
          IRI("http://purl.obolibrary.org/obo/RO_0002298"),
          IRI("http://purl.obolibrary.org/obo/RO_0000056"),
          IRI("http://purl.obolibrary.org/obo/RO_0002229"),
          IRI("http://purl.obolibrary.org/obo/RO_0004032"),
//          IRI("http://translator.renci.org/ubergraph-axioms.ofn#acts_upstream_of_o_enabled_by"), -- this maps to a qualified term, weirdly enough.
          IRI("http://purl.obolibrary.org/obo/RO_0000052"),
          IRI("http://purl.obolibrary.org/obo/BFO_0000066"),
          IRI("http://purl.obolibrary.org/obo/RO_0002348"),
          IRI("http://purl.obolibrary.org/obo/RO_0002588"),
          IRI("http://purl.obolibrary.org/obo/BFO_0000062"),
          IRI("http://purl.obolibrary.org/obo/RO_0002211"),
          IRI("http://purl.obolibrary.org/obo/RO_0002205"),
          IRI("http://purl.obolibrary.org/obo/RO_0002604"),
          IRI("http://purl.obolibrary.org/obo/RO_0003001"),
          IRI("http://purl.obolibrary.org/obo/RO_0002448"),
          IRI("http://purl.obolibrary.org/obo/RO_0002344"),
          IRI("http://purl.obolibrary.org/obo/RO_0002232"),
          IRI("http://purl.obolibrary.org/obo/RO_0002338"),
          IRI("http://purl.obolibrary.org/obo/RO_0002592"),
          IRI("http://purl.obolibrary.org/obo/RO_0002315"),
          IRI("http://purl.obolibrary.org/obo/RO_0002234"),
          IRI("http://purl.obolibrary.org/obo/RO_0002327"),
          IRI("http://purl.obolibrary.org/obo/RO_0002492"),
          IRI("http://purl.obolibrary.org/obo/RO_0002090"),
          IRI("http://purl.obolibrary.org/obo/RO_0002412"),
          IRI("http://purl.obolibrary.org/obo/RO_0002296"),
          IRI("http://purl.obolibrary.org/obo/GOREL_0001006"),
          IRI("http://purl.obolibrary.org/obo/RO_0002230"),
          IRI("http://purl.obolibrary.org/obo/RO_0002299"),
          IRI("http://purl.obolibrary.org/obo/RO_0002264"),
          IRI("http://purl.obolibrary.org/obo/RO_0002092"),
          IRI("http://purl.obolibrary.org/obo/RO_0002084"),
          IRI("http://purl.obolibrary.org/obo/RO_0002297"),
          IRI("http://purl.obolibrary.org/obo/RO_0003000"),
          IRI("http://purl.obolibrary.org/obo/RO_0004009"),
          IRI("http://purl.obolibrary.org/obo/RO_0002223"),
          IRI("http://purl.obolibrary.org/obo/RO_0002219"),
          IRI("http://purl.obolibrary.org/obo/RO_0002339"),
          IRI("http://purl.obolibrary.org/obo/RO_0002221"),
          // IRI("http://purl.obolibrary.org/obo/RO_0002313"), -- maps to a qualified predicate
          IRI("http://purl.obolibrary.org/obo/RO_0004008"),
          IRI("http://purl.obolibrary.org/obo/RO_0002326"),
          IRI("http://purl.obolibrary.org/obo/BFO_0000050"),
          IRI("http://purl.obolibrary.org/obo/RO_0002220"),
          IRI("http://purl.obolibrary.org/obo/RO_0001019"),
          IRI("http://purl.obolibrary.org/obo/RO_0002215"),
          IRI("http://purl.obolibrary.org/obo/RO_0002231"),
          IRI("http://purl.obolibrary.org/obo/RO_0012003"),
          IRI("http://purl.obolibrary.org/obo/RO_0002411"),
          IRI("http://purl.obolibrary.org/obo/RO_0004035"),
          IRI("http://purl.obolibrary.org/obo/RO_0002590"),
          IRI("http://purl.obolibrary.org/obo/RO_0002328"),
          IRI("http://purl.obolibrary.org/obo/RO_0002233")
        )
      ),

      // biomarker_for isn't present in our triplestore, so it should return an empty set.
      ConversionTest(Some(List(BiolinkPredicate("biomarker_for"))), None, Set()),

      // "increases expression of" should be mapped to http://purl.obolibrary.org/obo/RO_0003003, since this is what
      // predicate_mapping.yaml tells us.
      ConversionTest(
        Some(List(BiolinkPredicate("affects"))),
        Some(
          List(
            TRAPIQualifierConstraint(
              qualifier_set = List(
                TRAPIQualifier("biolink:object_aspect_qualifier", "activity_or_abundance"),
                TRAPIQualifier("biolink:object_direction_qualifier", "decreased")
              )
            )
          )
        ),
        Set(IRI("http://purl.obolibrary.org/obo/RO_0002449"))
      )
    )

    suite("biolink3conversions")(
      suiteM("Test whether we can map predicates to relations") {
        ZStream
          .fromIterable(biolink3conversions)
          .map { ct: ConversionTest =>
            test(s"Testing ${ct.biolinkPredicates} with ${ct.trapiQualifierConstraints} to ${ct.predicates}") {
              assert(PredicateMappings.mapQueryEdgePredicates(ct.biolinkPredicates, ct.trapiQualifierConstraints))(
                Assertion.hasSubset(ct.predicates))
            }
          }
          .runCollect
      },
      suiteM("Make sure we can map relations to predicates") {

        ZStream
          .fromIterable(biolink3conversions)
          .flatMap { ct: ConversionTest =>
            val biolinkPreds = ct.biolinkPredicates.toList.flatten.toSet

            ZStream
              .fromIterable(ct.predicates)
              .map(
                { pred =>
                  test(s"Testing ${pred} to ${ct.biolinkPredicates} with ${ct.trapiQualifierConstraints}") {
                    val preds = PredicateMappings.getBiolinkQualifiedPredicates(pred)

                    val qualifiersActual = preds.filter(p => biolinkPreds.contains(p.biolinkPredicate)).flatMap(_.qualifierList).toSet
                    val qualifiersExpected = ct.trapiQualifierConstraints.getOrElse(List()).flatMap(_.qualifier_set).toSet

                    assert(preds.map(_.biolinkPredicate).toSet)(Assertion.hasSubset(ct.biolinkPredicates.getOrElse(List()).toSet)) &&
                    assert(qualifiersActual)(Assertion.equalTo(qualifiersExpected))
                  }
                }
              )
          }
          .runCollect
      }
    )
  }

  def generateQualifiedEdge(obj: String,
                            subj: String,
                            preds: Option[List[BiolinkPredicate]],
                            knowledge_type: Option[String],
                            qualifier_aspect: Option[String],
                            qualified_direction: Option[String]) = {
    val qualifier_constraints: List[TRAPIQualifier] = (qualifier_aspect match {
      case Some(aspect: String) => List(TRAPIQualifier(qualifier_type_id = "biolink:object_aspect_qualifier", qualifier_value = aspect))
      case _                    => List()
    }) ++ (qualified_direction match {
      case Some(direction: String) =>
        List(TRAPIQualifier(qualifier_type_id = "biolink:object_direction_qualifier", qualifier_value = direction))
      case _ => List()
    })

    TRAPIQueryEdge(
      `object` = obj,
      subject = subj,
      predicates = preds,
      knowledge_type = knowledge_type,
      qualifier_constraints = (qualifier_constraints match {
        case Seq()                      => None
        case list: List[TRAPIQualifier] => Some(List(TRAPIQualifierConstraint(qualifier_set = list)))
      })
    )
  }

  val queries = Map(
    /* Example Biolink 3 query from https://github.com/NCATSTranslator/TranslatorArchitecture/issues/79
     * using http://identifiers.org/ncbigene/2859 ("G protein-coupled receptor 35"), which produces the
     * protein http://identifiers.org/uniprot/Q9HC97.
     *
     * We know via /lookup that we know that this protein participates in http://purl.obolibrary.org/obo/GO_0007204
     * ("positive regulation of cytosolic calcium ion concentration"), so we would expect calcium ions to appear here.
     *
     * We don't have NCBIGene:2859, but we do have NCBIGene:340061 ("stimulator of interferon response cGAMP interactor 1")
     */
    "STING1-increases-chemical" -> TRAPIQueryGraph(
      nodes = Map(
        "gene" -> TRAPIQueryNode(
          ids = Some(List(IRI("http://identifiers.org/ncbigene/340061"))),
          categories = Some(List(BiolinkClass("Gene"))),
          None,
          None
        ),
        "chemical" -> TRAPIQueryNode(None, Some(List(BiolinkClass("ChemicalEntity"))), None, None)
      ),
      edges = Map(
        "t_edge" -> generateQualifiedEdge(
          "gene",
          "chemical",
          Some(List(BiolinkPredicate("affects"))),
          knowledge_type = Some("inferred"),
          qualifier_aspect = Some("activity_or_abundance"),
          qualified_direction = Some("increased")
        )
      )
    ),

    /*
     * Example Biolink 3 query from https://github.com/NCATSTranslator/TranslatorArchitecture/issues/80
     * using http://identifiers.org/ncbigene/2859 ("G protein-coupled receptor 35"), which produces the
     * protein http://identifiers.org/uniprot/Q9HC97.
     *
     * We know via /lookup that we know that this protein participates in http://purl.obolibrary.org/obo/GO_0007204
     * ("positive regulation of cytosolic calcium ion concentration"), so we would expect calcium ions to appear here.
     *
     * We don't have NCBIGene:2859, but we do have NCBIGene:598 ("BCL2 like 1")
     */
    "BCL2L1-decreases-chemical" -> TRAPIQueryGraph(
      nodes = Map(
        "gene" -> TRAPIQueryNode(
          ids = Some(List(IRI("http://identifiers.org/ncbigene/598"))),
          categories = Some(List(BiolinkClass("Gene"))),
          None,
          None
        ),
        "chemical" -> TRAPIQueryNode(None, Some(List(BiolinkClass("ChemicalEntity"))), None, None)
      ),
      edges = Map(
        "t_edge" -> generateQualifiedEdge(
          "gene",
          "chemical",
          Some(List(BiolinkPredicate("affects"))),
          knowledge_type = Some("inferred"),
          qualifier_aspect = Some("activity_or_abundance"),
          qualified_direction = Some("decreased")
        )
      )
    ),

    /*
     * Example Biolink 3 query from https://github.com/NCATSTranslator/TranslatorArchitecture/issues/81
     * using valproic acid (PUBCHEM.COMPOUND:88111 = CHEBI:39867).
     *
     * Okay, we don't seem to have this information, but we do have it for CHEBI:51143 ("nitrogen molecular entity"),
     * so let's include that instead.
     *
     */
    "pyruvate-increases-chemical" -> TRAPIQueryGraph(
      nodes = Map(
        "gene" -> TRAPIQueryNode(None, Some(List(BiolinkClass("Gene"))), None, None),
        "chemical" -> TRAPIQueryNode(ids = Some(List(IRI("http://purl.obolibrary.org/obo/CHEBI_51143"))),
                                     categories = Some(List(BiolinkClass("ChemicalEntity"))),
                                     None,
                                     None)
      ),
      edges = Map(
        "t_edge" -> generateQualifiedEdge(
          "gene",
          "chemical",
          Some(List(BiolinkPredicate("affects"))),
          knowledge_type = Some("inferred"),
          qualifier_aspect = Some("activity_or_abundance"),
          qualified_direction = Some("increased")
        )
      )
    ),

    /*
     * Example Biolink 3 query from https://github.com/NCATSTranslator/TranslatorArchitecture/issues/82
     * using valproic acid (PUBCHEM.COMPOUND:88111 = CHEBI:39867).
     *
     * (Another one we could use is pyruvate (http://purl.obolibrary.org/obo/CHEBI_15361).
     *
     * Okay, we don't seem to have this information, but we do have it for CHEBI:51143 ("nitrogen molecular entity"),
     * so let's include that instead.
     */
    "valproic-acid-decreases-chemical" -> TRAPIQueryGraph(
      nodes = Map(
        "gene" -> TRAPIQueryNode(None, Some(List(BiolinkClass("Gene"))), None, None),
        "chemical" -> TRAPIQueryNode(ids = Some(List(IRI("http://purl.obolibrary.org/obo/CHEBI_51143"))),
                                     categories = Some(List(BiolinkClass("ChemicalEntity"))),
                                     None,
                                     None)
      ),
      edges = Map(
        "t_edge" -> generateQualifiedEdge(
          "gene",
          "chemical",
          Some(List(BiolinkPredicate("affects"))),
          knowledge_type = Some("inferred"),
          qualifier_aspect = Some("activity_or_abundance"),
          qualified_direction = Some("decreased")
        )
      )
    )
  )

  /** The new Biolink3 code has an error in it -- the knowledge graph edge names don't match with the result edge names. To test for that,
    * we'll make sure that all the queries we make here have no knowledge graph edge results that are not also present in the results.
    */
  def checkKnowledgeGraphIDs(message: TRAPIMessage): TestResult = {
    val kgEdgeIds: Set[String] = message.knowledge_graph.map(_.edges.keySet).toSet.flatten
    val resultEdgeIds: Set[String] = message.results.map(_.flatMap(_.edge_bindings.values.flatMap(_.map(_.id)))).toList.flatten.toSet

    assert(kgEdgeIds)(Assertion.equalTo(resultEdgeIds))
  }

  val biolink3exampleQueries = suiteM("biolink3exampleQueries") {
    ZStream
      .fromIterable(queries)
      .map { case (testName, queryGraph) =>
        testM(f"Test example Biolink 3 query ${testName}") {
          for {
            response <- QueryService
              .run(100, queryGraph)
            _ = logger.info(s"Response: ${response}")
          } yield (
            assert(response.status)(Assertion.isSome(Assertion.equalTo("Success"))) &&
              assert(response.message.results)(Assertion.isSome(Assertion.isNonEmpty)) &&
              checkKnowledgeGraphIDs(response.message)
          )
        }
      }
      .runCollect
  }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val testLayer = HttpClient.makeHttpClientLayer ++ Biolink.makeUtilitiesLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer

  def spec = suite("Biolink 3 example queries")(
    biolink3conversions,
    biolink3exampleQueries
  ).provideCustomLayer(testLayer.mapError(TestFailure.die))

}

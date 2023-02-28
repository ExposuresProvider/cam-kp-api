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
    ConversionTest(Some(List(BiolinkPredicate("related_to"))), None, Set(IRI("https://example.org"))),
    ConversionTest(Some(List(BiolinkPredicate("biomarker_for"))), None, Set(IRI("https://example.org"))),
    ConversionTest(Some(List(BiolinkPredicate("affects"))), Some(
      List(
        TRAPIQualifierConstraint(
          qualifier_set = List(
            TRAPIQualifier("biolink:object_aspect_qualifier", "secretion"),
            TRAPIQualifier("biolink:object_direction_qualifier", "increased"),
            TRAPIQualifier("biolink:qualified_predicate", "biolink:causes")
          )
        )
      )
    ), Set(IRI("https://example.org"))
  ))


    suite("biolink3conversions")(
      suiteM("Test whether we can map predicates to relations") {
        ZStream
          .fromIterable(biolink3conversions)
          .map { ct: ConversionTest =>
            test(s"Testing ${ct.biolinkPredicates} with ${ct.trapiQualifierConstraints} to ${ct.predicates}") {
              assert(PredicateMappings.mapQueryEdgePredicates(ct.biolinkPredicates, ct.trapiQualifierConstraints))(Assertion.equalTo(ct.predicates))
            }
          }
          .runCollect
      },
      suiteM("Make sure we can map relations to predicates") {
        ZStream
          .fromIterable(biolink3conversions)
          .flatMap { ct: ConversionTest =>
            ZStream.fromIterable(ct.predicates).map({ pred =>
            test(s"Testing ${pred} to ${ct.biolinkPredicates} with ${ct.trapiQualifierConstraints}") {
              val (biolinkPred, qualifiers) = PredicateMappings.getBiolinkQualifiedPredicate(pred)
              val qualifierConstraintOpt = qualifiers match {
                case None => None
                case Some(List()) => None
                case Some(qualifiers) => Some(List(TRAPIQualifierConstraint(qualifiers)))
              }

              assert(Some(List(biolinkPred)))(Assertion.equalTo(ct.biolinkPredicates)) &&
                assert(qualifierConstraintOpt)(Assertion.equalTo(ct.trapiQualifierConstraints))
            }})
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
     */
    "GRP35-increases-chemical" -> TRAPIQueryGraph(
      nodes = Map(
        "gene" -> TRAPIQueryNode(
          ids = Some(List(IRI("http://identifiers.org/uniprot/Q9HC97"))),
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
     */
    "GRP35-decreases-chemical" -> TRAPIQueryGraph(
      nodes = Map(
        "gene" -> TRAPIQueryNode(
          ids = Some(List(IRI("http://identifiers.org/uniprot/Q9HC97"))),
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
     * (Another one we could use is pyruvate (http://purl.obolibrary.org/obo/CHEBI_15361).
     */
    "valproic-acid-increases-chemical" -> TRAPIQueryGraph(
      nodes = Map(
        "gene" -> TRAPIQueryNode(None, Some(List(BiolinkClass("Gene"))), None, None),
        "chemical" -> TRAPIQueryNode(ids = Some(List(IRI("http://purl.obolibrary.org/obo/CHEBI_39867"))),
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
     */
    "valproic-acid-decreases-chemical" -> TRAPIQueryGraph(
      nodes = Map(
        "gene" -> TRAPIQueryNode(None, Some(List(BiolinkClass("Gene"))), None, None),
        "chemical" -> TRAPIQueryNode(ids = Some(List(IRI("http://purl.obolibrary.org/obo/CHEBI_39867"))),
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
              assert(response.message.results)(Assertion.isSome(Assertion.isNonEmpty))
            )
        }
      }
      .runCollect
  }

  val configLayer: Layer[Throwable, ZConfig[AppConfig]] = TypesafeConfig.fromDefaultLoader(AppConfig.config)
  val testLayer = HttpClient.makeHttpClientLayer ++ Biolink.makeUtilitiesLayer ++ configLayer >+> SPARQLQueryExecutor.makeCache.toLayer

  def spec = suite("Biolink 3 example queries")(
    biolink3conversions
    // biolink3exampleQueries
  ).provideCustomLayer(testLayer.mapError(TestFailure.die))

}

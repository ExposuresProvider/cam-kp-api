package org.renci.cam

import com.google.common.base.CaseFormat
import contextual.Case
import org.apache.commons.lang3.StringUtils
import org.apache.jena.iri.IRIFactory
import org.apache.jena.query.{ParameterizedSparqlString, QuerySolution}
import org.apache.jena.rdf.model.{RDFNode, ResourceFactory}
import org.apache.jena.sparql.core.{Var => JenaVar}
import org.phenoscape.sparql.FromQuerySolution
import org.phenoscape.sparql.SPARQLInterpolation.SPARQLInterpolator
import org.phenoscape.sparql.SPARQLInterpolation.SPARQLInterpolator.SPARQLContext
import sttp.tapir.Schema
import sttp.tapir.generic.auto._

import scala.util.Try

package object domain {

  final case class IRI(value: String)

  object IRI {

    implicit val embedInSPARQL = SPARQLInterpolator.embed[IRI](Case(SPARQLContext, SPARQLContext) { iri =>
      val pss = new ParameterizedSparqlString()
      pss.appendIri(iri.value)
      pss.toString
    })

    implicit object IRIFromQuerySolution extends FromQuerySolution[IRI] {

      def fromQuerySolution(qs: QuerySolution, variablePath: String = ""): Try[IRI] =
        getResource(qs, variablePath).map(r => IRI(r.getURI))

    }

    /** An implicit method for converting an IRI into an RDFNode.
      *
      * It's probably good enough to treat iri.value as a URI, but _just in case_ we use Jena's IRIFactory to convert it into an URI first.
      *
      * @param iri
      *   The IRI to convert to an RDFNode.
      * @return
      *   The RDFNode representing the provided IRI.
      */
    implicit def toNode(iri: IRI): RDFNode = ResourceFactory.createResource(
      IRIFactory.iriImplementation().construct(iri.value).toURI.toString
    )

    implicit val schema: Schema[IRI] = Schema.string

  }

  final case class Var(label: String)

  object Var {

    implicit val embedInSPARQL = SPARQLInterpolator.embed[Var](Case(SPARQLContext, SPARQLContext) { variable =>
      val pss = new ParameterizedSparqlString()
      pss.appendNode(JenaVar.alloc(variable.label))
      pss.toString
    })

  }

  sealed trait BiolinkTerm {

    def shorthand: String

    def iri: IRI

  }

  final case class BiolinkClass(shorthand: String, iri: IRI) extends BiolinkTerm

  object BiolinkClass {

    def apply(label: String): BiolinkClass =
      if (label.contains("_")) {
        BiolinkClass(label, IRI(s"${BiolinkTerm.namespace}${CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, label)}"))
      } else {
        BiolinkClass(label, IRI(s"${BiolinkTerm.namespace}${StringUtils.capitalize(label)}"))
      }

    implicit lazy val schema: Typeclass[BiolinkClass] = Schema.string

  }

  implicit class BiolinkClassOps(blClass: BiolinkClass) {
    def withBiolinkPrefix = s"biolink:${blClass.shorthand}"
  }

  final case class BiolinkPredicate(shorthand: String, iri: IRI) extends BiolinkTerm

  object BiolinkPredicate {

    implicit val embedInSPARQL = SPARQLInterpolator.embed[BiolinkPredicate](Case(SPARQLContext, SPARQLContext) { pred =>
      val pss = new ParameterizedSparqlString()
      pss.appendIri(pred.iri.value)
      pss.toString
    })

    implicit object BiolinkPredicateFromQuerySolution extends FromQuerySolution[BiolinkPredicate] {

      def fromQuerySolution(qs: QuerySolution, variablePath: String = ""): Try[BiolinkPredicate] =
        getResource(qs, variablePath).map(r => BiolinkPredicate(r.getURI))

    }

    def apply(label: String): BiolinkPredicate =
      if (!label.startsWith(BiolinkTerm.namespace)) {
        BiolinkPredicate(label, IRI(s"${BiolinkTerm.namespace}$label"))
      } else {
        BiolinkPredicate(label.replace(BiolinkTerm.namespace, ""), IRI(s"$label"))
      }

  }

  implicit class BiolinkPredicateOps(blPredicate: BiolinkPredicate) {
    def withBiolinkPrefix = s"biolink:${blPredicate.shorthand}"
  }

  object BiolinkTerm {

    val namespace: String = "https://w3id.org/biolink/vocab/"

  }

  final case class TRAPIQueryNode(ids: Option[List[IRI]],
                                  categories: Option[List[BiolinkClass]],
                                  is_set: Option[Boolean] = Some(false),
                                  constraints: List[TRAPIAttributeConstraint] = List())

  final case class TRAPIQueryEdge(predicates: Option[List[BiolinkPredicate]],
                                  subject: String,
                                  `object`: String,
                                  knowledge_type: Option[String] = None,
                                  attribute_constraints: Option[List[TRAPIAttributeConstraint]],
                                  qualifier_constraints: Option[List[TRAPIQualifierConstraint]])

  final case class TRAPIAttributeConstraint(id: IRI,
                                        name: String,
                                        not: Option[Boolean],
                                        operator: String,
                                        value: String,
                                        unit_id: Option[IRI],
                                        unit_name: Option[String])

  final case class TRAPIQueryGraph(nodes: Map[String, TRAPIQueryNode], edges: Map[String, TRAPIQueryEdge])

  final case class TRAPINode(name: Option[String], categories: Option[List[BiolinkClass]], attributes: Option[List[TRAPIAttribute]])

  final case class TRAPIEdge(predicate: Option[BiolinkPredicate],
                             subject: IRI,
                             `object`: IRI,
                             attributes: Option[List[TRAPIAttribute]],
                             qualifiers: Option[List[TRAPIQualifier]])

  final case class TRAPIAttribute(attribute_source: Option[String],
                                  attribute_type_id: IRI,
                                  original_attribute_name: Option[String],
                                  // Note that `value` is actually supposed to be able to support
                                  // any data type, including lists.
                                  // https://github.com/NCATSTranslator/ReasonerAPI/blob/7520ac564e63289dffe092d4c7affd6db4ba22f1/TranslatorReasonerAPI.yaml#L761-L764
                                  // Not sure if a List[String] is close enough to read attributes here.
                                  value: List[String],
                                  value_type_id: Option[IRI],
                                  value_url: Option[String],
                                  description: Option[String],
                                  attributes: Option[List[TRAPIAttribute]])

  final case class TRAPIQualifier(qualifier_type_id: String, qualifier_value: String)

  final case class TRAPIQualifierConstraint(qualifier_set: List[TRAPIQualifier])

  final case class TRAPIKnowledgeGraph(nodes: Map[IRI, TRAPINode], edges: Map[String, TRAPIEdge])

  object TRAPIKnowledgeGraph {

    // FIXME IRI needs to be shortened
    implicit lazy val nodesSchema: Schema[Map[IRI, TRAPINode]] = Schema.schemaForMap(_.value)

    implicit lazy val schema: Schema[TRAPIKnowledgeGraph] = Schema.derived

  }

  final case class TRAPINodeBinding(id: IRI, query_id: Option[IRI] = None, attributes: Option[List[TRAPIAttribute]] = None)

  final case class TRAPIEdgeBinding(id: String)

  final case class TRAPIResult(node_bindings: Map[String, List[TRAPINodeBinding]], edge_bindings: Map[String, List[TRAPIEdgeBinding]])

  final case class TRAPIMessage(query_graph: Option[TRAPIQueryGraph],
                                knowledge_graph: Option[TRAPIKnowledgeGraph],
                                results: Option[List[TRAPIResult]])

  final case class TRAPIQuery(message: TRAPIMessage, log_level: Option[String], submitter: Option[String])

  final case class TRAPIResponse(message: TRAPIMessage, status: Option[String], description: Option[String], logs: Option[List[LogEntry]])

  final case class LogEntry(timestamp: Option[String], level: Option[String], code: Option[String], message: Option[String])

  final case class MetaNode(id_prefixes: List[String], attributes: Option[List[MetaAttribute]])

  final case class MetaEdge(subject: BiolinkClass,
                            predicate: BiolinkPredicate,
                            `object`: BiolinkClass,
                            attributes: Option[List[MetaAttribute]],
                            knowledge_types: Option[List[String]] = Some(List("lookup")))

  final case class MetaAttribute(attribute_type_id: IRI,
                                 attribute_source: Option[String],
                                 original_attribute_names: Option[List[String]],
                                 constraint_use: Option[Boolean],
                                 constraint_name: Option[String])

//  final case class MetaKnowledgeGraph(nodes: Map[BiolinkClass, Map[String, List[String]]], edges: List[MetaEdge])
  final case class MetaKnowledgeGraph(nodes: Map[BiolinkClass, MetaNode], edges: List[MetaEdge])

  object MetaKnowledgeGraph {

    implicit lazy val nodesSchema: Schema[Map[BiolinkClass, MetaNode]] = Schema.schemaForMap(_.withBiolinkPrefix)

    implicit lazy val schema: Schema[MetaKnowledgeGraph] = Schema.derived

  }

}

package org.renci.cam

import com.google.common.base.CaseFormat
import contextual.Case
import org.apache.commons.lang3.StringUtils
import org.apache.jena.query.{ParameterizedSparqlString, QuerySolution}
import org.apache.jena.sparql.core.{Var => JenaVar}
import org.phenoscape.sparql.FromQuerySolution
import org.phenoscape.sparql.SPARQLInterpolation.SPARQLInterpolator
import org.phenoscape.sparql.SPARQLInterpolation.SPARQLInterpolator.SPARQLContext

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

  }

  implicit class BiolinkClassOps(blClass: BiolinkClass) {
    def withBiolinkPrefix = s"biolink:${blClass.shorthand}"
  }

  final case class BiolinkPredicate(shorthand: String, iri: IRI) extends BiolinkTerm

  object BiolinkPredicate {

    def apply(label: String): BiolinkPredicate = BiolinkPredicate(label, IRI(s"${BiolinkTerm.namespace}$label"))

  }

  implicit class BiolinkPredicateOps(blPredicate: BiolinkPredicate) {
    def withBiolinkPrefix = s"biolink:${blPredicate.shorthand}"
  }

  object BiolinkTerm {

    val namespace: String = "https://w3id.org/biolink/vocab/"

  }

  final case class TRAPIQueryNode(id: Option[IRI], category: Option[BiolinkClass], is_set: Option[Boolean])

  final case class TRAPIQueryEdge(subject: String, `object`: String, predicate: Option[BiolinkPredicate], relation: Option[String])

  final case class TRAPIQueryGraph(nodes: Map[String, TRAPIQueryNode], edges: Map[String, TRAPIQueryEdge])

  final case class TRAPINode(name: Option[String], category: Option[List[BiolinkClass]], attributes: Option[List[TRAPIAttribute]])

  final case class TRAPIEdge(subject: IRI,
                             `object`: IRI,
                             relation: Option[String],
                             predicate: Option[BiolinkPredicate],
                             attributes: Option[List[TRAPIAttribute]])

  final case class TRAPIAttribute(name: Option[String], value: String, `type`: IRI,  url: Option[String])

  final case class TRAPIKnowledgeGraph(nodes: Map[String, TRAPINode], edges: Map[String, TRAPIEdge])

  final case class TRAPINodeBinding(id: IRI)

  final case class TRAPIEdgeBinding(id: String, provenance: Option[String])

  final case class TRAPIResult(node_bindings: Map[String, TRAPINodeBinding],
                               edge_bindings: Map[String, TRAPIEdgeBinding])

  final case class TRAPIMessage(query_graph: Option[TRAPIQueryGraph],
                                knowledge_graph: Option[TRAPIKnowledgeGraph],
                                results: Option[List[TRAPIResult]])

  final case class TRAPIQuery(message: TRAPIMessage)

  final case class TRAPIResponse(message: TRAPIMessage, status: Option[String], description: Option[String], logs: Option[List[LogEntry]])

  final case class LogEntry(timestamp: Option[String], level: Option[String], code: Option[String], message: Option[String])

}

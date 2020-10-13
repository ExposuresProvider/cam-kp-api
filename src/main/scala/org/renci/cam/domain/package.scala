package org.renci.cam

import io.circe.Decoder.Result
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor}
import org.apache.commons.text.CaseUtils
import org.apache.jena.ext.com.google.common.base.CaseFormat

package object domain {

  final case class IRI(value: String)

  object IRI {

    private val Curie = "^([^:]*):(.*)$".r

    def makeDecoder(prefixesMap: Map[String, String]): Decoder[IRI] = new Decoder[IRI] {

      private val protocols = Set("http", "https", "ftp", "file", "mailto")

      override def apply(c: HCursor): Result[IRI] = for {
        value <- c.value.as[String]
        Curie(prefix, local) = value
        namespace <-
          if (protocols(prefix)) Right(prefix)
          else prefixesMap.get(prefix).toRight(DecodingFailure(s"No prefix expansion found for $prefix:$local", Nil))
      } yield IRI(s"$namespace$local")

    }

    def makeEncoder(prefixesMap: Map[String, String]): Encoder[IRI] = Encoder.encodeString.contramap { iri =>
      val startsWith = prefixesMap.filter { case (_, namespace) => iri.value.startsWith(namespace) }
      if (startsWith.nonEmpty) {
        val (prefix, namespace) = startsWith.maxBy(_._2.length)
        s"$prefix:${iri.value.drop(namespace.length)}"
      } else iri.value
    }

  }

  sealed trait BiolinkTerm {

    def shorthand: String

    def iri: IRI

  }

  final case class BiolinkClass(shorthand: String, iri: IRI) extends BiolinkTerm

  object BiolinkClass {

    def apply(label: String): BiolinkClass = {
      val underscoreLabel = label.replaceAllLiterally(" ", "_")
      val camelLabel = CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, underscoreLabel)
      BiolinkClass(camelLabel, IRI(s"${BiolinkTerm.namespace}$camelLabel"))
    }

    //FIXME would be good to check that this is a known Biolink term rather than just accepting
    implicit val decoder: Decoder[BiolinkClass] = Decoder.decodeString.map { s =>
      val local = CaseUtils.toCamelCase(s, true, '_')
      BiolinkClass(s, IRI(s"${BiolinkTerm.namespace}$local"))
    }

    implicit val encoder: Encoder[BiolinkClass] = Encoder.encodeString.contramap(blTerm => blTerm.shorthand)

  }

  final case class BiolinkPredicate(shorthand: String, iri: IRI) extends BiolinkTerm

  object BiolinkPredicate {

    def apply(label: String): BiolinkPredicate = {
      val underscoreLabel = label.replaceAllLiterally(" ", "_")
      BiolinkPredicate(underscoreLabel, IRI(s"${BiolinkTerm.namespace}$underscoreLabel"))
    }

    //FIXME would be good to check that this is a known Biolink term rather than just accepting
    implicit val decoder: Decoder[BiolinkPredicate] = Decoder.decodeString.map { s =>
      BiolinkPredicate(s, IRI(s"${BiolinkTerm.namespace}$s"))
    }

    implicit val encoder: Encoder[BiolinkPredicate] = Encoder.encodeString.contramap(blTerm => blTerm.shorthand)

  }

  object BiolinkTerm {

    val namespace: String = "https://w3id.org/biolink/vocab/"

  }

  case class TRAPIQueryNode(id: String, `type`: Option[BiolinkClass], curie: Option[IRI])

  case class TRAPIQueryEdge(id: String, source_id: String, target_id: String, `type`: Option[BiolinkPredicate])

  case class TRAPIQueryGraph(nodes: List[TRAPIQueryNode], edges: List[TRAPIQueryEdge])

  case class TRAPINode(id: String, name: Option[String], `type`: List[BiolinkClass])

  case class TRAPIEdge(id: String, source_id: IRI, target_id: IRI, `type`: Option[BiolinkPredicate])

  case class TRAPIKnowledgeGraph(nodes: List[TRAPINode], edges: List[TRAPIEdge])

  case class TRAPINodeBinding(qg_id: Option[String], kg_id: String)

  case class TRAPIEdgeBinding(qg_id: Option[String], kg_id: String, provenance: Option[String])

  case class TRAPIResult(node_bindings: List[TRAPINodeBinding],
                         edge_bindings: List[TRAPIEdgeBinding],
                         extra_nodes: Option[List[TRAPINodeBinding]],
                         extra_edges: Option[List[TRAPIEdgeBinding]])

  case class TRAPIMessage(query_graph: Option[TRAPIQueryGraph],
                          knowledge_graph: Option[TRAPIKnowledgeGraph],
                          results: Option[List[TRAPIResult]])

  case class TRAPIQueryRequestBody(message: TRAPIMessage)

}

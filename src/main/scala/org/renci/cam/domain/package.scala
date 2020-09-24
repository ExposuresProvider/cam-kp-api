package org.renci.cam

import org.renci.cam.Implicits.readPrefixes

package object domain {

  case class CURIEorIRI(prefix: Option[String], reference: String)

  case class TRAPIQueryNode(id: String, `type`: Option[CURIEorIRI], curie: Option[CURIEorIRI])

  case class TRAPIQueryEdge(id: String, source_id: String, target_id: String, `type`: Option[CURIEorIRI])

  case class TRAPIQueryGraph(nodes: List[TRAPIQueryNode], edges: List[TRAPIQueryEdge])

  case class TRAPINode(id: String, name: Option[String], `type`: List[String])

  case class TRAPIEdge(id: String, source_id: CURIEorIRI, target_id: CURIEorIRI, `type`: Option[CURIEorIRI])

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

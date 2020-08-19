package org.renci.cam

package object domain {

  case class TRAPIQueryNode(id: String, `type`: Option[String], curie: Option[String])

  case class TRAPIQueryEdge(id: String, source_id: String, target_id: String, `type`: Option[String])

  case class TRAPIQueryGraph(nodes: List[TRAPIQueryNode], edges: List[TRAPIQueryEdge])

  case class TRAPINodeAttribute(name: Option[String], value: String, `type`: String, url: Option[String], source: Option[String])

  case class TRAPINode(name: Option[String], `type`: List[String], attributes: List[TRAPINodeAttribute])

  case class TRAPIEdge(id: String, source_id: String, target_id: String, `type`: Option[String])

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

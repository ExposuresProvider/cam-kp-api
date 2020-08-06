package org.renci.cam

package object domain {

  case class TranslatorQueryNode(id: String, `type`: String, curie: Option[String])

  case class TranslatorQueryEdge(id: String, `type`: String, source_id: String, target_id: String)

  case class TranslatorQueryGraph(nodes: List[TranslatorQueryNode], edges: List[TranslatorQueryEdge])

  case class TranslatorNodeAttribute(name: Option[String], value: String, `type`: String, url: Option[String], source: Option[String])

  case class TranslatorNode(name: Option[String], `type`: List[String], attributes: List[TranslatorNodeAttribute])

  case class TranslatorEdge(id: String, `type`: Option[String], source_id: String, target_id: String)

  case class TranslatorKnowledgeGraph(nodes: List[TranslatorNode], edges: List[TranslatorEdge])

  case class TranslatorNodeBinding(qg_id: Option[String], kg_id: String)

  case class TranslatorEdgeBinding(qg_id: Option[String], kg_id: String, provenance: Option[String])

  case class TranslatorResult(node_bindings: List[TranslatorNodeBinding],
                              edge_bindings: List[TranslatorEdgeBinding],
                              extra_nodes: Option[List[TranslatorNodeBinding]],
                              extra_edges: Option[List[TranslatorEdgeBinding]])

  case class TranslatorMessage(query_graph: Option[TranslatorQueryGraph],
                               knowledge_graph: Option[TranslatorKnowledgeGraph],
                               results: List[TranslatorResult])

  case class TranslatorQueryRequestBody(message: TranslatorMessage)

}

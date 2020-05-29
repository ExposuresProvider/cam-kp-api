package org.renci.cam

final case class KGSQueryGraph(nodes: List[KGSNode], edges: List[KGSEdge])

final case class KGSNode(id: String, `type`: String, curie: Option[String])

final case class KGSEdge(id: String, source_id: String, target_id: String, `type`: String)

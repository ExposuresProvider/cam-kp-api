package org.renci.cam

package object domain {

  case class KGSQueryGraph(nodes: List[KGSNode], edges: List[KGSEdge])

  case class KGSNode(id: String, `type`: String, curie: Option[String])

  case class KGSEdge(id: String, source_id: String, target_id: String, `type`: String)

}

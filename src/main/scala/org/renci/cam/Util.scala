package org.renci.cam

import org.phenoscape.sparql.SPARQLInterpolation._

object Util {

  implicit class IterableSPARQLOps[A: SPARQLEmbedder](val self: Iterable[A]) {

    def asValues: QueryText = self.map(item => sparql" $item ").fold(sparql"")(_ + _)

  }

}

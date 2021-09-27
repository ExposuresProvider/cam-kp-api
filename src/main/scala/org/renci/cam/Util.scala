package org.renci.cam

import org.phenoscape.sparql.SPARQLInterpolation._

object Util {

  implicit class IterableSPARQLOps[A: SPARQLEmbedder](val self: Iterable[A]) {

    def asValues: QueryText = self.map(item => sparql" $item ").fold(sparql"")(_ + _)

    def asSPARQLList: QueryText =
      interpolate(sparql", ", self.to(List).map(item => sparql"$item")).fold(sparql"")(_ + _)

  }

  def interpolate[T](elem: T, list: List[T]): List[T] = list match {
    case Nil             => Nil
    case last @ _ :: Nil => last
    case x :: xs         => x :: elem :: interpolate(elem, xs)
  }

}

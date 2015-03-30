// nnsearch
package nnsearch.search {

import nnsearch.collections.immutable._
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.TreeMap

object NearestSearch {

  type Searcher = Stream[Variant] => Stream[Variant]

  // we make this 2 case classes to be available to write test
  case class K(k: Int) extends Searcher {
    override def apply(s: Stream[Variant]) = s take k
  }

  case class D(d: Int) extends Searcher {
    override def apply(s: Stream[Variant]) = s takeWhile (v => v.penalty < d)
  }

  def apply(trie: Trie, toFind: String, limit: Int = 10000): Seq[(String, Int)] =
    kNearest(trie, toFind, 1, limit)

  def search(
    trie: Trie,
    toFind: String,
    searcher: Searcher,
    limit: Int = 10000): Seq[(String, Int)] =
  {
    searcher(
      trie prefixes toFind take limit filter (
        v => v.node.ends && v.pos == toFind.length)) map (
      v => ((v.node makeString, v.penalty)))
  }

  def kNearest(trie: Trie, toFind: String, k: Int, limit: Int = 10000) =
    search(trie, toFind, K(k), limit)

  def dNearest(trie: Trie, toFind: String, d: Int, limit: Int = 10000) =
    search(trie, toFind, D(d), limit)
}

} // !package nnsearch

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

  // TODO: this operation should be performed before whole word filter applied
  case class D(d: Int) extends Searcher {
    override def apply(s: Stream[Variant]) = s takeWhile (v => v.penalty < d)
  }

  def apply(trie: Trie, toFind: String, limit: Int = 10000): Seq[(String, Int)] =
    kNearest(trie, toFind, 1, limit)

  /**
    * Is provided variant matches the whole word in dictionary and whole
    * word to search ('toFind')
    */
  private def isWholeWord(toFind: String)(v: Variant) = 
    v.node.ends && v.pos == toFind.length

  /**
    * Filters and transforms prefix matches to a sequence of nearest words, marked
    * by total penalty
    */
  def search(
    trie: Trie,
    toFind: String,
    searcher: Searcher,
    limit: Int = 10000): Seq[(String, Int)] =
  {
    searcher(
      trie prefixes toFind take limit filter isWholeWord(toFind)) map (
        v => ((v.node makeString, v.penalty)))
  }

  def kNearest(trie: Trie, toFind: String, k: Int, limit: Int = 10000) =
    search(trie, toFind, K(k), limit)

  def dNearest(trie: Trie, toFind: String, d: Int, limit: Int = 10000) =
    search(trie, toFind, D(d), limit)
}

} // !package nnsearch

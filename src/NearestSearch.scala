// nnsearch
package nnsearch {

import nnsearch.collections.immutable._
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.TreeMap

object NearestSearch {

  /// consumer of variants during nearest neighbour searching
  abstract class Consumer {
    def take(s: String, v: Variant) : Boolean
  }

  /// an interface for searching object, passed to NearestSearch
  trait Searcher extends Consumer {
    def get(): Seq[(String, Int)] = buffer
    protected val buffer = ArrayBuffer[(String, Int)]()

    def wholeWordMatch(toFind: String, v: Variant): Option[String] =
      if (!v.node.ends || toFind.length != v.pos) None
      else Some(v.node.makeString)
  }

  /// search for k nearest neighbours
  case class K(k: Int) extends Searcher
  {
    override def take(toFind: String, v: Variant): Boolean = {
      val value = wholeWordMatch(toFind, v).getOrElse("")
      if (value.isEmpty) return true
      buffer += ((value, v.penalty))
      return buffer.size < k
    }
  }

  /// search for neighbours in d-radius sphere
  case class Delta(d: Int) extends Searcher
  {
    override def take(toFind: String, v: Variant): Boolean = {
      if (v.penalty >= d) return false
      val value = wholeWordMatch(toFind, v).getOrElse("")
      if (value.isEmpty) return true
      buffer += ((value, v.penalty))
      return true
    }
  }

  /// general search
  def apply(trie: Trie, toFind: String, out : (String, Variant) => Boolean) =
    trie.nearest(toFind, out)

  /// general search v2
  def apply(trie: Trie, toFind: String, searcher: Searcher) = 
  {
    trie.nearest(toFind, searcher.take)
    searcher.get()
  }

  /// search for 1 nearest
  def apply(trie: Trie, toFind: String): Seq[(String, Int)] = 
    apply(trie, toFind, new K(1))

  /// search for k nearest
  def kNearest(trie: Trie, toFind: String, k: Int) = 
    apply(trie, toFind, new K(k))

  /// search for neighbours in radius [0, d)
  def dNearest(trie: Trie, toFind: String, d: Int) =
    apply(trie, toFind, new Delta(d))
}

} // !package nnsearch

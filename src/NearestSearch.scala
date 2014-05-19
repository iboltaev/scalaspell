// nnsearch
package nnsearch.search {

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

package unittest {
  import org.scalatest._
  import nnsearch.collections._
  import nnsearch.search._

  class NearestSearchSpec extends FlatSpec with Matchers {
    "A trie & NearestSearch" should "be capable of impresize search" in {
      val trie = immutable.Trie(Seq("111", "1111", "222", "333"))
      NearestSearch(trie, "111") should be (Seq(("111", 0)))
      NearestSearch(trie, "121") should be (Seq(("111", 1)))
      NearestSearch(trie, "122") should be (Seq(("222", 1)))
      NearestSearch(trie, "33") should be (Seq(("333", 1)))
      NearestSearch(trie, "221") should be (Seq(("222", 1)))
      NearestSearch(trie, "01") should be (Seq(("111", 2)))
      NearestSearch(trie, "10") should be (Seq(("111", 2)))
      NearestSearch(trie, "1") should be (Seq(("111", 2)))
      NearestSearch(trie, "02") should be (Seq(("222", 2)))
      NearestSearch(trie, "20") should be (Seq(("222", 2)))
      NearestSearch(trie, "2") should be (Seq(("222", 2)))
      NearestSearch(trie, "03") should be (Seq(("333", 2)))
      NearestSearch(trie, "30") should be (Seq(("333", 2)))
      NearestSearch(trie, "3") should be (Seq(("333", 2)))
    }

    it should "be capable of k-nearest searching" in {
      val trie = immutable.Trie(Seq("111", "1111", "222", "333"))
      NearestSearch.kNearest(trie, "112", 3) should contain theSameElementsAs Seq(("111", 1), ("1111", 2), ("222", 2))
      NearestSearch.kNearest(trie, "221", 2) should contain theSameElementsAs Seq(("222", 1), ("111", 2))
    }

    it should "be capable of delta-nearest searching" in {
      val trie = immutable.Trie(Seq("111", "1111", "222", "333"))
      NearestSearch.dNearest(trie, "112", 2) should equal (Seq(("111", 1)))
      NearestSearch.dNearest(trie, "112", 3) should contain theSameElementsAs Seq(("111", 1), ("222", 2), ("1111", 2))
    }

    it should "correctly process bug with end position insertion" in {
      val trie = immutable.Trie(
	Seq("sacrifice", "sacrificed", "sacrifices"))
      NearestSearch.dNearest(trie, "sacrifice", 2) should contain theSameElementsAs Seq(("sacrifice", 0), ("sacrificed", 1), ("sacrifices", 1))
    }
  }
}

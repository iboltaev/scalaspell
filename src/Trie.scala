// nnsearch.collections.immutable
package nnsearch.collections.immutable {

import scala.annotation.tailrec

import scala.collection.immutable.TreeMap

import scala.collection.mutable.HashSet
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.StringBuilder

import scala.util.Sorting

/// nearest search variant
case class Variant(
  val penalty: Int,
  val pos: Int,
  val node: Trie)
{
  override def hashCode() : Int = pos ^ node.hashCode()
  override def equals(v: Any) : Boolean = v match {
    case that: Variant => 
      this.pos == that.pos && 
      (this.node eq that.node)
      case _ => false
  }
}

object VariantOrder extends Ordering[Variant] 
{
  def compare(v1: Variant, v2: Variant) : Int = 
    if (v1.penalty == v2.penalty) v1.pos compare v2.pos
    else v2.penalty compare v1.penalty
}

  /**
 * Just immutable trie (also called prefix tree).
 * Capable of running general impresize search in a dictionary
 * (in terms of Levenstein metric).
 * Algorithm is greedy search in variations graph. No overhead
 * on correct data, if 1-2 error - also pretty fast.
 * More errors - significantly slow, because appropriate variants count
 * grow is near-exponential
 */
class Trie(
  val ends: Boolean = false, // whether this node is end of some string
  private val parent: Trie = null,
  private val childs : TreeMap[Char, Trie] = null,
  private val value : Char = 0x0)
{
  def +(s: String) = insert(s, 0)

  /// exact search
  def contains(s: String) =
    prefixImpl(this, s, 0) match {
      case Some(a: Trie) => a.ends
      case _ => false
    }

  def makeString() : String = 
  {
    var sb = new StringBuilder()
    var t = this
    while (t != null && t.parent != null) {
      sb.append(t.value)
      t = t.parent
    }
    return sb.result().reverse.toString()
  }

  /// nearest search with Levenstein metric; breaks if "consume" is false
  def nearest(
    str: String,
    consume : (String, Variant) => Boolean): Unit = 
  {
    def genvars(str: String,
	      best: Variant,
	      q: PriorityQueue[Variant]) = 
    {
      // eat symbol
      if (best.pos != str.length)
	q += Variant(best.penalty + 1, best.pos + 1, best.node)

      if (best.node.childs != null) {
	for ((key, child) <- best.node.childs) {
	  // symbol replacement
	  if (best.pos != str.length)
	    q += Variant(if (str(best.pos) == key) best.penalty 
			 else best.penalty + 1,
			 
			 best.pos + 1,
			 child)

	  // pass symbol
	  q += Variant(best.penalty + 1, best.pos, child)
	}
      }
    }

    def indeep(toFind: String,
	       variant: Variant,
	       consume: (String, Variant) => Boolean,
	       cache: HashSet[Variant],
	       queue: PriorityQueue[Variant]): Boolean = 
    {
      if (variant.node == null || 
	  toFind.length < variant.pos ||
	  cache.contains(variant))
	return false

      if (!consume(toFind, variant))
	return true

      cache += variant

      if (toFind.length == variant.pos) {
	genvars(toFind, variant, queue)
	return false
      }

      val c = toFind(variant.pos)
      if (variant.node.childs != null &&
	  variant.node.childs.contains(c)) 
      {
	val v = Variant(variant.penalty, variant.pos + 1,
			variant.node.childs(c))
	if (indeep(toFind, v, consume, cache, queue))
	  return true
      }

      genvars(toFind, variant, queue)

      return false
    }

    var q = PriorityQueue[Variant]()(VariantOrder)
    var cache = HashSet[Variant]()
    val start = Variant(0, 0, this)
    q += start

    while (q.size != 0) {
      val best = q.dequeue()

      if (indeep(str, best, consume, cache, q))
	return

      cache += best
    }
  }

  private def prefixImpl(t: Trie, s: String, pos: Int): Option[Trie] =
    if (s.length < pos) None
    else if (s.length == pos) Some(t)
    else if (t.childs == null) None
    else if (t.childs.contains(s(pos)) == false) None
    else prefixImpl(t.childs(s(pos)), s, pos + 1)

  private def insert(s: String, pos: Int = 0) : Trie = 
    if (s.length() < pos) this
    else if (contains(s)) this
    else if (s.length() == pos)
	if (ends) this
	else new Trie(true, parent, childs, value)
    else {
      val c = s(pos)
      val children = if (childs != null) childs else new TreeMap[Char, Trie]()
      val child = if (children contains c) children(c) 
		  else new Trie(s.length() == pos + 1, this, null, c)
      new Trie(ends, parent, children + ((c, child.insert(s, pos + 1))), value)
    }
}

object Trie {
  def apply(seq: Iterator[String]) : Trie = 
    (new Trie() /: seq.filter(! _.isEmpty))(_ + _)

  def apply(seq: Seq[String]) : Trie = apply(seq.iterator)
}

} // !package nnsearch.collections.immutable

package unittest {
  import org.scalatest._
  import nnsearch.collections._
  import nnsearch.NearestSearch

  class NearestSearchSpec extends FlatSpec with Matchers {
    "A trie" should "be capable of precise searching" in {
      val trie = immutable.Trie(Seq("111", "1111", "222", "333"))
      trie contains "111" should be (true)
      trie contains "1111" should be (true)
      trie contains "222" should be (true)
      trie contains "333" should be (true)
      trie contains "345" should be (false)
    }

    it should "be capable of impresize search" in {
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

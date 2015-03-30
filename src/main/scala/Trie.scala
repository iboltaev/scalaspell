package nnsearch.collections.immutable

import nnsearch.Utils._

import scala.annotation.tailrec

import scala.collection.immutable.TreeMap
import scala.collection.immutable.TreeSet
import scala.collection.immutable.Stream

import scala.collection.mutable.StringBuilder

import scala.util.Sorting

/// nearest search variant
case class Variant(
  val penalty: Int,
  val pos: Int,
  val node: Trie)
{
  // analogous to C++'s boost::hash_combine
  override def hashCode() : Int = 
    pos ^ (node.hashCode() + 0x9e3779b9 + (pos << 6) + (pos >> 2))

  override def equals(v: Any) : Boolean = v match {
    case that: Variant => 
      pos == that.pos && 
      (node eq that.node)
    case _ => false
  }
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
  val value : Char = 0x0)
{
  def +(s: String) = {
    def insert(trie: Trie, s: String, pos: Int = 0) : Trie = 
      if (s.length() < pos) trie
      else if (pos == 0 && trie.contains(s)) trie
      else if (s.length == pos)
	if (trie.ends) trie
	else new Trie(true, trie.parent, trie.childs, trie.value)
      else {
	val c = s(pos)
	val children = if (trie.childs != null) trie.childs 
		       else new TreeMap[Char, Trie]()
	val child = if (children contains c) children(c) 
		    else new Trie(s.length() == pos + 1, trie, null, c)
	new Trie(trie.ends, trie.parent, 
		 children + ((c, insert(child, s, pos + 1))), trie.value)
      }

    insert(this, s, 0)
  }

  /// exact search
  def contains(s: String) = {
    @tailrec def impl(t: Trie, s: String, pos: Int): Boolean =
      if (s.length < pos) false
      else if (s.length == pos) true
      else if (t.childs == null) false
      else if (t.childs.contains(s(pos)) == false) false
      else impl(t.childs(s(pos)), s, pos + 1)

    impl(this, s, 0)
  }

  def makeString() : String = {
    @tailrec def helper(t: Trie, sb: StringBuilder): String =
      if (t == null || t.parent == null) sb.result.reverse
      else {
	sb += t.value
	helper(t.parent, sb)
      }

    helper(this, new StringBuilder)
  }

  private case class Context(val q: TreeSet[Variant], val cache: TreeSet[Variant]) {
    def pop = {
      if (q.isEmpty) ((None, this))
      else {
        val v = q.firstKey
        ((Some(v), Context(q - v, cache)))
      }
    }

    def ++(vars: Seq[Variant]) = Context(q ++ vars, cache)

    def apply(v: Variant) = cache(v)
    def addCache(v: Variant) = Context(q, cache + v)
  }

  def prefixes(toFind: String) : Stream[Variant] = {
    implicit val ordering: Ordering[Variant] = Ordering by {
      (v) => (v.penalty, v.pos, v.node.value)}

    val init = Variant(0, 0, this)

    @tailrec def whileCached(ctx: Context): (Option[Variant], Context) =
    {
      val (v, ctx2) = ctx.pop
      if (v.isEmpty) ((v, ctx2))
      else if (!ctx2(v.get)) ((Some(v.get), ctx2))
      else whileCached(ctx2)
    }

    def genvars(v: Variant): List[Variant] = {
      val replacePass: List[Variant] = 
        if (v.node.childs == null) Nil
        else v.node.childs.toList flatMap(pair => {
          val (key, child) = pair
          val pass = Variant(v.penalty + 1, v.pos, child) :: Nil
          if (v.pos < toFind.length)
            Variant(v.penalty + {if (toFind(v.pos) == key) 0 else 1}, v.pos + 1, child) :: pass
          else pass
        })

      if (v.pos != toFind.length) {
        Variant(v.penalty + 1, v.pos + 1, v.node) :: replacePass
      } else replacePass
    }

    streamGen(Context(TreeSet[Variant]() + init, TreeSet[Variant]()))(
      ctx => { // Option[(Variant, Context)]
        val (best, ctx2) = whileCached(ctx)
        if (best.isEmpty) None
        else {
          val vars = genvars(best.get)
          Some(( best.get, (ctx2 ++ vars).addCache(best.get) ))
        }
      })
  }
}

object Trie {
  def apply(seq: Iterator[String]) : Trie = 
    (new Trie /: seq.filter(! _.isEmpty))(_ + _)

  def apply(seq: Seq[String]) : Trie = apply(seq.iterator)
}

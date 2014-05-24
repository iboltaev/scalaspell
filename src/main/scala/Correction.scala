// nnsearch.correction
package nnsearch.correction {

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

/// correction token base class
abstract class Token(data: String)
/// correct symbols token
case class Regular(data: String) extends Token(data)
/// mismatch symbol token
case class Correction(data: String) extends Token(data)

object Corrector {

  /// a corrected word type
  type Word = Seq[Token]

  /// a prefix correction variant
  case class Variant(penalty: Int = -1, pos1: Int = -1, pos2: Int = -1) 
       extends Ordered[Variant] 
  {
    override def compare(other: Variant) = penalty - other.penalty
    def +(v: Int) = Variant(penalty + v, pos1, pos2)
  }

  /// standart Vagner-Fisher algo; produces Seq[(Symbol, wasItCorrect)]
  def compute(toCorrect: String, correction: String)
      : Seq[(Char, Boolean)] = 
  {
    // memoization cache
    val cache = Array.ofDim[Variant](toCorrect.size, correction.size)

    def minimum[T <: Ordered[T]](seq: T*) = {
      @tailrec def impl(accum: T, s: Seq[T]): T = 
	if (s.isEmpty) accum
	else impl(if (s.head < accum) s.head else accum, s.tail)

      impl(seq.head, seq.tail)
    }

    // computes correction matrix
    def matrix(pos1: Int, pos2: Int): Variant =
      if (pos1 == -1) Variant(pos2 - pos1, pos1, pos2)
      else if (pos2 == -1) Variant(pos1 - pos2, pos1, pos2)
      else {
	if (cache(pos1)(pos2) != null) cache(pos1)(pos2)
	else {
	  val v1 = matrix(pos1 - 1, pos2 - 1) + 
	  (if (toCorrect(pos1) == correction(pos2)) 0; else 1)
	
	  val v2 = matrix(pos1 - 1, pos2) + 1
	  val v3 = matrix(pos1, pos2 - 1) + 1
	  
	  val best = minimum(Variant(v1.penalty, pos1 - 1, pos2 - 1), 
			     Variant(v2.penalty, pos1 - 1, pos2), 
			     Variant(v3.penalty, pos1, pos2 - 1))
	  
	  cache(pos1)(pos2) = best
	  best
	}
    }

    // restores character sequence with correction flags
    @tailrec def sequentialize(pos1: Int, pos2: Int, buf: ArrayBuffer[(Char, Boolean)]) : Seq[(Char, Boolean)] = {
      if (pos1 == -1 || pos2 == -1) {
	if (pos2 != -1) {
	  buf append ((correction(pos2), true))
	  sequentialize(pos1, pos2 - 1, buf)
	} else buf.reverse
      } else {
	val current = cache(pos1)(pos2)
	val color = if (pos1 - current.pos1 != pos2 - current.pos2 ||
		      toCorrect(pos1) != correction(pos2)) true
		    else false
	if (pos2 != current.pos2)
	  buf append ((correction(pos2), color))

	sequentialize(current.pos1, current.pos2, buf)
      }
    }

    matrix(toCorrect.size - 1, correction.size - 1)
    sequentialize(
      toCorrect.size - 1, 
      correction.size - 1, 
      ArrayBuffer[(Char, Boolean)]())
  }

  /// produces sequence of correction tokens
  def correct(toCorrect: String, correction: String) = {
    @tailrec def helper(it: Iterator[(Char, Boolean)], accum: ArrayBuffer[Token]): Word = {
      if (it.isEmpty) accum
      else {
	val head = it.next // can reuse "it"
	val span = it span { _._2 == head._2 } // not safe to reuse "it"
	val data = (new StringBuilder().append(head._1) /: span._1)(_ append _._1).mkString
	accum append { if (head._2 == true) Correction(data)
		       else Regular(data)}
	helper(span._2, accum)
      }
    }

    val correctives = compute(toCorrect, correction)
    helper(correctives.iterator, ArrayBuffer[Token]())
  }
}

} // !nnsearch.correction

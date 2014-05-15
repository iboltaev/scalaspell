// nnsearch.correction
package nnsearch.correction {

import nnsearch.serialization.Serializer

import scala.collection.mutable.ArrayBuffer

/// correction token base class
abstract class Token(data: String)
/// correct symbols token
case class Regular(data: String) extends Token(data)
/// mismatch symbol token
case class Correction(data: String) extends Token(data)

object Corrector {

  /// a corrected word type
  type Word = List[Token]

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

     // computes correction matrix
    def computeRec(pos1: Int, pos2: Int): Variant = {
      if (pos1 == -1) return Variant(pos2 - pos1, pos1, pos2)
      else if (pos2 == -1) return Variant(pos1 - pos2, pos1, pos2)
      else {
	if (cache(pos1)(pos2) != null) return cache(pos1)(pos2)

	var v = Variant()

	val v1 = computeRec(pos1 - 1, pos2 - 1) + 
	    (if (toCorrect(pos1) == correction(pos2)) 0; else 1)
	
	val v2 = computeRec(pos1 - 1, pos2) + 1
	val v3 = computeRec(pos1, pos2 - 1) + 1

	if (v1 < v2) v = Variant(v1.penalty, pos1 - 1, pos2 - 1)
	else v = Variant(v2.penalty, pos1 - 1, pos2)

	if (v3 < v) v = Variant(v3.penalty, pos1, pos2 - 1)

	cache(pos1)(pos2) = v
	return v
      }
    }

    /// restores character sequence with correction flags
    def sequentialize(pos1: Int, pos2: Int) : Seq[(Char, Boolean)] = {
      var p1 = pos1
      var p2 = pos2
      val buffer = ArrayBuffer[(Char, Boolean)]()
      while (p1 != -1 && p2 != -1) {
	val current = cache(p1)(p2)
	val ppos1 = current.pos1
	val ppos2 = current.pos2
	val color =
	  if (p1 - ppos1 != p2 - ppos2 || 
	      toCorrect(p1) != correction(p2)) true
	  else false

	if (ppos2 != p2)
	  buffer.append((correction(p2), color))

	p1 = ppos1
	p2 = ppos2
      }

      while (p2 != -1) {
	buffer.append((correction(p2), true))
	p2 -= 1
      }

      return buffer.reverse
    }

    val v = computeRec(toCorrect.size - 1, correction.size - 1)
    sequentialize(toCorrect.size - 1, correction.size - 1)
  }

  /// produces sequence of correction tokens
  def correct(toCorrect: String, correction: String) = {
    def tokenFromSeq(s: Seq[(Char, Boolean)]): Token =
      if (s.isEmpty) Regular("")
      else {
	val sb = new StringBuilder
	s.foreach(p => sb.append(p._1))
	if (s.head._2 == true) Correction(sb.mkString)
	else Regular(sb.mkString)
      } 

    def correctImpl(s: Seq[(Char, Boolean)], accum: Word): Word =
      if (s.isEmpty) accum
      else {
	val tail = s.dropWhile(_._2 == s.head._2)
	val head = s.takeWhile(_._2 == s.head._2)
	correctImpl(tail, tokenFromSeq(head) :: accum)
      }

    val correctives = compute(toCorrect, correction)
    correctImpl(correctives, Nil).reverse
  }
}

} // !nnsearch.correction

package unittest {
  import org.scalatest._
  import nnsearch.correction._
  import nnsearch.NearestSearch

  class CorrectionSpec extends FlatSpec with Matchers {
    "A corrector" should "correct empty to non-empty string '' -> 'ab'" in {
      Corrector.compute("", "ab") should be (Seq(('a', true), ('b', true)))
    }

    it should "correct empty to empty string" in {
      Corrector.compute("", "") should be (Seq[(Char, Boolean)]())
    }

    it should "correct a -> ab" in {
      Corrector.compute("a", "ab") should be (
	Seq(('a', false), ('b', true)))
    }

    it should "correct non-intersecting string aaa -> bbb" in {
      Corrector.compute("aaa", "bbb") should be (
	Seq(('b', true), ('b', true), ('b', true)))
    }

    it should "correct case 1: arestant -> dagestan" in {
      Corrector.compute("arestant", "dagestan") should be (
	Seq(('d', true), ('a', false), ('g', true), ('e', false), 
	    ('s', false), ('t', false), ('a', false), ('n', false)))
    }

    it should "correct case 2: dagestan -> arestant" in {
      Corrector.compute("dagestan", "arestant") should be (
	Seq(('a', false), ('r', true), ('e', false), ('s', false), 
	    ('t', false), ('a', false), ('n', false), ('t', true)))
    }

    it should "produce correct tokens in arestant -> dagestan" in {
      Corrector.correct("arestant", "dagestan") should be (
	Seq(Correction("d"), Regular("a"), Correction("g"),
	  Regular("estan")))
    }
  }
}

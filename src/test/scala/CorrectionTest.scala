package unittest {
  import org.scalatest._
  import nnsearch.correction._
  import nnsearch.search.NearestSearch

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

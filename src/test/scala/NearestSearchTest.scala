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

    it should "corectly process bug with duplocate endings" in {
      val seq = Seq("sia", "siac", "sic")
      val trie = immutable.Trie(seq)
      NearestSearch.dNearest(trie, "sia", 2) should contain theSameElementsAs seq
    }

    it should "support search variants limitation" in {
      val maxCounter = 16
      val searcher = NearestSearch.K(1)
      val trie = immutable.Trie(Seq("111", "1111", "222", "333"))
      // without limits, this delta-request should return 3 values
      NearestSearch.dNearest(trie, "112", 3, maxCounter) should contain theSameElementsAs Seq(("111", 1))
    }
  }
}

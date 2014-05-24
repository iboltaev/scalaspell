package unittest {
import org.scalatest._
import nnsearch.SearchParams
import nnsearch.search.NearestSearch

  class SearchParamsSpec extends FlatSpec with Matchers {
    "SearchParams" should "parse 100500" in {
      SearchParams.searcher("100500") should be (
	NearestSearch.K(100500))
    }

    it should "parse & substitute 0" in {
      SearchParams.searcher("0") should be (
	NearestSearch.K(1))
    }

    it should "parse d128" in {
      SearchParams.searcher("d128") should be (
	NearestSearch.Delta(128))
    }

    it should "parse abc100500def" in {
      SearchParams.searcher("abc100500def") should be (
	NearestSearch.K(1))
    }
  }

} // !unittest

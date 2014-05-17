// nnsearch
package nnsearch {

import serialization.Serializer
import correction.Corrector

/// object represents mapping of url parameters to logic objects
object SearchParams {
  val kMatch = "(\\d+)".r
  val dMatch = "d(\\d+)".r

  /// get search object by its string representation
  def searcher(s: String) = {
    def intParam(s: String) = {
      val p = s.toInt
      if (p <= 0) 1
      else p
    }

    s match {
      case kMatch(k) => new NearestSearch.K(intParam(k))
      case dMatch(d) => new NearestSearch.Delta(intParam(d))
      case _ => new NearestSearch.K(1)
    }
  }

  /// get serializer
  def serializer(s: String): Serializer.Serializer =
    if (s == "json") Serializer.toJson
    else Serializer.toXml(_).toString

  /// get content-type
  def contentType(s: String) =
    if (s == "json") "application/json"
    else "application/xml"
}


} // !nnsearch

package unittest {
import org.scalatest._
import nnsearch.SearchParams
import nnsearch.NearestSearch

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

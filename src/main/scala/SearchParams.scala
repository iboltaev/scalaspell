// nnsearch
package nnsearch {

import search._
import serialization.Serializer
import correction.Corrector

/// object represents mapping of url parameters to logic objects
object SearchParams {
  private val kMatch = "(\\d+)".r
  private val dMatch = "d(\\d+)".r
  private val variantsLimit = 128 * 1024

  /// get search object by its string representation
  def searcher(s: String) = {
    def intParam(s: String) = {
      val p = s.toInt
      if (p <= 0) 1
      else p
    }

    s match {
      case kMatch(k) => new NearestSearch.K(intParam(k), variantsLimit)
      case dMatch(d) => new NearestSearch.Delta(intParam(d), variantsLimit)
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

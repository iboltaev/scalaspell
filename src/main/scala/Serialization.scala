// nnsearch.serialization
package nnsearch.serialization {

import scala.annotation.tailrec

import nnsearch.correction._

import scala.collection.mutable.ArrayBuffer

/**
 * Typically, such thing as serialization is made via "Visitor".
 * But the scala way seems to use simple pattern matching
 * (as recommended by M.Odersky) or implicit conversions + PM instead.
  */

object WordSerializer {

  def toXml(obj: Corrector.Word): xml.Elem = 
    <wc><w>{obj.word map { _ match {
      case Regular(data) => <r>{data}</r>
      case Correction(data) => <c>{data}</c>
    }}}</w><ec>{obj.errorCount}</ec></wc>
  
  def toJson(word: Corrector.Word): String = {
    val sb = new StringBuilder
    sb append "{["
    for (token <- word.word) token match {
      case Regular(data) => {
	sb append "{\"r\":\""
	sb append data
	sb append "\"},"
      }
      case Correction(data) => {
	sb append "{\"c\":\""
	sb append data
	sb append "\"},"
      }
    }
    if (sb.size > 2) sb deleteCharAt {sb.size - 1}
    sb append "],\"ec\":"
    sb append word.errorCount.toString
    sb append "}"
    sb.mkString
  }
}

object Serializer {

  /// type of correction word sequence function
  type Serializer = (Seq[Corrector.Word]) => String

  def toXml(seq: Seq[Corrector.Word]): xml.Elem = 
    <ws>{seq map {WordSerializer.toXml(_)}}</ws>

  def toJson(seq: Seq[Corrector.Word]): String = {
    val sb = new StringBuilder
    sb += '['
    for (token <- seq) {
      sb append {WordSerializer.toJson(token)}
      sb += ','
    }
    sb deleteCharAt {sb.size - 1}
    sb += ']'
    sb.mkString
  }
}

} // !nnsearch.serialization

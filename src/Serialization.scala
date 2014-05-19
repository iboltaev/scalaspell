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
object Serializer {

  /// type of correction word sequence function
  type Serializer = (Seq[Corrector.Word]) => String

  def toXml(obj: Corrector.Word): xml.Elem = <w>{obj map { _ match {
      case Regular(data) => <r>{data}</r>
      case Correction(data) => <c>{data}</c>
  }}}</w>

  def toXml(seq: Seq[Corrector.Word]): xml.Elem = 
    <ws>{seq map {toXml(_)}}</ws>

  def toJson(seq: Corrector.Word): String = {
    val sb = new StringBuilder
    sb += '{'
    for (token <- seq) token match {
      case Regular(data) => {
	sb append "\"r\":\""
	sb append data
	sb append "\","
      }
      case Correction(data) => {
	sb append "\"c\":\""
	sb append data
	sb append "\","
      }
    }
    sb deleteCharAt {sb.size - 1}
    sb += '}'
    sb.mkString
  }

  def toJson(seq: Seq[Corrector.Word]): String = {
    val sb = new StringBuilder
    sb += '['
    for (token <- seq) {
      sb append {toJson(token)}
      sb += ','
    }
    sb deleteCharAt {sb.size - 1}
    sb += ']'
    sb.mkString
  }
}

} // !nnsearch.serialization

package unittest {
  import org.scalatest._
  import nnsearch.serialization._
  import nnsearch.correction._
  import nnsearch.serialization.Serializer._

  class SerializeSpec extends FlatSpec with Matchers {
    "A Serializer" should "serialize arestant -> dagestan to XML" in {
      toXml(List(Correction("d"), Regular("a"), 
		 Correction("g"), Regular("estan"))) should be (
	<w><c>d</c><r>a</r><c>g</c><r>estan</r></w>)
    }

    it should "serialize several correction words to XML" in {
      val words = ("111", "112") :: ("222", "232") :: Nil
      toXml(
	List(
	  List(Regular("11"), Correction("2")), 
	  List(Regular("2"), Correction("3"), Regular("2")))) should be (
	<ws><w><r>11</r><c>2</c></w><w><r>2</r><c>3</c><r>2</r></w></ws>)
    }

    it should "serialize arestant -> dagestan to JSON" in {
      toJson(List(Correction("d"), Regular("a"), 
		 Correction("g"), Regular("estan"))) should be (
	"{\"c\":\"d\",\"r\":\"a\",\"c\":\"g\",\"r\":\"estan\"}")
    }

    it should "serialize several correction words to JSON" in {
      val words = ("111", "112") :: ("222", "232") :: Nil
      toJson(
	List(
	  List(Regular("11"), Correction("2")), 
	  List(Regular("2"), Correction("3"), Regular("2")))) should be ("[{\"r\":\"11\",\"c\":\"2\"},{\"r\":\"2\",\"c\":\"3\",\"r\":\"2\"}]")
    }
  }
}

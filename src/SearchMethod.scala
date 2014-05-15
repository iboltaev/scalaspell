// nnsearch
package nnsearch {

object SearchType extends Enumeration {
  val KNearest, DNearest = Value
}

/// searching method requested by user
case class SearchMethod(searchType: SearchType.Value, param: Int) {
  override def toString(): String = searchType match {
    case SearchType.KNearest => param.toString
    case SearchType.DNearest => "d" + param.toString
  }
}

object SearchMethod {
  val kMatch = "(\\d+)".r
  val dMatch = "d(\\d+)".r

  def fromString(s: String) = s match {
    case kMatch(k) => SearchMethod(SearchType.KNearest, 
				   {
				     val v = k.toInt
				     if (v == 0) 1
				     else v
				   })
    case dMatch(d) => SearchMethod(SearchType.DNearest, d.toInt)
    case _ => SearchMethod(SearchType.KNearest, 1)
  }
}

} // !nnsearch

package unittest {
import org.scalatest._
import nnsearch.SearchMethod
import nnsearch.SearchType

  class SearchMethodSpec extends FlatSpec with Matchers {
    "SearchMethod" should "parse 100500" in {
      SearchMethod.fromString("100500") should be (
	SearchMethod(SearchType.KNearest, 100500))
    }

    it should "parse & substitute 0" in {
      SearchMethod.fromString("0") should be (
	SearchMethod(SearchType.KNearest, 1))
    }

    it should "parse d128" in {
      SearchMethod.fromString("d128") should be (
	SearchMethod(SearchType.DNearest, 128))
    }

    it should "parse abc100500def" in {
      SearchMethod.fromString("abc100500def") should be (
	SearchMethod(SearchType.KNearest, 1))
    }
  }

} // !unittest

package unittest {
import org.scalatest._
import nnsearch.io._;

  class UriSpec extends FlatSpec with Matchers {
    "An uri matcher" should "find parameters in /?abcdsdf=&check=112&find=1" in {
      Url("/?abc=&check=112&find=1") should be (
	Url("/", Map("abc" -> "", "check" -> "112", "find" -> "1"))
      )
    }

    it should "find parameters in abc=123&find=&check&" in {
      Url("abc=123&find=&check&") should be (
	Url("", Map("abc" -> "123", "find" -> "", "check" -> ""))
	)
    }

    it should "recognize url /some/path?abc" in {
      Url("/some/path?abc") should be (
	Url("/some/path", Map("abc" -> ""))
      )
    }

    it should "map empty parameters list /path" in {
      Url("/path") should be (Url("/path", Map[String, String]()))
    }

    it should "handle empty url" in {
      Url("") should be (Url("", Map[String, String]()))
    }
  }
} // !unittest

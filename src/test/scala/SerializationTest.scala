package unittest {
  import org.scalatest._
  import nnsearch.serialization._
  import nnsearch.correction._
  import nnsearch.serialization.Serializer._

  class SerializeSpec extends FlatSpec with Matchers {
    "A Serializer" should "serialize arestant -> dagestan to XML" in {
      WordSerializer.toXml(List(Correction("d"), Regular("a"), 
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
      WordSerializer.toJson(List(Correction("d"), Regular("a"), 
		 Correction("g"), Regular("estan"))) should be (
	"[{\"c\":\"d\"},{\"r\":\"a\"},{\"c\":\"g\"},{\"r\":\"estan\"}]")
    }

    it should "serialize empty word to JSON" in {
      WordSerializer.toJson(Nil) should be ("[]")
    }

    it should "serialize several correction words to JSON" in {
      val words = ("111", "112") :: ("222", "232") :: Nil
      toJson(
	List(
	  List(Regular("11"), Correction("2")), 
	  List(Regular("2"), Correction("3"), Regular("2")))) should be ("[[{\"r\":\"11\"},{\"c\":\"2\"}],[{\"r\":\"2\"},{\"c\":\"3\"},{\"r\":\"2\"}]]")
    }
  }
}

package unittest {
  import org.scalatest._
  import nnsearch.serialization._
  import nnsearch.correction._
  import nnsearch.serialization.Serializer._

  class SerializeSpec extends FlatSpec with Matchers {
    "A Serializer" should "serialize arestant -> dagestan to XML" in {
      WordSerializer.toXml(
	Corrector.Word(
	  List(Correction("d"), Regular("a"), 
	       Correction("g"), Regular("estan")), 3)) should be (
	<wc><w><c>d</c><r>a</r><c>g</c><r>estan</r></w><ec>{3}</ec></wc>)
    }

    it should "serialize several correction words to XML" in {
      val words = ("111", "112") :: ("222", "232") :: Nil
      toXml(
	  List(
	    Corrector.Word(List(Regular("11"), Correction("2")), 1), 
	    Corrector.Word(List(Regular("2"), Correction("3"), Regular("2")), 1))) should be (
	<ws><wc><w><r>11</r><c>2</c></w><ec>{1}</ec></wc><wc><w><r>2</r><c>3</c><r>2</r></w><ec>{1}</ec></wc></ws>)
    }

    it should "serialize arestant -> dagestan to JSON" in {
      WordSerializer.toJson(
	Corrector.Word(
	  List(Correction("d"), Regular("a"), 
	       Correction("g"), Regular("estan")), 3)) should be (
	"{[{\"c\":\"d\"},{\"r\":\"a\"},{\"c\":\"g\"},{\"r\":\"estan\"}],\"ec\":3}")
    }

    it should "serialize empty word to JSON" in {
      WordSerializer.toJson(Corrector.Word(Nil, 0)) should be ("{[],\"ec\":0}")
    }

    it should "serialize several correction words to JSON" in {
      val words = ("111", "112") :: ("222", "232") :: Nil
      toJson(
	  List(
	    Corrector.Word(List(Regular("11"), Correction("2")), 1), 
	    Corrector.Word(List(Regular("2"), Correction("3"), Regular("2")), 1))) should be (
	    "[{[{\"r\":\"11\"},{\"c\":\"2\"}],\"ec\":1},{[{\"r\":\"2\"},{\"c\":\"3\"},{\"r\":\"2\"}],\"ec\":1}]")
    }
  }
}

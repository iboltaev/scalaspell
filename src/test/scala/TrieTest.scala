package unittest {
  import org.scalatest._
  import nnsearch.collections._

  class TrieSpec extends FlatSpec with Matchers {
    "A trie" should "be capable of precise searching" in {
      val trie = immutable.Trie(Seq("111", "1111", "222", "333"))
      trie contains "111" should be (true)
      trie contains "1111" should be (true)
      trie contains "222" should be (true)
      trie contains "333" should be (true)
      trie contains "345" should be (false)
    }
  }
}

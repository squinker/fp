package chapter10

import org.scalatest.{FlatSpec, Matchers}

class BagTest extends FlatSpec with Matchers{

  behavior of "bag"

  it should "Return a map of string to number of string occurences" in {
    val seq = IndexedSeq("one", "two", "two", "three", "three", "three", "four", "four", "four", "four")
    val res = chapter10.bag(seq)

    res.get("one").get shouldBe 1
    res.get("four").get shouldBe 4
  }

}

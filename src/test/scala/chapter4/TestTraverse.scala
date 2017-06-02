package chapter4

import org.scalatest.{FlatSpec, Matchers}

class TestTraverse extends FlatSpec with Matchers {

  val strToInt: String => Chapter4.Option[Int] = (str) => {
    try {
      Chapter4.Some(str.toInt)
    }
    catch {
      case e: Exception => Chapter4.None
    }
  }

  behavior of "Traverse function"

  it should "Return a Some(Nil) if an empty list is passed in" in {
    Chapter4.traverse[String, Int](Nil)(strToInt) shouldBe Chapter4.Some(Nil)
  }

  it should "Return a Some if a list of strings if the passed in function can convert all ints to strings" in {
    Chapter4.traverse[String, Int](List("1", "2", "3"))(strToInt) shouldBe Chapter4.Some(List(1,2,3))
  }

  it should "Return a None if the passed in function cannot convert one of the elements to a string" in {
    Chapter4.traverse[String, Int](List("1", "2", "DEADBEEF"))(strToInt) shouldBe Chapter4.None
  }

}

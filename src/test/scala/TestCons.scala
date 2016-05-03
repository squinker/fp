import fpinscala._
import fpinscala.List
import org.scalatest.{Matchers, FunSuite}


class TestCons extends FunSuite with Matchers{

  test("Test matching Cons matches nested Cons operators") {

    val result = List(1,2,3,4,5) match {

      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x+y
      case Cons(h, t)                            => h + fpinscala.List.sum(t)
      case _                                     => 101
    }

    assert( result == 3)
  }

  test("Test matching Cons matches nested Cons operators2") {

    val result = List(1,2,3,4,5) match {

      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      //case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x+y
      case Cons(h, t)                            => h + fpinscala.List.sum(t)
      case _                                     => 101
    }

    assert( result == 15)
  }

  test("Test matching Cons matches nested Cons operators3") {

    val result = List(1,2,3,4,5) match {

      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      //case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x+y
      //case Cons(h, t)                            => h + fpinscala.List.sum(t)
      case _                                     => 101
    }

    assert( result == 101)
  }

  test("Test homemade tail method"){
    List.tail(List(1,2,3,4,5)) should equal( List(2,3,4,5) )
    List.tail( List("one", "two", "three") ) should equal( List("two", "three")  )
    List.tail( Nil ) should equal( Nil  )
  }

}
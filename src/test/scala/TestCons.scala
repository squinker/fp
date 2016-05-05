import fpinscala._
import fpinscala.List
import org.scalatest.{Matchers, FunSuite}


class TestCons extends FunSuite with Matchers{

  test("matching Cons matches nested Cons operators") {

    val result = List(1,2,3,4,5) match {

      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x+y
      case Cons(h, t)                            => h + fpinscala.List.sum(t)
      case _                                     => 101
    }

    assert( result == 3)
  }

  test("matching Cons matches nested Cons operators2") {

    val result = List(1,2,3,4,5) match {

      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      //case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x+y
      case Cons(h, t)                            => h + fpinscala.List.sum(t)
      case _                                     => 101
    }

    assert( result == 15)
  }

  test("matching Cons matches nested Cons operators3") {

    val result = List(1,2,3,4,5) match {

      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      //case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x+y
      //case Cons(h, t)                            => h + fpinscala.List.sum(t)
      case _                                     => 101
    }

    assert( result == 101)
  }

  test("homemade tail method"){
    List.tail(List(1,2,3,4,5)) should equal( List(2,3,4,5) )
    List.tail( List("one", "two", "three") ) should equal( List("two", "three")  )
    List.tail( Nil ) should equal( Nil  )
  }

  test("homemade setHead method"){
    List.setHead( List(2,3,4,5), 1) should equal( List(1,2,3,4,5) )
    List.setHead( List("two", "three", "four"), "one" ) should equal( List("one", "two", "three", "four") )
  }

  test("Drop method should return a three element list when the first two elements are dropped from a five-element list"){
    List.drop( List(1,2,3,4,5), 2 ) should equal (List(3,4,5))
  }

  test("Drop method should return nil when trying to drop two elements from a one-element list"){
    List.drop( List(1), 2 ) should equal (Nil)
  }

  test("Drop method should the complete list when no elements are dropped from it"){
    List.drop( List(1, 2, 3), 0) should equal ( List(1, 2, 3) )
  }


  test("Dropwhile will remove even numbers from a list when supplied with the appropriate predicate"){

    List.dropWhile(  List(1, 10, 3, 4, 11, 5), (x:Int) => x > 5) should equal( List(5, 4, 3, 1) )
  }

  /*
  test("Dropwhile2 will remove even numbers from a list when supplied with the appropriate predicate"){

    List.dropWhile2(  List(1, 10, 3, 4, 11, 5), (x:Int) => x > 5) should equal(  List(5, 4, 3, 1) )
  }
  */

  test("Dropwhile2 will remove numbers greater than five subsequence from List"){

    List.dropWhile2(  List(6, 7, 8, 10, 3, 4, 11, 5), (x:Int) => x > 5) should equal(  List(3, 4, 11, 5) )
  }


  test("Init returns all but the last element of a list"){
    List.init( List(1, 2, 3, 4, 5, 10, 4) ) should equal( List(1, 2, 3, 4, 5, 10) )
  }

}
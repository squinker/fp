import org.scalatest.FunSuite

import FuncProgrammingChapter2.FuncProgrammingChapter2

class TestFib extends FunSuite {

  test("Fib 1 to 5") {

    assert( FuncProgrammingChapter2.fib(0) === 0)
    assert( FuncProgrammingChapter2.fib(1) === 1)
    assert( FuncProgrammingChapter2.fib(2) === 1)
    assert( FuncProgrammingChapter2.fib(3) === 2)
    assert( FuncProgrammingChapter2.fib(4) === 3)
    assert( FuncProgrammingChapter2.fib(5) === 5)
    assert( FuncProgrammingChapter2.fib(6) === 8)
    assert( FuncProgrammingChapter2.fib(7) === 13)
  }

  test("Is sorted for array of ints") {


    val intCompare: (Int, Int) => Boolean = (x, y) => x <= y

    assert( FuncProgrammingChapter2.isSorted[Int](Array(1, 2, 3, 4), intCompare) )
    assert( FuncProgrammingChapter2.isSorted[Int](Array(1, 1, 2, 3, 4), intCompare) )
    assert( ! FuncProgrammingChapter2.isSorted[Int](Array(1, 3, 2, 4), (x, y) => x <= y) )

  }
}
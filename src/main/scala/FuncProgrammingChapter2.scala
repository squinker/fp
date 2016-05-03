package FuncProgrammingChapter2

object FuncProgrammingChapter2 {

  def fib(n: Int): BigInt = {
    @annotation.tailrec
    def fib_tail( n: Int, a:Int, acc:Int): Int = n match {
      case 0 => a
      case _ => fib_tail( n-1, acc, a + acc )
    }
    fib_tail(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @annotation.tailrec
    def sorted_tail(n: Int, as: Array[A]): Boolean =
      if (n >= (as.length -1)) true
      else if ( ordered(as(n), as(n + 1)) ) sorted_tail(n + 1, as)
      else false

    sorted_tail(0, as)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    b => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b:B) => f(a,b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)


  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f( g(a) )

}

object MyModule {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {

    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

      go(n, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {

    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {

    println(formatAbs(-42))

    println(formatFactorial(7))

  }

}



package chapter2

import scala.annotation.tailrec

object Chapter2 {



  /*
  fib(0)

  loop(0, 0, 1)

  def loop(n = 0, prev = 0, cur = 1)

  if(n==0 ) prev   //END Returns 0
  else loop(0-1, 1, 0 + 1)






  ////
  fib(1)

  loop(1, 0, 1)

  // 1st call
  def loop(n = 1, prev = 0, cur = 1)
  if(n == 0) prev   //FALSE
  else loop(1-1, 1, 0 + 1)

  // 2nd call
  def loop(n = 0, prev = 1, cur = 1 )
  if(n == 0) prev     //TRUE , so returns 1
  else loop( 0 -1, 0, 1 + 1)


Function.uncurried()


  ////
  fib(2)


  /*
  // 1st call
  def loop(n = 2, prev = 0, cur = 1)
  if(n == 0 ) prev   //FALSE
  else(loop(2 - 1, 1, 0 + 1))

  // 2nd call
  def loop(1, 1, 0)
*/






  def fib(n: Int): Int = {

    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)
    loop(n, 0, 1)

  }

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length-1) true
      else if (gt(as(n), as(n+1))) false
      else go(n+1)
    go(0)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)



  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => ()




*/



}

package fpinscala

sealed trait List[+A]

case object Nil extends List[Nothing]

case class  Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {

    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {

    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =

    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def tail[A](as: List[A] ): List[A] =
    as match {
      case Nil         => as
      case Cons(x, xs) => xs
    }

  def setHead[A](as: List[A], newHead: A): List[A] = {
    as match {
      case Nil => Cons(newHead, Nil)
      case _   => Cons(newHead, as)
    }
  }

  def drop[A](as: List[A], n: Int): List[A] = {
    if (n == 0) as
    else as match {
        case Nil         => Nil
        case Cons(x, xs) => drop(xs, n - 1)
      }

  }


  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = {


    def dropWhileHelper[A](as: List[A], f: A => Boolean, acc: List[A]): List[A] = {
      as match {

        case Nil         => acc
        case Cons(x, xs) =>
          if   ( f(x) ) dropWhileHelper(xs, f, acc        )
          else          dropWhileHelper(xs, f, Cons(x, acc))
      }
    }

    dropWhileHelper(as, f, Nil)
  }



  def dropWhile2[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile2(t, f)
      case _ => l
    }


  def append[A](a1: List[A], a2: List[A]): List[A] =

    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] = {



    def initHelper[A](l: List[A], acc: List[A], prev: List[A]): List[A] = {
      l match {
        case Nil         => prev
        case Cons(x, xs) => initHelper(xs, Cons(x, acc), acc )
      }
    }
    initHelper(l, Nil, Nil)
  }

}



import scala.annotation.tailrec

object Chapter3 {


  /*
  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]


  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }


  //3.2
  def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }
  }

  //3.3
  //Using the same idea, implement the function setHead for replacing the first element
  //of a List with a different value.
  def setHead[A](newHead: A, list: List[A]): List[A] = {
    list match {
      case Nil => Cons(newHead, Nil)
      case Cons(_, tail) => Cons(newHead, tail)
    }
  }

  //3.4
  //Generalize tail to the function drop, which removes the first n elements from a list.
  //  Note that this function takes time proportional only to the number of elements being
  //  dropped—we don’t need to make a copy of the entire List.

  def drop[A](l: List[A], n: Int): List[A] = {

    l match {
      case Nil => Nil
      case Cons(h, t) =>

        n match {
          case 0 => l
          case notZero => drop(t, n-1)
        }
    }
  }

  //3.5
  //Implement dropWhile, which removes elements from the List prefix as long as they
  //match a predicate.

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {

    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _                  => l
    }
  }

  //3.6
  //Not everything works out so nicely. Implement a function, init, that returns a List
  //consisting of all but the last element of a List. So, given List(1,2,3,4), init will
  //return List(1,2,3). Why can’t this function be implemented in constant time like
  //tail?


  def init[A](l: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(h, Nil) => l
        case Cons(h, t) => Cons(h, init(t))
      }
  }

  //3.7

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)
  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)



  foldRight( List(1,2,3), Nil:List[Int])(Cons(_,_))


  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {

    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  }

  //3.11
  def sumUsingFoldLeft(as: List[Int]): Int = foldLeft(as, 0)(_+_)
  def productUsingFoldLeft(as: List[Int]): Int = foldLeft(as, 0)(_ * _)
  def lengthOfListUsingFoldLeft[A](as: List[A]): Int = foldLeft(as, 0)((total, _) => total + 1)


  //3.12

  def reverseListUsingFold[A](as: List[A]): List[A] = foldLeft(as, List[A]())((acc,h) => Cons(h,acc))

  //3.13

  def foldLeftUsingFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, (b:B) => b)( (a,g) => b => g( f(b,a) ) )(z)
  }

  //3.14
  //def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
  //  foldLeft(as, )
 // }


*/






}

package Chapter5New

object Chapter5 {

  sealed trait Stream[+A] {
    def toList: List[A] = {
      this match {
        case Empty => Nil
        case Cons(h, t) => h() :: t().toList
      }
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, tl) if n > 1 => Stream.cons(h(), tl().take(n - 1))
      case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
      case _ => Stream.empty

    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }


    def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if f(h()) => Stream.cons(h(), t() takeWhile f)
      case _ => Stream.empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    def forAll(p: A => Boolean): Boolean = {

      foldRight(true)((a, b) => p(a) && b)
    }


    def takeWhileUsingFoldRight(f: A => Boolean): Stream[A] = {
      foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else Stream.empty)
    }

    def headOptionUsingFoldRight:Option[A] = {
      foldRight[Option[A]](None)( (a,_) => Some(a))
    }

    def mapUsingFoldRight[B](f: A => B): Stream[B] = {
      foldRight[Stream[B]](Empty)( (a,b) =>  Cons( ()=> f(a), () => b) )
    }

  }


  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))


  }


}

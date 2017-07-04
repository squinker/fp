package chapter5


object Chapter5 {

  sealed trait Stream[+A] {

    def toList: List[A] = this match {

      case Empty => Nil
      case Cons(h, tl) => h() :: tl().toList

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

    def forall(f: A => Boolean): Boolean = {
      foldRight(true)((a, b) => f(a) && b)
    }

    def takeWhile2(f: A => Boolean): Stream[A] = {
      foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else Stream.empty)
    }

    def headOption: Option[A] = {
      foldRight(None: Option[A])((h, _) => Some(h))

    }

    def mapUsingFoldright[B](f: A => B): Stream[B] = {
      foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))
    }

    def filterUsingFoldRight(p: A => Boolean): Stream[A] = {
      foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)
    }

    def appendUsingFoldright[B >: A](s: => Stream[B]): Stream[B] = {
      foldRight(s)((a, b) => Stream.cons(a, b))
    }

    def flatMap[B](f: A => Stream[B]):Stream[B] = {

      foldRight(Stream.empty[B])( (h,t) => f(h) appendUsingFoldright(t))

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

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def constant2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  val  fibs:Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] = Stream.cons(f0, go(f0, f0+f1))
    go(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[ (A,S) ] ): Stream[A] = {

    f(z) match {
      case None        => Stream.empty[A]
      case Some((a,s)) => Stream.cons(a, unfold(s)(f))
    }
  }

  def fibsUsingUnfold():Stream[Int] = unfold((0,1))(s => Some( (s._1,(s._1,s._1 + s._2)) ))

  def fromUsingUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((n,n+1)))

  def constantUsingUnfold[A](a: A): Stream[A] = unfold(a)(_ =>  Some((a,a)) )

  def onesUsingUnfold(): Stream[Int] = unfold(1)(_ => Some(1,1))



  
}

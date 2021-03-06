package chapter5

import scala.collection.immutable.Stream.cons


object Chapter5 {

  sealed trait Stream[+A] {

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, tl) => p(h()) || tl().exists(p)
      case _           => false
    }

    /*
  def containsSubsequence[A](s2: Stream[A]): Boolean = {

    def matchStreams(s1: Stream[A], subSeq: Stream[A]): Boolean = {

      s1 match {

        case Cons(s1h, s1tl) =>
          subSeq match {

            case Cons(subSeqH, subSeqtl) =>

              if (s1h() == subSeqH()) matchStreams(s1tl(), subSeqtl())
              else matchStreams(s1tl(), subSeq)

            case _ => true
          }
        case _ => false

      }
    }
    matchStreams(this, s2)
    }
    */


    /*
    def containsSubsequenceUsingZipall[A](s2: Stream[A]): Boolean = {

      def helper(str: Stream[A], subseq: Stream[A]): Boolean = {

        str.zipAll(subseq).forall(s => s match {

          case (str, sub) if str == sub => true
          case _ => helper(str.drop(1), subseq)
        })
      }
      helper(this, s2)
    }
    */


    // eg stream1.startsWith(Stream(1,2,3)
    def startsWith[A](str: Stream[A]): Boolean = {

      this.zipAll(str).takeWhile(!_._2.isEmpty) forall {
        case (h1, h2) => h1 == h2
      }
    }

    def tailsUsingUnfold: Stream[Stream[A]] = {

      unfold(this)(s => s match {
        case Cons(h, tl) => Some( (Stream.cons(h(), tl()), tl())  )
        case _           =>  None
      })
    } appendUsingFoldright Stream.empty

    def hasSubsequence[A](s: Stream[A]): Boolean = tailsUsingUnfold exists (_ startsWith(s))

    def scanRight[B](init: B)(f: (A, => B) => B): Stream[B] =
      foldRight( (init, Stream(init) ))((a,p0) => {

        lazy val p1 = p0
        val b2 = f(a, p1._1)
        (b2, Stream.cons(b2, p1._2))
      })._2



    def toList: List[A] = this match {

      case Empty       => Nil
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

    def flatMap[B](f: A => Stream[B]): Stream[B] = {
      foldRight(Stream.empty[B])((h, t) => f(h) appendUsingFoldright (t))
    }

    /*
    def mapUsingUnfold[B](f: A => B): Stream[B] = {
      unfold(this)(s => s match {
        case Stream.empty => None
        case Cons(h, tl) => Some((f(h()), tl()))
      }
      )
    }
    */

    def takeUsingUnfold(n: Int): Stream[A] = {
      unfold((n, this))(s =>

        if (s._1 < 1) None
        else s._2 match {
          case Cons(h, tl) => Some(h(), (n - 1, tl()))
          case _ => None
        }
      )
    }

    def takeWhileUsingUnfold(f: A => Boolean): Stream[A] = {

      unfold(this)(s => s match {
        case Cons(h, tl) => if (f(h())) Some(h(), tl()) else None
        case _ => None
      })
    }


    def zipWithUsingUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {

      unfold((this, s2))(s => s match {
        case (Cons(h1, tl1), Cons(h2, tl2)) => Some((f(h1(), h2()), (tl1(), tl2())))
        case _ => None
      })
    }


    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {

      unfold((this, s2))(s => s match {

        case (Cons(h1, tl1), Cons(h2, tl2)) => Some((Some(h1()), Some(h2())), (tl1(), tl2()))
        case (Empty, Cons(h2, tl2)) => Some((None, Some(h2())), (Stream.empty, tl2()))
        case (Cons(h1, tl1), Empty) => Some((Some(h1()), None), (tl1(), Stream.empty))
        case _ => None
      })
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

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] = Stream.cons(f0, go(f0, f0 + f1))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {

    f(z) match {
      case None => Stream.empty[A]
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }
  }

  def fibsUsingUnfold(): Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._1, s._1 + s._2))))

  def fromUsingUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((n, n + 1)))

  def constantUsingUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

  def onesUsingUnfold(): Stream[Int] = unfold(1)(_ => Some(1, 1))


}

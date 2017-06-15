package chapter4


object Chapter4 {

  trait Option[+A] {

    def map[B](f: A => B): Option[B] = {
      this match {
        case Some(value) => Some(f(value))
        case None => None
      }
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {

      map(f) getOrElse (None)
    }


    def getOrElse[B >: A](default: => B): B = {
      this match {
        case Some(value) => value
        case None => default
      }
    }


    def orElse[B >: A](ob: => Option[B]): Option[B] = this map (value => Some(value)) getOrElse ob


    def filter(f: A => Boolean): Option[A] = this.flatMap { v =>
      if (f(v)) this
      else None
    }
  }

  //sealed trait Option[+]
  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]


  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(al => b.map(bl => f(al, bl)))

  def map3[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] = a.flatMap(al => b.flatMap(bl => c.map(cl => f(al, bl, cl))))

  def map4[A, B, C, D, E](a: Option[A], b: Option[B], c: Option[C], d: Option[D])(f: (A, B, C, D) => E): Option[E]
  = a.flatMap(al => b.flatMap(bl => c.flatMap(cl => d.map(dl => f(al, bl, cl, dl)))))

  object Option {
    def sequence[A](a: List[Option[A]]): Option[List[A]] = {

      a match {
        case Nil => Some(Nil)
        case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
      }


    }

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {

      a match {
        case Nil => Some(Nil)
        case x :: xs => f(x).flatMap(bb => traverse(xs)(f).map(l => bb :: l))
      }
    }

    def sequenceUsingTraverse[A](a: List[Option[A]]): Option[List[A]] = {
      traverse(a)(x => x)
    }
  }


  object Either {

    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
      es match {
        case Nil => Right(Nil)
        case x :: xs => x flatMap (a => sequence(xs) map (l => a :: l))
      }
    }


    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      as match {
        case Nil => Right(Nil)
        case x :: xs => f(x) flatMap (a => traverse(xs)(f) map (v => a :: v))
      }
    }


    def traverseUsingMap2[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      as.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)((a, b) => a :: b))
    }

    def sequenceUsingTraverse[E, A](as: List[Either[E, A]]): Either[E, List[A]] = {
      traverseUsingMap2(as)((a) => a)
    }



  }

  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {

      case Left(e) => Left(e)
      case Right(a) => Right(f(a))

    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {

      this match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
      }
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {

      this match {
        case Left(_) => b
        case Right(_) => this
      }
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {

      this match {
        case Left(e) => Left(e)
        case Right(a) =>
          b map (bval => f(a, bval))


      }
    }

    def map2usingFor[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {

      for {
        a <- this
        b1 <- b
      } yield f(a, b1)

    }


  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]


}

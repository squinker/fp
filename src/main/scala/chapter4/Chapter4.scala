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
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {

    a match {
      case Nil     => Some(Nil)
      case x :: xs => f(x).flatMap(bb => traverse(xs)(f).map(l => bb :: l))

    }
  }

  def sequenceUsingTraverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(x => x)
  }

}

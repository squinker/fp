
object Chapter4 {

  trait Option[+A] {

    def map[B](f: A => B): Option[B] = {
      this match {
        case Some(value) => Some(f(value))
        case None        => None
      }
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {

      map(f) getOrElse(None)
    }


    def getOrElse[B >: A](default: => B): B = {
      this match {
        case Some(value) => value
        case None        => default
      }
    }


    def orElse[B >: A](ob: => Option[B]): Option[B] = this map (value => Some(value)) getOrElse ob


    def filter(f: A => Boolean): Option[A] = this.flatMap{v =>
      if( f(v) )this
      else None
    }
  }

  //sealed trait Option[+]
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]


  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum/xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap( m => mean(xs.map(x => math.pow(x-m,2))) )

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(al => b.map(bl => f(al, bl)))




}

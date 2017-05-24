
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


    def filter(f: A => Boolean): Option[A] = if(f(this)) this else None
  }

  //sealed trait Option[+]
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]
}

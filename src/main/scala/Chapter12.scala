import chapter11.chapter11.{Functor, Monad}

object Chapter12 {


  trait Applicative[F[_]] extends Functor[F] {
    //def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
    //def unit[A](a: A): F[A]
    //def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit( () ))( (a, _) => f(a))
    //def traverse[A,B](as: List[A], f: A => F[B]): F[List[B]] =
     // as.foldRight( unit(List[B])() )((a, b) => map2( f(a), b)((c, d) => c :: d))
    //def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas, x => unit(x))
    //def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
    //def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)( (a, b) => (a, b) )

    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fa, fab)( (a, fb) => fb(a) )
    def unit[A](a: => A): F[A]
    def map[A, B](fa: F[A])(f: A => B): F[B] = apply( unit(f) )(fa)
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply( apply( unit(f.curried) )(fa) )(fb)
    def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
    def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C],
                        fd: F[D])(f: (A, B, C, D) => E): F[E] = apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
  }


  //https://underscore.io/blog/posts/2016/12/05/type-lambdas.html
  //https://kubuszok.com/compiled/kinds-of-types-in-scala/
  def eitherMonad[E]: Monad[ ({type f[x] = Either[E, x]})#f ] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = {
      ma match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
      }
    }
  }

  /*
  def eitherMonad[E]: Monad[ ( {type f[x] = Either[E, x]}) #f ]

  sealed trait Validation[+E, +A]
  case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]

  class ApplicativeValidation[E] extends Applicative[Validation] {
    override def unit[A](a: => A): Validation[E, A] = ???

  }
  */

}

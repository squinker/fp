package chapter11

import chapter6.chapter6.State
import chapter7FromWeb.Nonblocking._

object chapter11 {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Monad[F[_]] extends Functor[F] {

    def unit[A](a: => A): F[A]

    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

    def map[A, B](ma: F[A])(f: A => B): F[B] = {
      flatMap(ma)(a => unit(f(a)))
    }

    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))

    def sequence[A](lma: List[F[A]]): F[List[A]] = {
      lma.foldRight(unit(List[A]()))((a, b) => map2(a, b)(_ :: _))
    }

    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
      la.foldRight(unit(List[B]()))((a, b) => map2(f(a), b)(_ :: _))
    }

    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {

      ms match {
        case Nil => unit(Nil)
        case h :: t =>

          flatMap( f(h))(res =>  map( filterM(t)(f) )(l => if(res) h :: l else   l) )
      }
    }

    def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap( f(a) )(g)

    def flatMapUsingCompose[A, B](ma: F[A])(f: A => F[B]): F[B] = compose[Unit, A, B](_ => ma, f)(())

    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(m => m)

    def flatMapInTermsOfJoinAndMap[A, B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(a => f(a)))


  }

  object ParMonad extends Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  object OptionMonad extends Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(a => f(a))
  }

  object StreamMonad extends Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(a => f(a))
  }


  case class Id[A](value: A) {

    def map[B](f: A => B): B = f(value)

    def flatMap[F[_], B](f: A => F[B]): F[B] = f(value)
  }


  object Id {
    val idMonad = new Monad[Id] {
      override def unit[A](a: => A): Id[A] = Id(a)

      override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma flatMap(f)
    }
  }


  def stateMonad[S] = new Monad[ ({type f[x] = State[S,x]})#f ] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]) = st.flatMap[B](f)

    def replicateM[A](n: Int, ma: Id[A]): Id[List[A]] = ???
  }

  case class Reader[R, A](run: R => A)

  object Reader {
    def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {

      def unit[A](a: => A): Reader[R, A] = Reader(_ =>  a)
      def flatMap[A, B](st : Reader[R, A])(f: A => Reader[R, B]): Reader[R, B]
        = Reader(r => f( st.run(r) ).run(r) )

    }
  }
  // what is action of flatmap?
  // What meaning does it give to monadic functions like sequence join and repM?
  // What meaning does it give to the monad laws?


  val stringReaderMonad = Reader.readerMonad[String]

}

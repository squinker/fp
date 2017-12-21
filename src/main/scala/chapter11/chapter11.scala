package chapter11

import chapter7FromWeb.Nonblocking._

object chapter11 {

  trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
  }

  trait Monad[F[_]] extends Functor[F]{

    def unit[A](a: => A): F[A]
    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
    def map[A,B](ma: F[A])(f: A => B): F[B] = {
      flatMap(ma)( a =>  unit(f(a)) )
    }


    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))


    def sequence[A](lma: List[F[A]]): F[List[A]] = {

      /*
      lma match {
        case Nil => unit(Nil)

        case x :: xs =>
          flatMap(x)( a => map( sequence(xs) )(l => a :: l) )
      }
      */

      lma.foldRight( unit(List[A]()) )( (a,b) => map2(a, b)(_ :: _ ) )

    }

    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {

      /*
      la match {
        case Nil =>
           unit(Nil)
        case x :: xs =>
          flatMap( f(x) )(a => map(traverse(xs)(f))(l => a :: l) )
      }
      */

      la.foldRight(unit(List[B]()))((a, b ) => map2(f(a), b)(_ :: _ ))

    }
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


  /*
  import chapter6.chapter6.State

  object StateMonad extends Monad[State[_, _]] {
     def unit[A, B](a: => A): State[A, B] = ???

    override def flatMap[A, B](ma: State[A, B])(f: A => State[B]): State[B] = ???
  }
  */




}

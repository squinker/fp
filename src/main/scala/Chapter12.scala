import chapter11.chapter11.Functor

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



  }

}

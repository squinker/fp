package chapter7

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

object withActors {

  sealed trait Future[+A] {
    private [chapter7] def apply(k: A => Unit): Unit
  }
  type Par[+A] = ExecutorService => Future[A]

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight[Par[List[A]]](unit(List()))((a, b) => map2(a, b)(_ :: _))
  }

  def asyncF[A, B](f: A => B): A => Par[B] = a => unit(f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }


  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)

    p(es) { a => ref.set(a); latch.countDown }

    latch.await
    ref.get
  }


  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        cb(a)
    }

  def fork[A](a: => Par[A]): Par[A] = {
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        eval(es)(a(es)(cb))
    }
  }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit( new Callable[Unit] {def call = r})



  def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      def apply (cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None

        val combiner = Actor[Either[A,B]](es) {

          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a,b)))
          }

          case  Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a,b)))
          }
        }

        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }

}

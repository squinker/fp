package chapter7

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Future}

import scala.concurrent.duration.TimeUnit

object chapter7 {



  type Par[A] = ExecutorService => Future[A]

  object Par {

    /*
    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new java.util.concurrent.atomic.AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
      val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
      p(es) { a => ref.set(a); latch.countDown } // Asynchronously set the result, and decrement the latch
      latch.await // Block until the `latch.countDown` is invoked asynchronously
      ref.get // Once we've passed the latch, we know `ref` has been set, and return its value
    }
*/

    /*
    def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => f(a)(es)(cb))
      }

*/
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def map2[A, B, C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }

    def map[A,B](a: => Par[A])(f: A => B): Par[B] = {
      map2(a, unit(()))( (a,_) => f(a) )
    }

    def fork[A](a: => Par[A]): Par[A] = es => es.submit( new Callable[A] {def call = a(es).get} )

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
      ps.foldRight[Par[List[A]]](unit(List()))((a, b) => map2(a, b)(_ :: _))
    }


    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val pars: List[Par[List[A]]] = as map asyncF(a => if(f(a)) List(a) else Nil  )
      map(sequence(pars))(_.flatten)
    }

    def parSum(nums: List[Int]): Par[Int] = nums.foldRight[Par[Int]](unit(0))( (a,b) => map2(unit(a),b)(_ + _) )


    // List paragraphs, returns total num of words across all paragraphs, generalize as much as possible
    def totalWordsAcrossAllParagraphsPar(paragraphs: List[String]): Par[Int] = {

      val res: Par[Int] =  map(parMap(paragraphs)(a => a.split("\\s").toList))(a => a.flatten.size)
      res
    }

    def map3[A,B,C,D](fa: => Par[A], fb: => Par[B], fc: => Par[C])(f: (A, B, C) => D): Par[D] = {

      val m: Par[(C) => D] = map2(fa, fb)((a, b) => (c: C) => f(a, b, c))
      map2( map2(fa, fb)((a, b) => (c: C) => f(a, b, c)), fc)((e,f) => e(f) )

  }
  def run(nums: List[Int]) = Par.parSum(nums)
  def sumParagraphs(paras: List[String]) = Par.totalWordsAcrossAllParagraphsPar(paras)
  }
}

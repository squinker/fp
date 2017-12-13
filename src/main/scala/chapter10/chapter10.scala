package chapter10

import java.util.concurrent.ExecutorService

import chapter7FromWeb.Nonblocking._
import chapter7FromWeb.Nonblocking.Par._

object chapter10 {


  //10.1
  trait Monoid[A] {
    def op(a1: A, a2: A): A

    def zero: A
  }

  object IntAddition extends Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    def zero = 0
  }

  object IntMultiplication extends Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    def zero = 1
  }

  object BooleanOrMonoid extends Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    def zero = false
  }

  object BooleanAndMonoid extends Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    def zero = true
  }

  val intAddition: Monoid[Int] = IntAddition
  val intMultiplication: Monoid[Int] = IntMultiplication
  val booleanOr: Monoid[Boolean] = BooleanOrMonoid
  val booleanAndMonoid: Monoid[Boolean] = BooleanAndMonoid


  //10.2

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    def zero: Option[A] = None
  }

  //10.3

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: (A) => A, a2: (A) => A) = a1.compose(a2)
    override def zero(): A => A = x => x
  }

  //10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((x, y) => m.op(x, f(y)))

  //10.6
  def foldRightUsingFoldMap[A,B](as: List[A])(z: B)(f: (A,B) => B): B = foldMap( as, endoMonoid[B])( f.curried  )(z)

  //10.7
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {

    def helper[A,B](s: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {

      s match {
        case head +: tail   => m.op(f(head), helper(tail, m)(f))
        case head +: Seq()  => f(head)
        case Seq() +: Seq() => m.zero
      }
    }
    val res: (IndexedSeq[A], IndexedSeq[A]) = v.splitAt(v.length/2)
    m.op( helper(res._1, m)(f), helper(res._2, m)(f))
  }

  //10.8
  def par[A](m : Monoid[A]): Monoid[Par[A]] = {

    new Monoid[Par[A]] {
      override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1,a2)(m.op)
      override def zero = Par.unit(m.zero)
    }
  }

  //def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
  //  val res: Par[List[B]] = Par.parMap(v.toList)(f)
  //  res
 // }

  //def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
  //  Par.parMap(v)(f).flatMap { bs =>
  //    foldMapV(bs, par(m))(b => Par.async(b))
  //  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  //= foldMapV(v,par(m))( (a => Par.unit(f(a))) )

  //10.9 Use foldMap to see if an IndexedSeq[Int] is ordered

  /*
  def isOrdered(c: IndexedSeq[Int]) = {


    val m = new Monoid[Int] {
      override def op(a1: Int, a2: Int) = a2-a1
      override def zero = 0
    }

    val (first, second ) = c.splitAt(c.length/2)

    foldMap(c.toList, m)


    def helper(s: IndexedSeq[Int]): IndexedSeq[Int] = {
      foldMap(s, m)(x => x)
    }

  }
  */

  //Ex 10.10
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC


  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC) = {

      (a1, a2) match {
        case (Stub(c1), Stub(c2))                                          => Stub(c1 + c2)
        case (Stub(c1), Part(lstub, words, rStub))                         => Part(c1 + lstub, words, rStub)
        case (Part(lstub, words, rStub), Stub(c1))                         => Part(lstub, words, rStub + c1)
        case (Part(lstub1, words1, rStub1), Part(lstub2, words2, rStub2) ) => Part(lstub1, words1 + words2 + 1, rStub1)
      }
    }
    override def zero = Stub("")
  }

  // Ex 10.11


  //def countWords(str: String): Int = {

    //def charToWc(c: Char): WC =
   //   if (c.isWhitespace) Part("", 0, "")
   //   else Stub(c.toString)

//
 //   def unstub(s: String) = s.length min 1

  //  foldMapV(str.toIndexedSeq, wcMonoid)(charToWc)


//  }


  // Ex 10.12

  trait Foldable[F[_]] {

    def foldRight[A,B](as: F[A])(z: B)(f: (A,B)=> B): B =
      foldMap(as)(f.curried)(endoMonoid[B])(z)

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
      foldMap(as)(a => (b:B) => f(b,a))(dual(endoMonoid[B]))(z)

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      foldRight(as)(mb.zero)( (a, b) => mb.op(f(a),b) )


    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)( m.op)
  }

  object ListFoldable extends Foldable[List] {

    override def foldRight[A,B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)


    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((a,b) => mb.op(f(b),a))

  }

  object IndexedSeqFoldable extends Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def concatenate[A](as: IndexedSeq[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = foldLeft(as)(mb.zero)((a,b)=> mb.op(a, f(b)))
  }



  sealed trait Tree[+A]

  case class Leaf[A](v: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object TreeFoldable extends Foldable[Tree] {

    override def foldMap[A,B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case Leaf(v) => f(v)
        case Branch(l, r) => mb.op( foldMap(l)(f)(mb), foldMap(r)(f)(mb) )
      }


    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Leaf(v) => f(v, z)
        case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
      }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Leaf(v) => f(z, v)
        case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
      }
  }



  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = {

    new  Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

      override def zero: (A, B) = (A.zero, B.zero)
    }
  }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = {
      new Monoid[A => B] {
        override def op(a1: A => B, a2: A => B): A => B  = x => B.op(a1(x), a2(x))

        override def zero: A => B = x => B.zero
      }
  }


  // 10.18

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = {
    new Monoid[Map[K, V]] {

      def zero = Map[K, V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
                              b.getOrElse(k, V.zero)))
        }
    }
  }


  val seq = IndexedSeq("one", "two", "two", "three", "three", "three", "four", "four", "four", "four")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {

     IndexedSeqFoldable.foldMap(as)(a => Map(a -> 1))(mapMergeMonoid(intAddition))

  }




  val res = bag(IndexedSeq("one", "two", "three", "three"))
}



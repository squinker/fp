package chapter10

class chapter10 {


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
        case Seq() +: tail  => f(tail)
        case Seq() +: Seq() => m.zero
      }

    }
    val res: (IndexedSeq[A], IndexedSeq[A]) = v.splitAt(v.length/2)
    m.op( helper(res._1, m)(f), helper(res._2, m)(f))
  }
}



package fpinscala

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

  val intAddition: Monoid[Int]          = IntAddition
  val intMultiplication: Monoid[Int]    = IntMultiplication
  val booleanOr: Monoid[Boolean]        = BooleanOrMonoid
  val booleanAndMonoid: Monoid[Boolean] = BooleanAndMonoid



  //10.2

  def optionMonoid[A]: Monoid[Option[A]] = {
    def op(a1: Option[A], a2: Option[A] ): Option[A] = a1 orElse a2

    def zero: Option[A] = None


  }
}

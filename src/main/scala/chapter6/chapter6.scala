package chapter6


object chapter6 {

  type Rand[+A] = RNG => (A, RNG)

  trait RNG {
    def nextInt: (Int, RNG)
  }


  case class SimpleRNG(seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0XBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

  }


  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }


  def intDouble(rng: RNG): ((Int, Double), RNG) = {

    val (int, rng1) = rng.nextInt
    val (dub, rng2) = double(rng1)

    ((int, dub), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (int, rng1) = rng.nextInt
    val (dub, rng2) = double(rng1)

    ((dub, int), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (dub1, rng2) = double(rng)
    val (dub2, rng3) = double(rng2)
    val (dub3, rng4) = double(rng3)

    ((dub1, dub2, dub3), rng4)
  }


  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    def helper(c: Int, rn: List[Int], newrng: RNG): (List[Int], RNG) = {

      if (c == 0) (rn, newrng)
      else {
        val (i, rng) = newrng.nextInt
        helper(c - 1, i :: rn, rng)
      }
    }

    helper(count, Nil, rng)
  }

  def map[S, A, B](a: S => (A, S))(f: A => B): S => (B, S) = {
    rng => {

      val (a1, rng2) = a(rng)
      (f(a1), rng2)
    }
  }


  //Ex 6.05
  def doubleUsingMap(): Rand[Double] = {
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))
  }

  //Ex 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra.apply(rng)
      val (b, rng3) = rb.apply(rng2)

      (f(a, b), rng3)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  //def unit[A](a: A): Rand[A] = rng => (a, rng)

  def unitGeneral[A, S](a: A): S => (A, S) = s => (a, s)

  //def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  //  fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {

    rng => {

      val (a, rng2) = f(rng)

      g(a)(rng2)
    }

  }

  //def mapUsingFlatmap[A, B](s: Rand[A])(f: A => B): Rand[B] =
  //  flatMap(s)(a => unit(f(a)))


  def map2UsingFlatmap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }


  //type State[S,+A] = S => (A,S)
  case class State[S, +A](run: S => (A, S)) {

    def map[B](f: A => B): State[S, B] =
      flatMap(a => State.unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] = {
      ls.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
    }

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }


  //6.11

  sealed trait Input

  //define functions corresponsing to these inouts
  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  /*
  {


    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = { State(s => {




    }

    /*
    def simulateMachineWithFold(inputs: List[Input]): State[Machine, (Int, Int)] = {


    inputs.fold(State.unit[Machine,(Int, Int)](this.coins, this.candies))

    }
    */
  }


  }

  val res: State[Machine, (Int, Int)] = Machine(true, 10, 0).simulateMachine(List(Coin, Turn))
*/

}
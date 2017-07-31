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

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def unitGeneral[A, S](a: A): S => (A, S) = s => (a, s)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {

    rng => {

      val (a, rng2) = f(rng)

      g(a)(rng2)
    }

  }

  def mapUsingFlatmap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))


  def map2UsingFlatmap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {


    //flatMap(ra)(a => flatMap(rb)(b => unit(f(a,b))))
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  }


  //type State[S,+A] = S => (A,S)
  case class State[S, +A](run: S => (A, S)){
    def map[S, A, B](a: S => (A, S))(f: A => B): S => (B, S) = {
      s => {

        val (a1, s2) = a(s)
        (f(a1), s2)
      }
    }

    def map2[A,B,C,S](ra: S => (A,S), rb: S => (B, S))(f: (A, B) => C): S => (C, S) = {

      s => {
        val (a, s2) = ra(s)
        val (b, s3) = rb(s2)

        (f(a,b),s3)
      }
    }
    
  }

  object State {
    def unit[A, S](a: A): S => (A, S) = s => (a, s)
  }

}

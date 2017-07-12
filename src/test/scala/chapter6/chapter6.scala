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

    ((int, dub),rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (int, rng1) = rng.nextInt
    val (dub, rng2) = double(rng1)

    ((dub, int),rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (dub1, rng2) = double(rng)
    val (dub2, rng3) = double(rng2)
    val (dub3, rng4) = double(rng3)

    ((dub1,dub2,dub3),rng4)
  }


  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    def helper(c: Int, rn: List[Int], newrng: RNG): (List[Int], RNG) = {

      if (c == 0) (rn, newrng)
      else {
        val (i, rng) = newrng.nextInt
        helper(c-1,i :: rn, rng)
      }
    }
    helper(count, Nil, rng)
  }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {

      val (a, rng2) = s(rng)
      (f(a),rng2)
    }
  }

  def doubleUsingMap(rng: RNG): Rand[Double] = {
    map( nonNegativeInt )(i => i / (Int.MaxValue.toDouble + 1))
  }


}

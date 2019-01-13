package fpinscala._06state

trait RNG {

  def nextInt: (Int, RNG)
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (randomInt, newRNG) = rng.nextInt
    (if (randomInt < 0) -(randomInt + 1) else randomInt, newRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (randomInt, newRNG) = nonNegativeInt(rng)
    (randomInt.toDouble / Int.MaxValue, newRNG)
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, newRNG) = s(rng)
    (f(a), newRNG)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = f(rng)
    g(a)(rng1)
  }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a, b)))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((rand, acc) => map2(rand, acc)(_ :: _))
}

case class LinearCongruentalRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = LinearCongruentalRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

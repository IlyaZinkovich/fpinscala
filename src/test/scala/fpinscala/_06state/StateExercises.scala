package fpinscala._06state

import fpinscala._06state.RNG._
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class StateExercises extends FlatSpec with Matchers {

  "Exercise 6.1" should "nonNegativeInt" in {
    val rng = LinearCongruentalRNG(1)
    nonNegativeInt(nonNegativeInt(rng)._2)._1 should be(1151252338)
  }

  "Exercise 6.2" should "double between 0 and 1" in {
    val rng = LinearCongruentalRNG(1)
    double(rng)._1 should be(1.79162249052507E-4)
  }

  "Exercise 6.3" should "pairs and tuples" in {
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (randomInt, rng1) = rng.nextInt
      val (randomDouble, rng2) = double(rng1)
      ((randomInt, randomDouble), rng2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val (randomDouble, rng1) = double(rng)
      val (randomInt, rng2) = rng1.nextInt
      ((randomDouble, randomInt), rng2)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (randomDouble1, rng1) = double(rng)
      val (randomDouble2, rng2) = double(rng1)
      val (randomDouble3, rng3) = double(rng2)
      ((randomDouble1, randomDouble2, randomDouble3), rng3)
    }

    val rng = LinearCongruentalRNG(1)
    intDouble(rng)._1 should be(384748, 0.5360936459787626)
    doubleInt(rng)._1 should be(1.79162249052507E-4, -1151252339)
    double3(rng)._1 should be(1.79162249052507E-4, 0.5360936459787626, 0.25582678907356543)
  }

  "Exercise 6.4" should "ints" in {
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      if (count > 0) {
        val (randomInt, newRNG) = rng.nextInt
        val (list, finalRNG) = ints(count - 1)(newRNG)
        (randomInt :: list, finalRNG)
      } else (Nil, rng)
    }

    def intsTailRecursive(count: Int)(rng: RNG): (List[Int], RNG) = {
      @tailrec
      def loop(loopCount: Int, loopRNG: RNG, randomInts: List[Int]): (List[Int], RNG) = {
        if (loopCount <= 0)
          (randomInts, loopRNG)
        else {
          val (randomInt, newLoopRNG) = loopRNG.nextInt
          loop(loopCount - 1, newLoopRNG, randomInt :: randomInts)
        }
      }

      loop(count, rng, Nil)
    }

    val rng = LinearCongruentalRNG(1)
    ints(3)(rng)._1 should be(List(384748, -1151252339, -549383847))
    intsTailRecursive(3)(rng)._1 should be(List(-549383847, -1151252339, 384748))
  }

  "Exercise 6.5" should "double via map" in {
    def double: Rand[Double] = map(nonNegativeInt)(_.toDouble / Int.MaxValue)

    val rng = LinearCongruentalRNG(1)
    double(rng)._1 should be(1.79162249052507E-4)
  }

  "Exercise 6.6" should "map2" in {
    val rng = LinearCongruentalRNG(1)
    map2(double, nonNegativeInt)((_, _))(rng)._1 should be(1.79162249052507E-4, 1151252338)
  }

  "Exercise 6.7" should "sequence and ints" in {
    def randomInt: Rand[Int] = _.nextInt

    def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(randomInt))

    val rng = LinearCongruentalRNG(1)
    sequence(List(double(_), nonNegativeInt(_)))(rng)._1 should be(List(1.79162249052507E-4, 1151252338))
    ints(3)(rng)._1 should be(List(384748, -1151252339, -549383847))
  }

  "Exercise 6.8" should "flatMap and nonNegativeLessThan" in {
    def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { randomInt =>
      val mod = randomInt % n
      if (randomInt + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

    val rng = LinearCongruentalRNG(1)
    nonNegativeLessThan(10)(rng)._1 should be(8)
  }

  "Exercise 6.9" should "map and map2 via flatMap" in {
    val rng = LinearCongruentalRNG(1)
    map2ViaFlatMap(double, nonNegativeInt)((_, _))(rng)._1 should be(1.79162249052507E-4, 1151252338)
    mapViaFlatMap(nonNegativeInt)(_.toDouble / Int.MaxValue)(rng)._1 should be(1.79162249052507E-4)
  }

  "Exercise 6.10" should "State unit, flatMap, map, map2 and sequence" in {
    val randomInt: State[RNG, Int] = State(rng => rng.nextInt)
    val rng = LinearCongruentalRNG(1)
    State.sequence[RNG, Int](List[State[RNG, Int]](randomInt, randomInt)).run(rng)._1 should be(List(384748, -1151252339))
  }
}

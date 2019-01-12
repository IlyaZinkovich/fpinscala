package fpinscala._05sctictnessandlaziness

import org.scalatest.{FlatSpec, Matchers}

class StreamExercises extends FlatSpec with Matchers {

  "Exercise 5.1" should "toList" in {
    def square(number: Int): Int = {
      val result = number * number
      println(result)
      result
    }

    val stream = Stream(square(1), square(2), square(3))
    stream.toList should be(List(1, 4, 9))
    stream.toListStackUnsafe should be(List(1, 4, 9))
  }

  "Exercise 5.2" should "take and drop" in {
    val stream = Stream(1, 2, 3)

    stream.take(2).toList should be(List(1, 2))
    stream.drop(1).toList should be(List(2, 3))
  }

  "Exercise 5.3" should "takeWhile" in {
    val stream = Stream(1, 2, 3)

    stream.takeWhile(_ < 3).toList should be(List(1, 2))
    stream.takeWhile(_ % 2 == 1).toList should be(List(1))
  }

  "Exercise 5.4" should "forAll" in {
    val stream = Stream(1, 2, 3)

    stream.forAll(_ < 4) should be(true)
    stream.forAll(_ % 2 == 1) should be(false)
  }

  "Exercise 5.5" should "takeWhile via foldRight" in {
    val stream = Stream(1, 2, 3)

    stream.takeWhileViaFoldRight(_ < 3).toList should be(List(1, 2))
    stream.takeWhileViaFoldRight(_ % 2 == 1).toList should be(List(1))
  }

  "Exercise 5.6" should "headOption via foldRight" in {
    Stream().headOption() should be(None)
    Stream(1, 2, 3).headOption() should be(Some(1))
  }

  "Exercise 5.7" should "map, filter, append, and flatMap" in {
    val stream = Stream(1, 2, 3)

    stream.map(_ * 2).toList should be(List(2, 4, 6))
    stream.filter(_ < 0).toList should be(List())
    stream.filter(_ % 2 == 1).toList should be(List(1, 3))
    stream.append(Stream(4, 5, 6)).toList should be(List(1, 2, 3, 4, 5, 6))
    stream.append(Stream()).toList should be(stream.toList)
    stream.flatMap(value => Stream(value * 2)).toList should be(List(2, 4, 6))
  }

  "Exercise 5.8" should "constant" in {
    def constant[A](a: A): Stream[A] = {
      lazy val const: Stream[A] = Cons(() => a, () => const)
      const
    }

    constant(1).take(2).toList should be(List(1, 1))
  }

  "Exercise 5.9" should "from n" in {
    def from(n: Int): Stream[Int] = {
      Stream.cons(n, from(n + 1))
    }

    from(1).take(3).toList should be(List(1, 2, 3))
  }

  "Exercise 5.10" should "fibs" in {
    def fibs(): Stream[Int] = {
      def generator(first: Int, second: Int): Stream[Int] = {
        Stream.cons(first, generator(second, first + second))
      }

      generator(0, 1)
    }

    fibs().take(6).toList should be(List(0, 1, 1, 2, 3, 5))
  }

  "Exercise 5.11" should "unfold" in {
    val generateThreeZeros = Stream.unfold(1)(state => if (state < 4) Some(0, state + 1) else None)
    generateThreeZeros.take(5).toList should be(List(0, 0, 0))
  }

  "Exercise 5.12" should "fibs, from, constant, ones via unfold" in {
    def fibs(): Stream[Int] =
      Stream.unfold((0, 1)) {
        case (first, second) => Some(first, (second, first + second))
      }

    def from(n: Int): Stream[Int] = Stream.unfold(n)(state => Some((state, state + 1)))

    def constant[A](a: A): Stream[A] = Stream.unfold(a)(state => Some((state, state)))

    def ones: Stream[Int] = constant(1)

    fibs().take(6).toList should be(List(0, 1, 1, 2, 3, 5))
    from(1).take(3).toList should be(List(1, 2, 3))
    constant(1).take(2).toList should be(List(1, 1))
    ones.take(2).toList should be(List(1, 1))
  }

  "Exercise 5.13" should "map, take, takeWhile, zipWith, and zipAll" in {
    val stream = Stream(1, 2, 3)

    stream.mapViaUnfold(_ * 2).toList should be(List(2, 4, 6))
    stream.takeViaUnfold(2).toList should be(List(1, 2))
    stream.takeViaUnfold(0).toList should be(List())
    stream.takeWhileViaUnfold(_ < 3).toList should be(List(1, 2))
    stream.takeWhileViaUnfold(_ % 2 == 1).toList should be(List(1))
    Stream(1, 2).zipAll(Stream(3)).toList should be(List((Some(1), Some(3)), (Some(2), None)))
    Stream(1, 2).zipWith(Stream(3))(_ + _).toList should be(List(4))
  }

  "Exercise 5.14" should "startsWith" in {
    val stream = Stream(1, 2, 3)

    stream.startsWith(Stream(1, 2)) should be(true)
    stream.startsWith(Stream(2)) should be(false)
  }

  "Exercise 5.15" should "tails" in {
    val stream = Stream(1, 2, 3)

    stream.tails.toList.map(_.toList) should be(List(List(1, 2, 3), List(2, 3), List(3), List()))
  }

  "Exercise 5.16" should "scanRight" in {
    val stream = Stream(1, 2, 3)

    stream.scanRight(0)(_ + _).toList should be(List(6, 5, 3, 0))
  }
}

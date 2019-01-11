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
}

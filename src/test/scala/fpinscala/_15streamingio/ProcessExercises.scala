package fpinscala._15streamingio

import org.scalatest.{FlatSpec, Matchers}

class ProcessExercises extends FlatSpec with Matchers {

  "Exercise 15.1" should "take drop" in {
    val process: Process[Int, Int] = Process.lift[Int, Int](_ * 2)
    val stream = Stream(1, 2, 3)
    process(stream).toList should be(List(2, 4, 6))
//    process.take(2)(stream).toList should be(List(2, 4))
//    process.drop(2)(stream).toList should be(List(6))
//    process.takeWhile((i: Int) => i < 5)(stream).toList should be(List(2, 4))
//    process.dropWhile((i: Int) => i < 5)(stream).toList should be(List(6))
  }

  "Exercise 15.2" should "count" in {
    Process.id.count(Stream(5, 6, 7)).toList should be(List(1, 2, 3))
  }
}

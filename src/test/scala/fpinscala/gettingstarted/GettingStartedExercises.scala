package fpinscala.gettingstarted

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class GettingStartedExercises extends FlatSpec with Matchers {

  "Exercise 2.1" should "generate fibonacci numbers" in {
    def fibonacci(number: Int): Int = {
      @tailrec
      def loop(number: Int, previous: Int, current: Int): Int = {
        if (number == 0) previous
        else if (number == 1) previous + current
        else loop(number - 1, current, previous + current)
      }

      loop(number, 0, 1)
    }

    fibonacci(0) should be(0)
    fibonacci(1) should be(1)
    fibonacci(5) should be(8)
  }

  "Exercise 2.2" should "check if array is sorted" in {
    def isSorted[A](array: Array[A], ordered: (A, A) => Boolean): Boolean = {
      @tailrec
      def loop(n: Int, acc: Boolean): Boolean = {
        if (n == array.length - 1) acc
        else loop(n + 1, ordered(array(n), array(n + 1)))
      }

      loop(0, acc = true)
    }

    val ordered: (Int, Int) => Boolean = (left: Int, right: Int) => left <= right
    isSorted(Array(1, 2, 3), ordered) should be(true)
    isSorted(Array(1, 3, 2), ordered) should be(false)
  }

  "Exercise 2.3" should "curry" in {
    def curry[A, B, C](f: (A, B) => C): A => B => C = {
      a: A => (b: B) => f(a, b)
    }

    val curriedSum = curry((a: Int, b: Int) => a + b)
    curriedSum(1)(2) should be(3)
  }

  "Exercise 2.4" should "uncurry" in {
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
      (a, b) => f(a)(b)
    }

    val uncurriedSum = uncurry(((a: Int, b: Int) => a + b).curried)
    uncurriedSum(1, 2) should be(3)
  }

  "Exercise 2.5" should "compose" in {
    def compose[A, B, C](f: B => C, g: A => B): A => C = {
      a: A => f(g(a))
    }

    val composition = compose((b: Int) => b + 1, (a: Int) => a + 2)
    composition(1) should be(4)
  }
}

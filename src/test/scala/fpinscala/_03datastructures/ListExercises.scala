package fpinscala._03datastructures

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class ListExercises extends FlatSpec with Matchers {

  "Exercise 3.1" should "give right answer" in {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Construct(head, tail) => head + sum(tail)
    }

    val result = List(1, 2, 3, 4, 5) match {
      case Construct(x, Construct(2, Construct(4, _))) => x
      case Nil => 42
      case Construct(x, Construct(y, Construct(3, Construct(4, _)))) => x + y
      case Construct(h, t) => h + sum(t)
      case _ => 101
    }
    result should be(3)
  }

  "Exercise 3.2" should "tail" in {
    def tail[A](list: List[A]): List[A] = list match {
      case Nil => Nil
      case Construct(_, Nil) => Nil
      case Construct(_, tail) => tail
    }

    tail(List()) should be(List())
    tail(List(1)) should be(List())
    tail(List(1, 2)) should be(List(2))
    tail(List(1, 2, 3)) should be(List(2, 3))
  }

  "Exercise 3.3" should "set head" in {
    def setHead[A](newHead: A, list: List[A]): List[A] = list match {
      case Nil => Nil
      case Construct(_, tail) => Construct(newHead, tail)
    }

    setHead(1, List()) should be(List())
    setHead(2, List(1)) should be(List(2))
    setHead(3, List(1, 2)) should be(List(3, 2))
  }

  "Exercise 3.4" should "drop" in {
    def drop[A](list: List[A], n: Int): List[A] = {
      list match {
        case Nil => Nil
        case original@Construct(_, tail) =>
          if (n < 1) original
          else if (n == 1) tail
          else drop(tail, n - 1)
      }
    }

    drop(List(), 1) should be(List())
    drop(List(1), 1) should be(List())
    drop(List(1, 2), 1) should be(List(2))
    drop(List(1, 2), 2) should be(List())
    drop(List(1, 2, 3), 1) should be(List(2, 3))
    drop(List(1, 2, 3), 0) should be(List(1, 2, 3))
    drop(List(1, 2, 3), -1) should be(List(1, 2, 3))
    drop(List(), 0) should be(List())
    drop(List(), -1) should be(List())
  }

  "Exercise 3.5" should "dropWhile" in {
    def dropWhile[A](list: List[A], predicate: A => Boolean): List[A] = list match {
      case Nil => Nil
      case original@Construct(head, tail) =>
        if (predicate(head)) dropWhile(tail, predicate) else original
    }

    def dropWhileWithTypeInference[A](list: List[A])(predicate: A => Boolean): List[A] = list match {
      case Nil => Nil
      case original@Construct(head, tail) =>
        if (predicate(head)) dropWhile(tail, predicate) else original
    }

    val predicate: Int => Boolean = (number: Int) => number < 2
    dropWhile(List(), predicate) should be(List())
    dropWhile(List(1), predicate) should be(List())
    dropWhile(List(1, 2), predicate) should be(List(2))
    dropWhile(List(1, 2, 3), predicate) should be(List(2, 3))
    dropWhileWithTypeInference(List(1, 2, 3))(_ < 2) should be(List(2, 3))
  }

  "Exercise 3.+" should "append" in {
    def append[A](target: List[A], elements: List[A]): List[A] = target match {
      case Nil => elements
      case Construct(head, tail) => Construct(head, append(tail, elements))
    }

    append(List(1), List(2, 3, 4)) should be(List(1, 2, 3, 4))
  }

  "Exercise 3.6" should "init" in {
    def init[A](source: List[A]): List[A] = source match {
      case Nil => Nil
      case Construct(_, Nil) => Nil
      case Construct(head, tail) => Construct(head, init(tail))
    }

    def initEfficient[A](source: List[A]): List[A] = {
      import collection.mutable.ListBuffer
      val buf = new ListBuffer[A]

      @annotation.tailrec
      def loop(current: List[A]): List[A] = current match {
        case Nil => Nil
        case Construct(_, Nil) => List(buf.toList: _*)
        case Construct(head, tail) =>
          buf += head
          loop(tail)
      }

      loop(source)
    }

    init(List(1, 2, 3)) should be(List(1, 2))
    initEfficient(List(1, 2, 3)) should be(List(1, 2))
  }

  "Exercise 3.++" should "append" in {
    def foldRight[A, B](source: List[A], initialValue: B)(combine: (A, B) => B): B = source match {
      case Nil => initialValue
      case Construct(head, tail) => combine(head, foldRight(tail, initialValue)(combine))
    }

    def sum(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

    def product(doubles: List[Double]): Double = foldRight(doubles, 1.0)(_ * _)

    sum(List(1, 2, 3, 4)) should be(10)
    product(List(1, 2, 3, 4)) should be(24.0)
  }

  "Exercise 3.7" should "short-circuit foldRight" in {
    """No, this is not possible!
      |The reason is because _before_ we ever call our function, `f`, we evaluate its argument,
      |which in the case of `foldRight` means traversing the list all the way to the end.
      |We need _non-strict_ evaluation to support early termination""" shouldNot be("")
  }

  "Exercise 3.8" should "foldRight with Nil and Cons" in {
    def foldRight[A, B](source: List[A], initialValue: B)(combine: (A, B) => B): B = source match {
      case Nil => initialValue
      case Construct(head, tail) => combine(head, foldRight(tail, initialValue)(combine))
    }

    foldRight(List(1, 2, 3), Nil: List[Int])(Construct(_, _)) should be(List(1, 2, 3))
  }

  "Exercise 3.9" should "length" in {
    def foldRight[A, B](source: List[A], seed: B)(f: (A, B) => B): B = source match {
      case Nil => seed
      case Construct(head, tail) => f(head, foldRight(tail, seed)(f))
    }

    def length[A](source: List[A]): Int = foldRight(source, 0)((_, accumulator) => accumulator + 1)

    length(List()) should be(0)
    length(List(1, 2, 3)) should be(3)
  }

  "Exercise 3.10" should "foldLeft" in {
    @tailrec
    def foldLeft[A, B](source: List[A], accumulator: B)(f: (B, A) => B): B = source match {
      case Nil => accumulator
      case Construct(head, tail) => foldLeft(tail, f(accumulator, head))(f)
    }

    foldLeft(List(1, 2, 3, 4), 0)(_ + _) should be(10)
    foldLeft(List(1, 2, 3, 4), 1.0)(_ * _) should be(24.0)
  }

  "Exercise 3.11" should "sum, product and length with foldLeft" in {
    @tailrec
    def foldLeft[A, B](source: List[A], accumulator: B)(f: (B, A) => B): B = source match {
      case Nil => accumulator
      case Construct(head, tail) => foldLeft(tail, f(accumulator, head))(f)
    }

    def length[A](source: List[A]): Int = foldLeft(source, 0)((accumulator, _) => accumulator + 1)

    def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

    def product(doubles: List[Double]): Double = foldLeft(doubles, 1.0)(_ * _)

    sum(List(1, 2, 3, 4)) should be(10)
    product(List(1, 2, 3, 4)) should be(24)
    length(List(1, 2, 3, 4)) should be(4)
  }

  "Exercise 3.12" should "reverse" in {
    @tailrec
    def foldLeft[A, B](source: List[A], accumulator: B)(f: (B, A) => B): B = source match {
      case Nil => accumulator
      case Construct(head, tail) => foldLeft(tail, f(accumulator, head))(f)
    }

    def reverse[A](source: List[A]): List[A] =
      foldLeft(source, Nil: List[A])((accumulator, value) => Construct(value, accumulator))

    reverse(List(1, 2, 3)) should be(List(3, 2, 1))
  }

  "Exercise 3.13" should "foldRight through foldLeft" in {
    @tailrec
    def foldLeft[A, B](source: List[A], accumulator: B)(f: (B, A) => B): B = source match {
      case Nil => accumulator
      case Construct(head, tail) => foldLeft(tail, f(accumulator, head))(f)
    }

    def reverse[A](source: List[A]): List[A] =
      foldLeft(source, Nil: List[A])((accumulator, value) => Construct(value, accumulator))

    def foldRight[A, B](source: List[A], seed: B)(f: (A, B) => B): B =
      foldLeft(reverse(source), seed)((a, b) => f(b, a))

    foldRight(List(1, 2, 3, 4), 0)(_ + _) should be(10)
    foldRight(List(1, 2, 3, 4), 1.0)(_ * _) should be(24.0)
  }

  "Exercise 3.14" should "append" in {
    @tailrec
    def foldLeft[A, B](source: List[A], accumulator: B)(f: (B, A) => B): B = source match {
      case Nil => accumulator
      case Construct(head, tail) => foldLeft(tail, f(accumulator, head))(f)
    }

    def append[A](source: List[A], elements: List[A]): List[A] =
      foldLeft(source, elements)((accumulator, value) => Construct(value, accumulator))

    append(List(1), List(2, 3, 4)) should be(List(1, 2, 3, 4))
  }

  "Exercise 3.15" should "concat" in {
    @tailrec
    def foldLeft[A, B](source: List[A], accumulator: B)(f: (B, A) => B): B = source match {
      case Nil => accumulator
      case Construct(head, tail) => foldLeft(tail, f(accumulator, head))(f)
    }

    def reverse[A](source: List[A]): List[A] =
      foldLeft(source, Nil: List[A])((accumulator, value) => Construct(value, accumulator))

    def append[A](source: List[A], elements: List[A]): List[A] =
      foldLeft(source, elements)((accumulator, value) => Construct(value, accumulator))

    def concat[A](source: List[List[A]]): List[A] = {
      reverse(foldLeft(source, Nil: List[A])((accumulator, value) => append(value, accumulator)))
    }

    concat(List(List(1), List(2, 3, 4), List(5, 6))) should be(List(1, 2, 3, 4, 5, 6))
  }

  "Exercise 3.16" should "adds 1" in {
    import Support.foldRight

    def add1(list: List[Int]): List[Int] =
      foldRight(list, Nil: List[Int])((value, accumulator) => Construct(value + 1, accumulator))

    add1(List(1, 2, 3)) should be(List(2, 3, 4))
  }

  "Exercise 3.17" should "map to string" in {
    import Support.foldRight

    def mapToString(list: List[Int]): List[String] =
      foldRight(list, Nil: List[String])((value, accumulator) => Construct(value.toString, accumulator))

    mapToString(List(1, 2, 3)) should be(List("1", "2", "3"))
  }

  "Exercise 3.18" should "map" in {
    import Support.foldRight

    def map[A, B](list: List[A])(f: A => B): List[B] =
      foldRight(list, Nil: List[B])((value, accumulator) => Construct(f(value), accumulator))

    def mapWithMutableList[A, B](list: List[A])(f: A => B): List[B] = {
      val mutableList = new ListBuffer[B]

      def loop(list: List[A]): Unit = list match {
        case Nil => ()
        case Construct(head, tail) => mutableList += f(head); loop(tail)
      }

      loop(list)
      List(mutableList.toList: _*)
    }

    map(List(1, 2, 3))(_ + 1) should be(List(2, 3, 4))
    map(List(1, 2, 3))(_.toString) should be(List("1", "2", "3"))
    mapWithMutableList(List(1, 2, 3))(_ + 1) should be(List(2, 3, 4))
    mapWithMutableList(List(1, 2, 3))(_.toString) should be(List("1", "2", "3"))
  }

  "Exercise 3.19" should "filter" in {
    import Support.foldRight

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      foldRight(list, Nil: List[A])((value, accumulator) =>
        if (predicate(value)) Construct(value, accumulator) else accumulator)

    def filterWithMutableList[A](list: List[A])(predicate: A => Boolean): List[A] = {
      val mutableList = new ListBuffer[A]

      def loop(list: List[A]): Unit = list match {
        case Nil => ()
        case Construct(head, tail) => if (predicate(head)) mutableList += head; loop(tail)
      }

      loop(list)
      List(mutableList.toList: _*)
    }

    filter(List(1, 2, 3))(_ % 2 == 1) should be(List(1, 3))
    filterWithMutableList(List(1, 2, 3))(_ % 2 == 1) should be(List(1, 3))
  }

  "Exercise 3.20" should "flatMap" in {
    import Support.{concat, map}

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = concat(map(list)(f))

    flatMap(List(1, 2, 3))(i => List(i, i)) should be(List(1, 1, 2, 2, 3, 3))
  }

  "Exercise 3.21" should "filter with flatMap" in {
    import Support.flatMap

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      flatMap(list)(value => if (predicate(value)) List(value) else Nil)

    filter(List(1, 2, 3))(_ % 2 == 1) should be(List(1, 3))
  }

  "Exercise 3.22" should "zip with add" in {
    def zipAdd(left: List[Int], right: List[Int]): List[Int] = (left, right) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Construct(leftHead, leftTail), Construct(rightHead, rightTail)) =>
        Construct(leftHead + rightHead, zipAdd(leftTail, rightTail))
    }

    zipAdd(List(1, 2, 3), List(4, 5, 6)) should be(List(5, 7, 9))
  }

  "Exercise 3.23" should "zip with" in {
    def zipWith[A, B, C](left: List[A], right: List[B])(f: (A, B) => C): List[C] = (left, right) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Construct(leftHead, leftTail), Construct(rightHead, rightTail)) =>
        Construct(f(leftHead, rightHead), zipWith(leftTail, rightTail)(f))
    }

    zipWith(List(1, 2, 3), List(4, 5, 6))((a, b) => a + b) should be(List(5, 7, 9))
  }

  "Exercise 3.24" should "has sub-sequence" in {
    @annotation.tailrec
    def startsWith[A](list: List[A], prefix: List[A]): Boolean = (list, prefix) match {
      case (_, Nil) => true
      case (Construct(head, tail), Construct(prefixHead, prefixTail)) if head == prefixHead => startsWith(tail, prefixTail)
      case _ => false
    }

    @annotation.tailrec
    def hasSubsequence[A](source: List[A], subsequence: List[A]): Boolean = (source, subsequence) match {
      case (Nil, Nil) => true
      case (Nil, _) => false
      case _ if startsWith(source, subsequence) => true
      case (Construct(_, tail), _) => hasSubsequence(tail, subsequence)
    }

    hasSubsequence(List(1, 2, 3), List()) should be(true)
    hasSubsequence(List(1, 2, 3), List(1)) should be(true)
    hasSubsequence(List(1, 2, 3), List(1, 2)) should be(true)
    hasSubsequence(List(1, 2, 3), List(2, 3)) should be(true)
    hasSubsequence(List(1, 2, 3), List(1, 3)) should be(false)
  }

  object Support {

    @tailrec
    def foldLeft[A, B](source: List[A], accumulator: B)(f: (B, A) => B): B = source match {
      case Nil => accumulator
      case Construct(head, tail) => foldLeft(tail, f(accumulator, head))(f)
    }

    def foldRight[A, B](source: List[A], seed: B)(f: (A, B) => B): B =
      foldLeft(reverse(source), seed)((a, b) => f(b, a))

    def reverse[A](source: List[A]): List[A] =
      foldLeft(source, Nil: List[A])((accumulator, value) => Construct(value, accumulator))

    def append[A](source: List[A], elements: List[A]): List[A] =
      foldLeft(source, elements)((accumulator, value) => Construct(value, accumulator))

    def concat[A](source: List[List[A]]): List[A] = {
      reverse(foldLeft(source, Nil: List[A])((accumulator, value) => append(value, accumulator)))
    }

    def map[A, B](list: List[A])(f: A => B): List[B] =
      foldRight(list, Nil: List[B])((value, accumulator) => Construct(f(value), accumulator))

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = concat(map(list)(f))
  }

}

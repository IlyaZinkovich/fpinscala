package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class ListExercises extends FlatSpec with Matchers {

}

/*

// Exercise 3.1

val result = List(1, 2, 3, 4, 5) match {
  case Construct(x, Construct(2, Construct(4, _))) => x
  case Nil => 42
  case Construct(x, Construct(y, Construct(3, Construct(4, _)))) => x + y
  case Construct(h, t) => h + List.sum(t)
  case _ => 101
}

// Answer is 3

// Exercise 3.2

object List32 {

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Construct(_, Nil) => Nil
    case Construct(_, tail) => tail
  }
}

//List32.tail(List())
//List32.tail(List(1))
//List32.tail(List(1, 2))
//List32.tail(List(1, 2, 3))

// Exercise 3.3

object List33 {

  def setHead[A](newHead: A, list: List[A]): List[A] = list match {
    case Nil => Nil
    case Construct(head, tail) => Construct(newHead, tail)
  }
}

//List33.setHead(1, List())
//List33.setHead(2, List(1))
//List33.setHead(3, List(1, 2))

// Exercise 3.4

object List34 {

  def drop[A](list: List[A], n: Int): List[A] = {
    list match {
      case Nil => Nil
      case original@Construct(_, tail) =>
        if (n < 1) original
        else if (n == 1) tail
        else drop(tail, n - 1)
    }
  }
}

//List34.drop(List(), 1)
//List34.drop(List(1), 1)
//List34.drop(List(1, 2), 1)
//List34.drop(List(1, 2), 2)
//List34.drop(List(1, 2, 3), 1)
//List34.drop(List(1, 2, 3), 0)
//List34.drop(List(1, 2, 3), -1)
//List34.drop(List(), 0)
//List34.drop(List(), -1)

// Exercise 3.5

object List35 {

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
}

//val predicate: Int => Boolean = (number: Int) => number < 2
//List35.dropWhile(List(), predicate)
//List35.dropWhile(List(1), predicate)
//List35.dropWhile(List(1, 2), predicate)
//List35.dropWhile(List(1, 2, 3), predicate)
//List35.dropWhileWithTypeInference(List(1, 2, 3))(_ < 2)

object List35_ {

  def append[A](target: List[A], elements: List[A]): List[A] = target match {
    case Nil => elements
    case Construct(head, tail) => Construct(head, append(tail, elements))
  }
}

List35_.append(List(1), List(2, 3, 4))

// Exercise 3.6

object List36 {

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
}

List36.init(List(1, 2, 3))

object List3_ {

  def foldRight[A, B](source: List[A], initialValue: B)(combine: (A, B) => B): B = source match {
    case Nil => initialValue
    case Construct(head, tail) => combine(head, foldRight(tail, initialValue)(combine))
  }

  def sum(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

  def product(doubles: List[Double]): Double = foldRight(doubles, 1.0)(_ * _)
}

List3_.sum(List(1, 2, 3, 4))
List3_.product(List(1, 2, 3, 4))

// Exercise 3.7

// No, this is not possible! The reason is because _before_ we ever call our function, `f`, we evaluate its argument, which in the case of `foldRight` means traversing the list all the way to the end. We need _non-strict_ evaluation to support early termination---we discuss this in chapter 5.

// Exercise 3.8

// We get back the original list
List3_.foldRight(List(1, 2, 3), Nil: List[Int])(Construct(_, _))

// Exercise 3.9

object List39 {

  def foldRight[A, B](source: List[A], seed: B)(f: (A, B) => B): B = source match {
    case Nil => seed
    case Construct(head, tail) => f(head, foldRight(tail, seed)(f))
  }

  def length[A](source: List[A]): Int = foldRight(source, 0)((_, accumulator) => accumulator + 1)
}

List39.length(List())
List39.length(List(1))
List39.length(List(1, 2, 3))

// Exercise 3.10

object List310 {

  @tailrec
  def foldLeft[A, B](source: List[A], accumulator: B)(f: (B, A) => B): B = source match {
    case Nil => accumulator
    case Construct(head, tail) => foldLeft(tail, f(accumulator, head))(f)
  }
}

List310.foldLeft(List(1, 2, 3, 4), 0)(_ + _)
List310.foldLeft(List(1, 2, 3, 4), 1.0)(_ * _)

// Exercise 3.11

object List311 {

  @tailrec
  def foldLeft[A, B](source: List[A], accumulator: B)(f: (B, A) => B): B = source match {
    case Nil => accumulator
    case Construct(head, tail) => foldLeft(tail, f(accumulator, head))(f)
  }

  def length[A](source: List[A]): Int = foldLeft(source, 0)((accumulator, _) => accumulator + 1)

  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product(doubles: List[Double]): Double = foldLeft(doubles, 1.0)(_ * _)
}

List311.sum(List(1, 2, 3, 4))
List311.product(List(1, 2, 3, 4))
List311.length(List(1, 2, 3, 4))

// Exercise 3.12

object List312 {

  @tailrec
  def foldLeft[A, B](source: List[A], accumulator: B)(f: (B, A) => B): B = source match {
    case Nil => accumulator
    case Construct(head, tail) => foldLeft(tail, f(accumulator, head))(f)
  }

  def reverse[A](source: List[A]): List[A] =
    foldLeft(source, Nil: List[A])((accumulator, value) => Construct(value, accumulator))
}

List312.reverse(List(1, 2, 3, 4))


// Exercise 3.13

object List313 {

  @tailrec
  def foldLeft[A, B](source: List[A], accumulator: B)(f: (B, A) => B): B = source match {
    case Nil => accumulator
    case Construct(head, tail) => foldLeft(tail, f(accumulator, head))(f)
  }

  def reverse[A](source: List[A]): List[A] =
    foldLeft(source, Nil: List[A])((accumulator, value) => Construct(value, accumulator))

  def foldRight[A, B](source: List[A], seed: B)(f: (A, B) => B): B =
    foldLeft(reverse(source), seed)((a, b) => f(b, a))
}

List313.foldRight(List(1, 2, 3, 4), 0)(_ + _)

// Exercise 3.14

object List314 {

  @tailrec
  def foldLeft[A, B](source: List[A], accumulator: B)(f: (B, A) => B): B = source match {
    case Nil => accumulator
    case Construct(head, tail) => foldLeft(tail, f(accumulator, head))(f)
  }

  def append[A](source: List[A], elements: List[A]): List[A] =
    foldLeft(source, elements)((accumulator, value) => Construct(value, accumulator))
}

List314.append(List(1), List(2, 3, 4))

// Exercise 3.15

object List315 {

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
}

List315.concat(List(List(1), List(2, 3, 4), List(5, 6)))

 */
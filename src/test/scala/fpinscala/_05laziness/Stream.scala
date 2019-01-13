package fpinscala._05laziness

import scala.collection.mutable.ListBuffer

sealed trait Stream[+A] {

  import Stream._

  def toList: List[A] = {
    val list = ListBuffer[A]()

    def loop(stream: Stream[A]): List[A] = stream match {
      case Empty => list.toList
      case Cons(head, tail) => list.append(head()); loop(tail())
    }

    loop(this)
  }

  def toListStackUnsafe: List[A] = this match {
    case Empty => Nil
    case Cons(head, tail) => head() :: tail().toListStackUnsafe
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(head, tail) if n > 1 => cons(head(), tail().take(n - 1))
    case Cons(head, _) if n == 1 => cons(head(), empty)
    case _ => this
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, tail) if n > 0 => tail().drop(n - 1)
    case _ => this
  }

  def takeWhile(predicate: A => Boolean): Stream[A] = this match {
    case Cons(head, tail) if predicate(head()) => cons(head(), tail().takeWhile(predicate))
    case _ => Empty
  }

  def foldRight[B](seed: B)(f: (A, => B) => B): B = this match {
    case Empty => seed
    case Cons(head, tail) => f(head(), tail().foldRight(seed)(f))
  }

  def exists(predicate: A => Boolean): Boolean = {
    foldRight(false)((a, b) => predicate(a) || b)
  }

  def forAll(predicate: A => Boolean): Boolean = {
    foldRight(true)((a, b) => predicate(a) && b)
  }

  def takeWhileViaFoldRight(predicate: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (predicate(a)) cons(a, b) else empty)

  def headOption(): Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(predicate: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (predicate(a)) cons(a, b) else b)

  def append[B >: A](stream: Stream[B]): Stream[B] =
    foldRight(stream)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((a, b) => f(a).append(b))
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(head, tail) => Some((f(head()), tail()))
    case Empty => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(head, tail), number) if number > 0 => Some((head(), (tail(), number - 1)))
    case _ => None
  }

  def takeWhileViaUnfold(predicate: A => Boolean): Stream[A] = unfold(this) {
    case Cons(head, tail) if predicate(head()) => Some((head(), tail()))
    case _ => None
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(head1, tail1), Cons(head2, tail2)) => Some((f(head1(), head2()), (tail1(), tail2())))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(head1, tail1), Cons(head2, tail2)) =>
      Some((Some(head1()), Some(head2())), (tail1(), tail2()))
    case (Cons(head1, tail1), Empty) =>
      Some((Some(head1()), None), (tail1(), Empty))
    case (Empty, Cons(head2, tail2)) =>
      Some((None, Some(head2())), (Empty, tail2()))
    case (Empty, Empty) => None
  }

  def startsWith[A](other: Stream[A]): Boolean = {
    zipAll(other).takeWhile(_._2.isDefined).forAll {
      case (left, right) => left == right
    }
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case state => Some(state, state drop 1)
  } append Stream(empty)


  def scanRight[B](seed: B)(f: (A, => B) => B): Stream[B] =
    foldRight((seed, Stream(seed))) { (streamElement, accumulator) =>
      lazy val cachedAccumulator = accumulator
      val result = f(streamElement, cachedAccumulator._1)
      (result, cons(result, cachedAccumulator._2))
    }._2
}

case object Empty extends Stream[Nothing]

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val cachedHead = head
    lazy val cachedTail = tail
    Cons(() => cachedHead, () => cachedTail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def unfold[A, S](seed: S)(generator: S => Option[(A, S)]): Stream[A] = {
    generator(seed) match {
      case Some((element, state)) => cons(element, unfold(state)(generator))
      case None => empty
    }
  }
}

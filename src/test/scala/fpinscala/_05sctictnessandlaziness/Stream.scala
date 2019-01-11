package fpinscala._05sctictnessandlaziness

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
}

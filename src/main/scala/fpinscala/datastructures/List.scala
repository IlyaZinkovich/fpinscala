package fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Construct[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Construct(as.head, apply(as.tail: _*))
  }
}

package fpinscala._13io

import fpinscala._07parallelism.Par.Par

sealed trait Free[F[_], A] {

  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMapFree(this, f)

  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (ReturnFree(_)))
}

case class ReturnFree[F[_], A](a: A) extends Free[F, A]

case class SuspendFree[F[_], A](resume: F[A]) extends Free[F, A]

case class FlatMapFree[F[_], A, B](sub: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {

  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]
}
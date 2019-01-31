package fpinscala._13io

import fpinscala._07parallelism.Par
import fpinscala._07parallelism.Par.Par

trait AsyncIO[A] {

  def map[B](f: A => B): AsyncIO[B] =
    flatMap(f andThen (ReturnAsync(_)))

  def flatMap[B](f: A => AsyncIO[B]): AsyncIO[B] =
    FlatMapAsync(this, f)
}

case class ReturnAsync[A](a: A) extends AsyncIO[A]

case class SuspendAsync[A](resume: Par[A]) extends AsyncIO[A]

case class FlatMapAsync[A, B](sub: AsyncIO[A], f: A => AsyncIO[B]) extends AsyncIO[B]

object AsyncIO {

  @annotation.tailrec
  def step[A](async: AsyncIO[A]): AsyncIO[A] = async match {
    case FlatMapAsync(FlatMapAsync(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMapAsync(ReturnAsync(x), f) => step(f(x))
    case _ => async
  }

  def run[A](async: AsyncIO[A]): Par[A] = step(async) match {
    case ReturnAsync(a) => Par.unit(a)
    case SuspendAsync(r) => r
    case FlatMapAsync(x, f) => x match {
      case SuspendAsync(r) => Par.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible, since `step` eliminates these cases")
    }
  }
}
package fpinscala._13io

import fpinscala._12applicative.Monad

trait TailRec[A] {

  def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))

  def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
}

case class Return[A](a: A) extends TailRec[A]

case class Suspend[A](resume: () => A) extends TailRec[A]

case class FlatMap[A, B](sub: TailRec[A], f: A => TailRec[B]) extends TailRec[B]

object TailRec extends Monad[TailRec] {

  def unit[A](a: => A): TailRec[A] = Return(a)

  def flatMap[A, B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] = a flatMap f

  def suspend[A](a: => TailRec[A]) =
    Suspend(() => ()).flatMap { _ => a }

  def forever[A, B](a: TailRec[A]): TailRec[B] = {
    lazy val b: TailRec[B] = a.flatMap(_ => forever(a))
    a flatMap (_ => b)
  }

  @annotation.tailrec def run[A](io: TailRec[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }
}

object TailRecMain extends App {

  val p = TailRec.forever(printLine("Still going..."))

  def printLine(s: String): TailRec[Unit] = Suspend(() => Return(println(s)))

  TailRec.run(p)
}

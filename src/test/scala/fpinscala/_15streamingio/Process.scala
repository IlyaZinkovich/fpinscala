package fpinscala._15streamingio

sealed trait Process[I, O] {

  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(receive) => s match {
      case h #:: t => receive(Some(h))(t)
      case xs => receive(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(receive) => Await {
        case None => receive(None)
        case i => go(receive(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }

    go(this)
  }

  def filter[I](p: I => Boolean): Process[I, I] = Await[I, I] {
    case Some(i) if p(i) => Emit(i)
    case _ => Halt()
  }.repeat


  def take[I](n: Int): Process[I, I] =
    if (n < 0) Halt()
    else Await {
      case Some(i) => Emit[I, I](i, take[I](n - 1))
      case None => Halt()
    }

  def drop[I](n: Int): Process[I, I] =
    if (n <= 0) Process.id
    else Await {
      case Some(_) => drop[I](n - 1)
      case None => Halt()
    }

  def takeWhile[I](f: I => Boolean): Process[I, I] = Await {
    case Some(value) => if (f(value)) Emit(value, takeWhile(f)) else Halt()
    case None => Halt()
  }

  def dropWhile[I](f: I => Boolean): Process[I, I] = Await {
    case Some(value) => if (f(value)) dropWhile(f) else Emit(value, Process.id)
    case None => Halt()
  }

  def count[I]: Process[I, Int] = {
    def go(n: Int): Process[I, Int] = Await {
      case Some(value) => Emit(n + 1, go(n + 1))
      case None => Halt()
    }

    go(0)
  }
}

object Process {

  def liftOne[I, O](f: I => O): Process[I, O] = Await {
    case Some(i) => Emit(f(i))
    case None => Halt()
  }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def id[I]: Process[I, I] = lift(identity)
}

case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]

case class Await[I, O](receive: Option[I] => Process[I, O]) extends Process[I, O]

case class Halt[I, O]() extends Process[I, O]

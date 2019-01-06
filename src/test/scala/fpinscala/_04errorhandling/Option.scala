package fpinscala._04errorhandling

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)) getOrElse ob
  }

  def filter(predicate: A => Boolean): Option[A] = {
    flatMap(value => if (predicate(value)) Some(value) else None)
  }
}

case class Some[+A](a: A) extends Option[A]

case object None extends Option[Nothing]

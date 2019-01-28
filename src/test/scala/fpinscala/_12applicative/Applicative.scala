package fpinscala._12applicative

import fpinscala._11monad.Functor

trait Applicative[F[_]] extends Functor[F] {

  def unit[A](a: A): F[A]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((elem, acc) => map2(f(elem), acc)(_ :: _))

  def sequence[A](as: List[F[A]]): F[List[A]] = traverse(as)(identity)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
}

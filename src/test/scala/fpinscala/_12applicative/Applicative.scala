package fpinscala._12applicative

import fpinscala._11monad.Functor

trait Applicative[F[_]] extends Functor[F] {

  def unit[A](a: => A): F[A]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fa, fab)((a, fa) => fa(a))

  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((elem, acc) => map2(f(elem), acc)(_ :: _))

  def sequence[A](as: List[F[A]]): F[List[A]] = traverse(as)(identity)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def map2ViaApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
        (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f))
    }
  }

  def sequenceMap[K, V](mfv: Map[K, F[V]]): F[Map[K, V]] = mfv.foldLeft(unit(Map.empty[K, V])) {
    case (acc: F[Map[K, V]], (k: K, fv: F[V])) => map2(fv, acc)((v, m) => m + (k -> v))
  }
}

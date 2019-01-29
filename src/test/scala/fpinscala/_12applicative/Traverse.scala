package fpinscala._12applicative

import fpinscala._06state.State
import fpinscala._10monoid.Monoid
import fpinscala._11monad.{Functor, Id}

trait Traverse[F[_]] extends Functor[F] {
  self =>

  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    val idApplicative = new Applicative[Id] {
      def unit[A](a: => A): Id[A] = Id(a)

      def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = Id(f(fa.value, fb.value))
    }
    traverse[Id, A, B](fa)(a => Id(f(a)))(idApplicative, idApplicative).value
  }

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]]

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({type f[x] = Const[M, x]})#f] {
    def unit[A](a: => A): M = M.zero

    def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)
  }

  def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({type f[x] = Const[M, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb), monoidApplicative(mb))

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State.unit(a)

    def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa.flatMap(f)
  }

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(stateMonad, stateMonad)

  def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta)((a: A) => for {
      i <- State.get[Int]
      _ <- State.set(i + 1)
    } yield (a, i)).run(0)._1

  def toList[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => for {
      as <- State.get[List[A]]
      _ <- State.set(a :: as)
    } yield ()).run(Nil)._2.reverse

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = traverseS(fa)((a: A) => for {
    s1 <- State.get[S]
    (b, s2) = f(a, s1)
    _ <- State.set(s2)
  } yield b).run(s)

  def toList_[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex_[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList_(fa).reverse)((a, s) => (s.head, s.tail))._1

  def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H, G product H)

//  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f]

//  def composeM[F[_],G[_]](F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f]
}

package fpinscala._12applicative

import fpinscala._11monad.Functor

trait Monad[F[_]] extends Functor[F] with Applicative[F] {

  def unit[A](a: => A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    map(traverse(ms) { a =>
      map(f(a))(b => (a, b))
    })(list => list.filter(tuple => tuple._2).map(tuple => tuple._1))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(b => g(b))

  def flatMapViaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => fa, f)(())

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)

  def flatMapViaJoinAndMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def composeViaJoinAndMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))((b: B) => g(b)))

//  def compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = {
//    val self = this
//    new Monad[({type f[x] = F[G[x]]})#f] {
//      def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
//
//      def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
//        self.flatMap(fa)((ga: G[A]) => G.flatMap(ga)(a => f(a)))
//    }
//  }
}

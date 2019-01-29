package fpinscala._12applicative

import org.scalatest.{FlatSpec, Matchers}

class TraverseExercises extends FlatSpec with Matchers {

  private val listTraverse = new Traverse[List] {
    def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List[B]()))((elem, acc) => G.map2(f(elem), acc)(_ :: _))
  }

  "Exercise 12.13" should "traverse List, Option, Tree" in {
    new Traverse[List] {
      def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
        fa.foldRight(G.unit(List[B]()))((elem, acc) => G.map2(f(elem), acc)(_ :: _))
    }
    new Traverse[Option] {
      def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
        fa match {
          case Some(a) => G.map(f(a))(Some(_))
          case None => G.unit(None)
        }
    }
  }

  "Exercise 12.14" should "traverse map" in {
    listTraverse.map(List(1, 2, 3))(_ * 2) should be(List(2, 4, 6))
  }

  "Exercise 12.15" should "why Foldable isn’t a functor" in {
    /*
     It's because `foldRight`, `foldLeft`, and `foldMap`
     don't give us any way of constructing a value of the foldable type.
     In order to `map` over a structure, you need the ability to create a new structure
     (such as `Nil` and `Cons` in the case of a `List`).
     `Traverse` is able to extend `Functor` precisely because a traversal preserves the original structure.

     An example of a Foldable that is not a functor:

     case class Iteration[A](a: A, f: A => A, n: Int) {
       def foldMap[B](g: A => B)(M: Monoid[B]): B = {
         def iterate(n: Int, b: B, c: A): B =
           if (n <= 0) b else iterate(n-1, g(c), f(a))
         iterate(n, M.zero, a)
       }
     }
   */
  }

  "Exercise 12.16" should "reverse" in {

  }

  "Exercise 12.17" should "foldLeft" in {

  }

  "Exercise 12.18" should "fuse" in {

  }

  "Exercise 12.19" should "compose" in {

  }

  "Exercise 12.20" should "composeM" in {

  }
}

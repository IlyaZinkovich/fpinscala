package fpinscala._11monad

import fpinscala._05laziness.Stream
import fpinscala._06state.State
import fpinscala._07parallelism.Par
import fpinscala._07parallelism.Par.Par
import org.scalatest.{FlatSpec, Matchers}

class MonadExercises extends FlatSpec with Matchers {

  "Exercise 11.1" should "Monad for Par, Option, Stream, and List" in {
    new Monad[Par] {
      def unit[A](a: => A): Par[A] = Par.unit(a)

      def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(fa)(f)
    }

    new Monad[Option] {
      def unit[A](a: => A): Option[A] = Some(a)

      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }

    new Monad[Stream] {
      def unit[A](a: => A): Stream[A] = Stream(a)

      def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] = fa.flatMap(f)
    }

    new Monad[List] {
      def unit[A](a: => A): List[A] = List(a)

      def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    }
  }

  "Exercise 11.2" should "Monad for State" in {
    def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
      def unit[A](a: => A): State[S, A] = State.unit(a)

      def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa.flatMap(f)
    }
  }

  "Exercise 11.3" should "sequence and traverse" in {
    val optionMonad = new Monad[Option] {
      def unit[A](a: => A): Option[A] = Some(a)

      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }
    optionMonad.sequence(List(Some(1), None)) should be(None)
    optionMonad.sequence(List(Some(1), Some(2))) should be(Some(List(1, 2)))
    optionMonad.traverse(List(1, 2))(Some(_)) should be(Some(List(1, 2)))
  }

  "Exercise 11.4" should "replicateM" in {
    val optionMonad = new Monad[Option] {
      def unit[A](a: => A): Option[A] = Some(a)

      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }
    optionMonad.replicateM(3, Some(1)) should be(Some(List(1, 1, 1)))
  }

  "Exercise 11.5" should "replicateM meaning" in {
    val optionMonad = new Monad[Option] {
      def unit[A](a: => A): Option[A] = Some(a)

      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }

    val listMonad = new Monad[List] {
      def unit[A](a: => A): List[A] = List(a)

      def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    }
    /* Generates list of all possible combinations of length n with elements from the input list */
    listMonad.replicateM(2, List(1, 2)) should be(List(List(1, 1), List(1, 2), List(2, 1), List(2, 2)))
    /* Generates Some or None depending on the input Option.
       Some will contain a list containing n equal elements of input Option
     */
    optionMonad.replicateM(3, Some(1)) should be(Some(List(1, 1, 1)))
    /* In general replicateM repeats the `ma` monadic value `n` times
       and gathers the results in a single value,
       where the monad `F` determines how values are actually combined.
     */
  }

  "Exercise 11.6" should "replicateM meaning" in {
    val optionMonad = new Monad[Option] {
      def unit[A](a: => A): Option[A] = Some(a)

      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }

    optionMonad.filterM(List(1, 2, 3))(a => if (a < 2) Some(true) else None) should be(None)
    optionMonad.filterM(List(1, 2, 3))(a => if (a < 2) Some(true) else Some(false)) should be(Some(List(1)))
  }

  "Exercise 11.7" should "Kleisli composition" in {
    val optionMonad = new Monad[Option] {
      def unit[A](a: => A): Option[A] = Some(a)

      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }

    optionMonad.compose[Int, Int, Int](a => Some(a + 2), a => Some(a / 2))(4) should be(Some(3))
  }

  "Exercise 11.8" should "flatMap via compose" in {
    val optionMonad = new Monad[Option] {
      def unit[A](a: => A): Option[A] = Some(a)

      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }

    optionMonad.flatMapViaCompose(Some(2))(a => if (a < 3) Some(a) else None) should be(Some(2))
    optionMonad.flatMapViaCompose(Some(3))(a => if (a < 3) Some(a) else None) should be(None)
  }

  "Exercise 11.9" should "flatMap and compose associativity proofs are equivalent" in {
    /*
      compose(compose(f, g), h) == compose(f, compose(g, h))
      a => flatMap(compose(f, g)(a))(b => h(b)) == a => flatMap(f(a))(b => compose(g, h)(b))
      a => flatMap(a => flatMap(f(a))(g)(a))(h) == a => flatMap(f(a))(b => flatMap(g(b))(h))
      a => flatMap(flatMap(f(a))(g)))(h) == a => flatMap(f(a))(b => flatMap(g(b))(h))
      flatMap(flatMap(x)(f)))(g) == flatMap(x)(a => flatMap(f(a))(g))
     */
  }

  "Exercise 11.10" should "flatMap and compose identity proofs are equivalent" in {
    /*
      compose(f, unit) == compose(unit, f) == f

      flatMap(f)(unit) == f
      flatMap(unit(y))(f) == f(y)

      compose(f, unit)(v) == f(v)
      a => flatMap(f(a))(b => unit(b))(v) == f(v)
      flatMap(f(v))(unit) == f(v)
      flatMap(x)(unit) == x

      compose(unit, f)(y) == f(y)
      flatMap(unit(y))(f) == f(y)
     */
  }

  "Exercise 11.11" should "identity laws hold for Option monad" in {
    /*
      flatMap(f)(unit) == f
      flatMap(None)(Some(_)) == None

      flatMap(Some(v))(Some(_)) == Some(v)
      Some(v) == Some(v)

      flatMap(Some(None))(f) == f(None)
      f(None) == f(None)

      flatMap(Some(Some(y)))(f) == f(Some(y))
      f(Some(y)) == f(Some(y))
     */
  }

  "Exercise 11.12" should "join" in {
    val optionMonad = new Monad[Option] {
      def unit[A](a: => A): Option[A] = Some(a)

      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }

    optionMonad.join(Some(Some(1))) should be(Some(1))
  }

  "Exercise 11.13" should "flatMap and compose via join and map" in {
    val optionMonad = new Monad[Option] {
      def unit[A](a: => A): Option[A] = Some(a)

      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }

    optionMonad.flatMapViaJoinAndMap(Some(1))(a => Some(a * 2)) should be(Some(2))
  }

  "Exercise 11.14" should "monad laws via join and map" in {
    /*
      join(map(x)(unit)) == x
      join(unit(x)) == x

      x.flatMap(z => z).flatMap(z => z) == x.flatMap(a => a.flatMap(z => z))
      join(join(x)) == x.flatMap(join)
      join(join(x)) == join(map(x)(join))
     */
  }

  "Exercise 11.15" should "associativity for Par and Parser" in {
    /*
      join(join(x)) == join(map(x)(join))

      For `Par`, the `join` combinator means something like
      "make the outer thread wait for the inner one to finish."
      What this law is saying is that if you have threads starting threads three levels deep,
      then joining the inner threads and then the outer ones is the same
      as joining the outer threads and then the inner ones.

      For `Parser`, the `join` combinator is running the outer parser to produce a `Parser`,
      then running the inner `Parser` _on the remaining input_.
      The associative law is saying, roughly, that only the _order_ of nesting matters,
      since that's what affects the order in which the parsers are run.
     */
  }

  "Exercise 11.16" should "identity for List and Gen" in {
    /*
      The left identity law for `Gen`:
      The law states that if you take the values generated by `unit(x)` (which are always `x`)
      and apply `f` to those values, that's exactly the same as the generator returned by `f(x)`.

      The right identity law for `Gen`:
      The law states that if you apply `unit` to the values inside the generator `x`,
      that does not in any way differ from `x` itself.

      The left identity law for `List`:
      The law says that wrapping a list in a singleton `List`
      and then flattening the result is the same as doing nothing.

      The right identity law for `List`:
      The law says that if you take every value in a list, wrap each one in a singleton `List`,
      and then flatten the result, you get the list you started with.
     */
  }
}

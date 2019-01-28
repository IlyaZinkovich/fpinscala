package fpinscala._11monad

import fpinscala._05laziness.Stream
import fpinscala._06state.{LinearCongruentalRNG, RNG, State}
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

  "Exercise 11.17" should "Id monad" in {
    new Monad[Id] {
      def unit[A](a: => A): Id[A] = Id(a)

      def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(f)
    }
  }

  "Exercise 11.18" should "State monad replicateM, map2 and sequence meaning" in {
    def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))

      def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
        State(s => {
          val (a, s1) = fa.run(s)
          val (b, s2) = f(a).run(s1)
          (b, s2)
        })
    }

    val rng = LinearCongruentalRNG(1)
    val rngState: State[RNG, Int] = State(rng => rng.nextInt)
    // Applies n state changes sequentially, passing along the state and combining results in a list
    stateMonad.replicateM(3, rngState).run(rng)._1 should be(List(384748, -1151252339, -549383847))
    val fixedState = stateMonad[RNG].unit[Int](10)
    // Combines two results of state transitions with a provided function passing the state from one state transition to another
    stateMonad.map2(rngState, fixedState)(_ + _).run(rng)._1 should be(384758)
    // Passes the state through the list of state transitions collecting the results in a list
    stateMonad.sequence(List(rngState, fixedState)).run(rng)._1 should be(List(384748, 10))

    /*
      `replicateM` for `State` repeats the same state transition a number of times
      and returns a list of the results.
      It's not passing the same starting state many times,
      but chaining the calls together so that
      the output state of one is the input state of the next.

      `map2` works similarly in that it takes two state transitions
      and feeds the output state of one to the input of the other.
      The outputs are not put in a list, but combined with a function `f`.

      `sequence` takes an entire list of state transitions and does the same kind of thing as `replicateM`:
      it feeds the output state of the first state transition to the input state of the next, and so on.
      The results are accumulated in a list.

     */
  }

  "Exercise 11.19" should "laws that mutually hold for getState, setState, unit, and flatMap" in {
    /*

      Getting and setting the same state does nothing:
      getState.flatMap(setState) == unit(())

      written as for-comprehension:
      for {
        x <- getState
        _ <- setState(x)
      } yield ()

      Setting the state to `s` and getting it back out yields `s`.
      setState(s).flatMap(_ => getState) == unit(s)

      alternatively:
      for {
        _ <- setState(s)
        x <- getState
      } yield x
     */
  }

  /*
      We can see that a chain of flatMap calls (or an equivalent for-comprehension)
      is like an imperative program with statements that assign to variables and
      !!!the monad specifies what occurs at statement boundaries!!!.
      For example, with Id, nothing at all occurs except unwrapping and re-wrapping in the Id constructor.
      With State, the most current state gets passed from one statement to the next.
      With the Option monad, a statement may return None and terminate the program.
      With the List monad, a statement may return many results,
      which causes statements that follow it to potentially run multiple times, once for each result.

      The Monad contract doesn’t specify what is happening between the lines,
      only that whatever is happening satisfies the laws of associativity and identity.
   */

  "Exercise 11.20" should "Reader monad" in {
    def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
      def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

      /*
         The action of Reader's `flatMap` is to pass the `r` argument along to both the
         outer Reader and also to the result of `f`, the inner Reader. Similar to how
         `State` passes along a state, except that in `Reader` the "state" is read-only.
       */
      def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
        Reader(r => f(fa.run(r)).run(r))
    }

    /*
       The meaning of `sequence` here is that if you have a list of functions, you can
       turn it into a function that takes one argument and passes it to all the functions
       in the list, returning a list of the results.
     */

    /*
       The meaning of `join` is simply to pass the same value as both arguments to a
       binary function.
     */

    /*
       The meaning of `replicateM` is to apply the same function a number of times to
       the same argument, returning a list of the results. Note that if this function
       is _pure_, (which it should be), this can be exploited by only applying the
       function once and replicating the result instead of calling the function many times.
       This means the Reader monad can override replicateM to provide a very efficient
       implementation.
     */
  }
}

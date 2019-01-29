package fpinscala._12applicative

import org.scalatest.{FlatSpec, Matchers}

class ApplicativeExercises extends FlatSpec with Matchers {

  private val optionApplicative = new Applicative[Option] {
    def unit[A](a: => A): Option[A] = Some(a)

    def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = (fa, fb) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }
  }

  private def listApplicative = new Applicative[List] {
    def unit[A](a: => A): List[A] = List(a)

    def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] =
      fa.flatMap(a => fb.map(b => f.apply(a, b)))
  }

  "Exercise 12.1" should "sequence, replicateM and product" in {
    optionApplicative.sequence(List(Some(1), Some(2))) should be(Some(List(1, 2)))
    optionApplicative.sequence(List(Some(1), None)) should be(None)
    optionApplicative.replicateM(2, Some(1)) should be(Some(List(1, 1)))
    optionApplicative.replicateM(2, None) should be(None)
    optionApplicative.product(Some(1), Some(2)) should be(Some((1, 2)))
    optionApplicative.product(Some(1), None) should be(None)
  }

  "Exercise 12.2" should "apply via map, map via apply, map2 via apply" in {
    optionApplicative.apply(Some[Int => Int](_ * 2))(Some(1)) should be(Some(2))
    optionApplicative.mapViaApply(Some(1))(_ * 2) should be(Some(2))
    optionApplicative.map2ViaApply(Some(1), Some(2))(_ * _) should be(Some(2))
  }

  "Exercise 12.3" should "map3 and map4" in {
    optionApplicative.map3(Some(1), Some(2), Some(3))(_ * _ * _) should be(Some(6))
    optionApplicative.map4(Some(1), Some(2), Some(3), Some(4))(_ * _ * _ * _) should be(Some(24))
  }

  "Exercise 12.4" should "stream sequence" in {
    val streamApplicative = new Applicative[Stream] {
      def unit[A](a: => A): Stream[A] =
        Stream.continually(a)

      def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
        a zip b map f.tupled
    }

    /*
      Sequence transposes the list!
      That is, we start with a list of rows, each of which is possibly infinite in length.
      We get back a single row, where each element is the column of values at that position.
     */
    streamApplicative.sequence(List(Stream(1, 2, 3), Stream(4, 5), Stream(6, 7, 8))).toList should be(List(List(1, 4, 6), List(2, 5, 7)))
  }

  "Exercise 12.5" should "either monad" in {
    def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)

      def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
        case Right(value) => f(value)
        case Left(value) => Left(value)
      }
    }

    val eithers = List(Right(1), Left("error1"), Left("error2"))
    eitherMonad[String].sequence(eithers) should be(Left("error1"))
  }

  "Exercise 12.6" should "validation applicative" in {
    def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
      def unit[A](a: => A): Validation[E, A] = Success(a)

      def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (failure@Failure(_, _), Success(_)) => failure
        case (Success(_), failure@Failure(_, _)) => failure
        case (Failure(headA, tailA), Failure(headB, tailB)) => Failure(headA, tailA ++ Vector(headB) ++ tailB)
      }
    }

    val validations = List(Success(1), Failure("error1", Vector()), Failure("error2", Vector()))
    validationApplicative[String].sequence(validations) should be(Failure("error1", Vector("error2")))
  }

  "Exercise 12.7" should "prove monads are applicatives" in {
    /*
      map2(unit(()), fa)((_,a) => a) == fa // Left identity
      flatMap(unit())(u => map(fa)(a => a)) == fa
      flatMap(unit())(u => fa) == fa
      compose(unit, u => fa)(()) == fa
      compose(unit, f) == f
      (u => fa)(()) == fa
      fa == fa

      map2(fa, unit(()))((a,_) => a) == fa // Right identity
      flatMap(fa)(a => map(unit(()))(u => a)) == fa
      flatMap(fa)(a => unit(a)) == fa  // via functor laws
      compose(u => fa, unit)(()) == fa
      (u => fa)(()) == fa
      fa == fa

      Associativity and naturality are TBD.
     */
  }

  "Exercise 12.8" should "applicatives product" in {
    val optionListApplicative = optionApplicative.product(listApplicative)
    optionListApplicative.sequence(List((Some(1), List(2)), (None, List(3)))) should be((None, List(List(2, 3))))
  }

  "Exercise 12.9" should "applicatives compose" in {
    val optionListApplicative = optionApplicative.compose(listApplicative)
    optionListApplicative.sequence(List(Some(List(1, 2)), Some(List(3)))) should be(Some(List(List(1, 3), List(2, 3))))
  }

  "Exercise 12.10" should "composite applicative satisfies the laws" in {
    /*
      https://github.com/runarorama/sannanir/blob/master/Applicative.v
     */
  }

  "Exercise 12.11" should "try compose on monads" in {
    /*
      def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        self.flatMap(fa)((ga: G[A]) => G.flatMap(ga)(a => f(a)))

      f(a) doesn't fit the G.flatMap function
     */
  }

  "Exercise 12.12" should "sequenceMap" in {
    optionApplicative.sequenceMap(Map(1 -> Some(1), 2 -> None)) should be(None)
    optionApplicative.sequenceMap(Map(1 -> Some(1), 2 -> Some(2))) should be(Some(Map(1 -> 1, 2 -> 2)))
  }
}

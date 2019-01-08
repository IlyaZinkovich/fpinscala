package fpinscala._04errorhandling

import org.scalatest.{FlatSpec, Matchers}

class EitherExercises extends FlatSpec with Matchers {

  "Exercise 4.6" should "map flatMap orElse and map2" in {
    val successEither: Either[RuntimeException, String] = Right("value")
    val errorEither: Either[RuntimeException, String] = Left(new RuntimeException("error"))
    val modifiedSuccessEither: Either[RuntimeException, String] = Right("VALUE")

    successEither.map(str => str.toUpperCase) should be(modifiedSuccessEither)
    errorEither.map(str => str.toUpperCase) should be(errorEither)

    successEither.flatMap(str => Right(str.toUpperCase)) should be(modifiedSuccessEither)
    errorEither.flatMap(str => Right(str.toUpperCase)) should be(errorEither)

    successEither.orElse(errorEither) should be(successEither)
    errorEither.orElse(successEither) should be(successEither)

    successEither.map2(modifiedSuccessEither)(_ ++ _) should be(Right("valueVALUE"))
    successEither.map2(errorEither)(_ ++ _) should be(errorEither)
    errorEither.map2(successEither)(_ ++ _) should be(errorEither)
  }

  "Exercise 4.7" should "sequence and traverse" in {
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
      traverse(es)(x => x)
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      as.foldRight[Either[E, List[B]]](Right(Nil)) { (element, accumulator) =>
        f(element).map2(accumulator)(_ :: _)
      }
    }

    val successEither = Right("success")
    val exception = new RuntimeException("error")
    val errorEither = Left(exception)
    sequence(List(errorEither)) should be(errorEither)
    sequence(List(successEither)) should be(Right(List("success")))
    sequence(List(successEither, errorEither)) should be(errorEither)
    sequence(List(errorEither, successEither)) should be(errorEither)
    sequence(List(successEither, successEither)) should be(Right(List("success", "success")))
    sequence(List(errorEither, errorEither)) should be(Left(exception))

    val function: String => Either[RuntimeException, String] = (str: String) => {
      str match {
        case "success" => successEither
        case "error" => errorEither
      }
    }
    traverse(List("error"))(function) should be(errorEither)
    traverse(List("success"))(function) should be(Right(List("success")))
    traverse(List("success", "error"))(function) should be(errorEither)
    traverse(List("error", "success"))(function) should be(errorEither)
    traverse(List("success", "success"))(function) should be(Right(List("success", "success")))
    traverse(List("error", "error"))(function) should be(errorEither)
  }

  "Exercise 4.8" should "aggregating error handling approach" in {
    /*
    Personal Experience:
    Use Either[NonEmptyList[E], A], Validated[NonEmptyList[E], A] or ValidatedNel[E, A]

    Book Answer:
    There are a number of variations on `Option` and `Either`. If we want to accumulate multiple errors, a simple approach is a new data type that lets us keep a list of errors in the data constructor that represents failures:
    trait Partial[+A,+B]
    case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
    case class Success[+B](get: B) extends Partial[Nothing,B]
    There is a type very similar to this called `Validation` in the Scalaz library. You can implement `map`, `map2`, `sequence`, and so on for this type in such a way that errors are accumulated when possible (`flatMap` is unable to accumulate errors--can you see why?). This idea can even be generalized further--we don't need to accumulate failing values into a list; we can accumulate values using any user-supplied binary function.
    It's also possible to use `Either[List[E],_]` directly to accumulate errors, using different implementations of helper functions like `map2` and `sequence`.
    */
  }
}

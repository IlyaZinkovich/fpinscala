package fpinscala._09parsing

import org.scalatest.{FlatSpec, Matchers}

class ParsersExercises extends FlatSpec with Matchers {

  "Exercise 9.1" should "map2 via product and many1 via map2" in {
  }

  "Exercise 9.2" should "product laws" in {
    /*
      Associativity:
      (a ** b) ** c
      a ** (b ** c)
     */
  }

  "Exercise 9.3" should "many in terms of or, map2 and succeed" in {
  }

  "Exercise 9.4" should "listOfN in terms of map2 and succeed" in {
  }

  "Exercise 9.5" should "introduce non-strictness with a separate combinator" in {
    /*
    def wrap[A](p: => Parser[A]): Parser[A]

    def many[A](p: Parser[A]): Parser[List[A]] = map2(p, wrap(many(p)))(_ :: _) or succeed(List())
     */
  }

  "Exercise 9.6" should "flatMap use-case" in {
    /*
    for {
      digit <- "[0-9]+".r
      val n = digit.toInt // we really should catch exceptions thrown by toInt and convert to parse failure
      _ <- listOfN(n, char('a'))
    } yield n
     */
  }

  "Exercise 9.7" should "product and map2 via flatMap" in {
  }

  "Exercise 9.8" should "map via flatMap" in {
  }

}

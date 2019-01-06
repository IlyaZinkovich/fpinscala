package fpinscala._04errorhandling

import org.scalatest.{FlatSpec, Matchers}

class OptionExercises extends FlatSpec with Matchers {

  "Exercise 4.1" should "map flatMap getOrElse orElse and filter" in {
    Some("value").map(str => str.toUpperCase) should be(Some("VALUE"))
    None.getOrElse("value") should be("value")
    Some("value").getOrElse("VALUE") should be("value")
    None.flatMap((str: String) => Some(str.toUpperCase)) should be(None)
    Some("value").flatMap(str => None) should be(None)
    Some("value").flatMap(str => Some(str.toUpperCase)) should be(Some("VALUE"))
    None.orElse(None) should be(None)
    None.orElse(Some("value")) should be(Some("value"))
    Some("value").orElse(Some("VALUE")) should be(Some("value"))
    Some("value").orElse(None) should be(Some("value"))
    None.filter((str: String) => str.startsWith("v")) should be(None)
    Some("value").filter((str: String) => str.startsWith("v")) should be(Some("value"))
    Some("VALUE").filter((str: String) => str.startsWith("v")) should be(None)
  }

  "Exercise 4.2" should "variance" in {
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] = {
      mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
    }

    variance(Seq()) should be(None)
    variance(Seq(1, 2, 3, 4, 5)) should be(Some(2.0))
  }

  "Exercise 4.3" should "map2" in {
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      a flatMap (aValue => b map (bValue => f(aValue, bValue)))
    }

    val none: Option[Int] = None
    map2(none, none)(_ + _) should be(none)
    map2(none, Some(2))(_ + _) should be(none)
    map2(Some(1), none)(_ + _) should be(none)
    map2(Some(1), Some(2))(_ + _) should be(Some(3))
  }

  "Exercise 4.4" should "sequence" in {
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      a flatMap (aValue => b map (bValue => f(aValue, bValue)))
    }

    def sequence[A](options: List[Option[A]]): Option[List[A]] =
      options.foldRight[Option[List[A]]](Some(Nil)) { (option, accumulator) =>
        map2(option, accumulator)(_ :: _)
      }

    sequence(List(None)) should be(None)
    sequence(List(None, Some("value"))) should be(None)
    sequence(List(None, Some("value"), None)) should be(None)
    sequence(List(Some("value"), None)) should be(None)
    sequence(List(Some("value"))) should be(Some(List("value")))
    sequence(List(Some("value1"), Some("value2"))) should be(Some(List("value1", "value2")))
  }

  "Exercise 4.5" should "traverse" in {
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      a flatMap (aValue => b map (bValue => f(aValue, bValue)))
    }

    def traverse[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] = {
      list.foldRight[Option[List[B]]](Some(Nil)) { (element, accumulator) =>
        map2(f(element), accumulator)(_ :: _)
      }
    }

    def sequence[A](options: List[Option[A]]): Option[List[A]] = traverse(options)(x => x)

    val f = (str: String) => if (str.startsWith("v")) Some(str) else None
    traverse(List("value"))(f) should be(Some(List("value")))
    traverse(List("VALUE"))(f) should be(None)
    traverse(List("value", "VALUE"))(f) should be(None)

    sequence(List(None)) should be(None)
    sequence(List(Some("value"))) should be(Some(List("value")))
    sequence(List(Some("value"), None)) should be(None)
  }
}

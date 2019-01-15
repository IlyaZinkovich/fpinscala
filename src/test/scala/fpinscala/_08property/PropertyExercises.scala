package fpinscala._08property

import java.util.concurrent.Executors.{newCachedThreadPool, newFixedThreadPool}

import fpinscala._06state.LinearCongruentalRNG
import fpinscala._07parallelism.Par
import fpinscala._07parallelism.Par.Par
import fpinscala._08property.Gen._
import fpinscala._08property.Prop.forAll
import org.scalatest.{FlatSpec, Matchers}

class PropertyExercises extends FlatSpec with Matchers {

  "Exercise 8.1" should "list properties of sum: List[Int] => Int" in {
    /*
        1. Sum of a single-element list is equal to the element of this list
        2. Sum of a list with equal elements is equal to the element value multiplied by the size of a list
        3. Sum of reversed list is equal to the sum of original list
        4. Sum of the empty list is 0
        5. If list == list1 ++ list2 then sum(list) == sum(list1) + sum(list2)
     */
  }

  "Exercise 8.2" should "list properties of max: List[Int] => Int" in {
    /*
        1. Max of a single-element list is equal to the element of this list
        2. Max of a list is greater or equal to all the elements of the list
        3. Max of a list is an element of a list
        4. Max of the empty list is None
     */
  }

  "Exercise 8.3" should "Prop with boolean check compose with &&" in {
    val propTrue = new PropWithBooleanCheck {
      def check: Boolean = true
    }
    val propFalse = new PropWithBooleanCheck {
      def check: Boolean = false
    }
    (propTrue && propFalse).check should be(false)
  }

  "Exercise 8.4" should "Gen.choose" in {
    val rng = LinearCongruentalRNG(1)
    choose(5, 10).sample.run(rng)._1 should be(8)
  }

  "Exercise 8.5" should "unit boolean and listOfN" in {
    val rng = LinearCongruentalRNG(1)
    unit(2).sample.run(rng)._1 should be(2)
    Gen.boolean.sample.run(rng)._1 should be(false)
    Gen.listOfN(3, unit(4)).sample.run(rng)._1 should be(List(4, 4, 4))
  }

  "Exercise 8.6" should "flatMap and listOfN" in {
    val rng = LinearCongruentalRNG(1)
    unit(10).flatMap(choose(0, _)).sample.run(rng)._1 should be(8)
    unit(4).listOfN(unit(3)).sample.run(rng)._1 should be(List(4, 4, 4))
  }

  "Exercise 8.7" should "union" in {
    val rng = LinearCongruentalRNG(1)
    Gen.union(unit(0), unit(1)).sample.run(rng)._1 should be(1)
  }

  "Exercise 8.8" should "weighted" in {
    val rng = LinearCongruentalRNG(1)
    weighted((unit(0), 0.9), (unit(1), 0.1)).sample.run(rng)._1 should be(0)
  }

  "Exercise 8.9" should "&& and ||" in {
    val rng = LinearCongruentalRNG(1)
    val passedProperty = Prop.forAll(unit(1))(_ == 1)
    val falsifiedProperty = Prop.forAll(unit(1))(_ == 2)
    val testCases = 10
    val maxSize = 10
    (passedProperty && passedProperty).run(maxSize, testCases, rng) should be(Passed)
    (passedProperty && falsifiedProperty).run(maxSize, testCases, rng) shouldNot be(Passed)
    (falsifiedProperty || passedProperty).run(maxSize, testCases, rng) should be(Passed)
    (falsifiedProperty || falsifiedProperty).run(maxSize, testCases, rng) shouldNot be(Passed)
  }

  "Exercise 8.10" should "Gen unsized" in {
    val rng = LinearCongruentalRNG(1)
    val unsizedGen: SGen[Int] = unit(1).unsized
    unsizedGen.forSize(10).sample.run(rng)._1 should be(1)
  }

  "Exercise 8.11" should "SGen flatMap" in {
    val rng = LinearCongruentalRNG(1)
    val sgen1 = unit(1).unsized
    val sgen2 = unit(2).unsized
    sgen1.flatMap(_ => sgen2).forSize(1).sample.run(rng)._1 should be(2)
  }

  "Exercise 8.12" should "listOf" in {
    val rng = LinearCongruentalRNG(1)
    val sgen = listOf(unit(1))
    sgen.forSize(3).sample.run(rng)._1 should be(List(1, 1, 1))
  }

  "Exercise 8.13" should "test max with listOf1" in {
    val smallInt = choose(-10, 10)
    val maxProp = forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    Prop.run(maxProp)
  }

  "Exercise 8.14" should "test sorted" in {
    val smallInt = choose(-10, 10)
    val sortProp = forAll(listOf(smallInt)) { list =>
      val sortedList = list.sorted
      sortedList.isEmpty || sortedList.tail.isEmpty || sortedList.zip(sortedList.tail).forall {
        case (left, right) => left <= right
      } && sortedList.size == list.size &&
        sortedList.forall(element => list.contains(element)) && list.forall(element => sortedList.contains(element))
    }
    Prop.run(sortProp)
  }

  "Exercise 8.15" should "test small domains exhaustively" in {
    /*
    Book Answer:
    You will need to add to the representation of `Gen`.
    For example, `Gen[Int]` should be capable of generating random integers
    as well as generating a stream of all the integers from `Int.MinValue` to `Int.MaxValue`.
    You may want to have the behavior depend on how many test cases were requested.
     */
  }

  "Exercise 8.16" should "test Par" in {
    /* A `Gen[Par[Int]]` generated from a list summation that spawns a new parallel
     * computation for each element of the input list summed to produce the final
     * result. This is not the most compelling example, but it provides at least some
     * variation in structure to use for testing.
     */
    val parGen: Gen[Par[Int]] = choose(-100, 100)
      .listOfN(choose(0, 20))
      .map(list => list.foldLeft(Par.unit(0))((par, integer) => Par.fork {
        Par.map2(par, Par.unit(integer))(_ + _)
      }))
  }

  "Exercise 8.17" should "test fork" in {
    def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p, p2)(_ == _)

    val S = weighted(
      choose(1000, 2000).map(newFixedThreadPool) -> .100,
      unit(newCachedThreadPool) -> .0
    )

    def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = forAll(S ** g) {
      case s ** a => f(a)(s).get
    }

    val parGen: Gen[Par[Int]] = choose(-10, 10)
      .listOfN(choose(0, 2))
      .map(list => list.foldLeft(Par.unit(0))((par, integer) => Par.fork {
        Par.map2(par, Par.unit(integer))(_ + _)
      }))

    val forkProp = forAllPar(parGen)(i => equal(Par.fork(i), i)) tag "fork"
    //    Prop.run(forkProp)
  }

  "Exercise 8.18" should "test takeWhile and dropWhile" in {
    val lists = Gen.listOf(Gen.choose(0, 100))
    val prop = forAll(lists) { list =>
      val lessThan50: Int => Boolean = _ < 50
      list.takeWhile(lessThan50) ++ list.dropWhile(lessThan50) == list
    }
    Prop.run(prop)
  }

  "Exercise 8.19" should "generate meaningful functions" in {
    /*
    https://github.com/fpinscala/fpinscala/blob/master/answerkey/testing/19.answer.markdown
     */
  }

  "Exercise 8.20" should "experiment with the prop testing lib" in {
    /*
    Experiment
     */
  }
}

object ** {
  def unapply[A, B](p: (A, B)) = Some(p)
}

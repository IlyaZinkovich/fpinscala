package fpinscala._08property

import fpinscala._06state.LinearCongruentalRNG
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
    Gen.choose(5, 10).sample.run(rng)._1 should be(8)
  }

  "Exercise 8.5" should "unit boolean and listOfN" in {
    val rng = LinearCongruentalRNG(1)
    Gen.unit(2).sample.run(rng)._1 should be(2)
    Gen.boolean.sample.run(rng)._1 should be(false)
    Gen.listOfN(3, Gen.unit(4)).sample.run(rng)._1 should be(List(4, 4, 4))
  }

  "Exercise 8.6" should "flatMap and listOfN" in {
    val rng = LinearCongruentalRNG(1)
    Gen.unit(10).flatMap(Gen.choose(0, _)).sample.run(rng)._1 should be(8)
    Gen.unit(4).listOfN(Gen.unit(3)).sample.run(rng)._1 should be(List(4, 4, 4))
  }

  "Exercise 8.7" should "union" in {
    val rng = LinearCongruentalRNG(1)
    Gen.union(Gen.unit(0), Gen.unit(1)).sample.run(rng)._1 should be(1)
  }

  "Exercise 8.8" should "weighted" in {
    val rng = LinearCongruentalRNG(1)
    Gen.weighted((Gen.unit(0), 0.9), (Gen.unit(1), 0.1)).sample.run(rng)._1 should be(0)
  }

  "Exercise 8.9" should "&& and ||" in {
    val rng = LinearCongruentalRNG(1)
    val passedProperty = Prop.forAll(Gen.unit(1))(_ == 1)
    val falsifiedProperty = Prop.forAll(Gen.unit(1))(_ == 2)
    val testCases = 10
    (passedProperty && passedProperty).run(testCases, rng) should be(Passed)
    (passedProperty && falsifiedProperty).run(testCases, rng) shouldNot be(Passed)
    (falsifiedProperty || passedProperty).run(testCases, rng) should be(Passed)
    (falsifiedProperty || falsifiedProperty).run(testCases, rng) shouldNot be(Passed)
  }
}

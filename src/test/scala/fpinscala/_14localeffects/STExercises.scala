package fpinscala._14localeffects

import org.scalatest.{FlatSpec, Matchers}

class STExercises extends FlatSpec with Matchers {

  "Exercise 14.1" should "STArray fill from Map" in {
    val result = new RunnableST[Int] {
      def apply[S]: ST[S, Int] = for {
        stArray <- STArray(10, 0)
        _ <- stArray.fill(Map(2 -> 1, 1 -> 2))
        array <- stArray.read(1)
      } yield array
    }

    ST.runST(result) should be(2)
  }

  "Exercise 14.2" should "STArray partition and quicksort" in {
    STArray.quicksort(List(3, 5, 4, 1, 2)) should be(List(1, 2, 3, 4, 5))
  }

  "Exercise 14.3" should "STHashMap" in {
    val result = new RunnableST[Option[Int]] {
      def apply[S]: ST[S, Option[Int]] = for {
        map <- STMap.empty[S, Int, Int]
        _ <- map += (1, 2)
        _ <- map -= 1
        _ <- map += (2, 1)
        value <- map.get(2)
      } yield value
    }

    ST.runST(result) should be(Option(1))
  }
}
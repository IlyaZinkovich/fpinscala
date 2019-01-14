package fpinscala._07parallelism

import java.util.concurrent.Executors.newSingleThreadExecutor

import org.scalatest.{FlatSpec, Matchers}

class ParExercises extends FlatSpec with Matchers {

  private val executorService = newSingleThreadExecutor()

  "Exercise 7.1" should "Par.map2 signature" in {
    /* def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] */
  }

  "Exercise 7.2" should "Par representation" in {
    /* Back Par.run with the ExecutorService: type Par[A] = ExecutorService => Future[A] */
  }

  "Exercise 7.3" should "map2 with timeouts respect" in {
    /* Requires creation of new Future implementation
       that tracks execution time of one future
       and subtracts it from the timeout of the second future
     */
  }

  "Exercise 7.4" should "asyncF" in {
    Par.asyncF[Int, Int](_ * 2)(3)(newSingleThreadExecutor()).get() should be(6)
  }

  "Exercise 7.5" should "sequence" in {
    val list = List(Par.unit(2 * 3), Par.unit(3 * 4))
    Par.sequence(list)(executorService).get() should be(List(6, 12))
  }

  "Exercise 7.6" should "parFilter" in {
    val list = List(2 * 3, 3 * 4)
    Par.parFilter[Int](list)(_ < 8)(executorService).get() should be(List(6))
  }

  "Exercise 7.7" should "prove map(map(y)(g))(f) == map(y)(f compose g)" in {
    /*
      See https://github.com/quchen/articles/blob/master/second_functor_law.md
      Also https://gist.github.com/pchiusano/444de1f222f1ceb09596
    */
  }

  "Exercise 7.8" should "counterexample of fork(x) == x" in {
    /*
      val list = List(Par.unit(2 * 3), Par.unit(3 * 4))
      Par.map(Par.sequenceBalanced(list.toIndexedSeq))(_.toList)(executorService).get() will result in deadlock
    */
  }

  "Exercise 7.9" should "prove current fork can result in deadlock for any fixed-size thread pool" in {
    /*
      For a thread pool of size 2, fork(fork(fork(x))) and so on
    */
  }

  "Exercise 7.10" should "introduce error handling to actor-based Par" in {
    /*
      Add a second continuation argument to `Future.apply`, which takes an error handler.
      See Task data type in Chapter 13
    */
  }

  "Exercise 7.11" should "choice and choiceN" in {
    Par.choice(Par.unit(false))(Par.unit(1), Par.unit(2))(executorService).get should be(2)
  }

  "Exercise 7.12" should "choice map" in {
    Par.choiceMap(Par.unit(1))(Map(1 -> Par.unit(1), 2 -> Par.unit(2)))(executorService).get should be(1)
  }

  "Exercise 7.13" should "chooser" in {
    Par.flatMap(Par.unit(1))(Map(1 -> Par.unit(1), 2 -> Par.unit(2)))(executorService).get should be(1)
  }

  "Exercise 7.14" should "join" in {
    Par.joinViaFlatMap(Par.unit(Par.unit(1)))(executorService).get should be(1)
  }
}

package fpinscala._08property

import fpinscala._06state.{RNG, State}
import fpinscala._08property.Prop.{FailedCase, SuccessCount, TestCases}

trait PropWithBooleanCheck {

  def check: Boolean

  def &&(prop: PropWithBooleanCheck): PropWithBooleanCheck = new PropWithBooleanCheck {
    def check: Boolean = PropWithBooleanCheck.this.check && prop.check
  }
}

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))
}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(randomInt => randomInt % (stopExclusive - start) + start))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeInt).map(randomInt => randomInt % 2 == 1))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(randomBoolean => if (randomBoolean) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    Gen(State(RNG.double)).flatMap(randomDouble => {
      val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
      if (randomDouble < g1Threshold) g1._1 else g2._1
    })
}

sealed trait Result

case object Passed extends Result

case class Falsified(failure: FailedCase, successCount: SuccessCount) extends Result

case class Prop(run: (TestCases, RNG) => Result) {

  def &&(prop: Prop): Prop = Prop((testCases: TestCases, rng: RNG) => {
    run(testCases, rng) match {
      case Passed => prop.run(testCases, rng)
      case falsified => falsified
    }
  })

  def ||(prop: Prop): Prop = Prop((testCases: TestCases, rng: RNG) => {
    run(testCases, rng) match {
      case Falsified(failure, _) => prop.tag(failure).run(testCases, rng)
      case Passed => prop.run(testCases, rng)
    }
  })

  def tag(msg: String) = Prop((testCases: TestCases, rng: RNG) => {
    run(testCases, rng) match {
      case Falsified(failure, successCount) => Falsified(msg + "\n" + failure, successCount)
      case passed => passed
    }
  })
}

object Prop {

  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int

  import fpinscala._05laziness.Stream

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop((testCases, rng) => {
    randomStream(as)(rng).zip(Stream.from(0)).take(testCases).map {
      case (generatedValue, testCase) => try {
        if (f(generatedValue)) Passed else Falsified(generatedValue.toString, testCase)
      } catch {
        case e: Exception => Falsified(buildMsg(generatedValue, e), testCase)
      }
    }.find {
      case Falsified(_, _) => true
      case _ => false
    }.getOrElse(Passed)
  })

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String = {
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  }
}

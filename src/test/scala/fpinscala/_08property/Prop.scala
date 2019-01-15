package fpinscala._08property

import java.lang.System.currentTimeMillis

import fpinscala._06state.{LinearCongruentalRNG, RNG, State}
import fpinscala._08property.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}

trait PropWithBooleanCheck {

  def check: Boolean

  def &&(prop: PropWithBooleanCheck): PropWithBooleanCheck = new PropWithBooleanCheck {
    def check: Boolean = PropWithBooleanCheck.this.check && prop.check
  }
}

case class Gen[+A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))

  def unsized: SGen[A] = SGen(_ => this)

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = Gen(sample.map2(g.sample)(f))

  def **[B](g: Gen[B]): Gen[(A,B)] = (this map2 g)((_,_))
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

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(size => listOfN(size, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(size => listOfN(size max 1, g))
}

case class SGen[+A](forSize: Int => Gen[A]) {

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(size => forSize(size).flatMap(f(_).forSize(size)))
}

sealed trait Result

case object Proved extends Result

case object Passed extends Result

case class Falsified(failure: FailedCase, successCount: SuccessCount) extends Result

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(prop: Prop): Prop = Prop((maxSize, testCases, rng) => {
    run(maxSize, testCases, rng) match {
      case Passed => prop.run(maxSize, testCases, rng)
      case Proved => prop.run(maxSize, testCases, rng)
      case falsified => falsified
    }
  })

  def ||(prop: Prop): Prop = Prop((maxSize, testCases, rng) => {
    run(maxSize, testCases, rng) match {
      case Falsified(failure, _) => prop.tag(failure).run(maxSize, testCases, rng)
      case passedOrProved => prop.run(maxSize, testCases, rng)
    }
  })

  def tag(msg: String) = Prop((maxSize, testCases, rng) => {
    run(maxSize, testCases, rng) match {
      case Falsified(failure, successCount) => Falsified(msg + "\n" + failure, successCount)
      case passed => passed
    }
  })
}

object Prop {

  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int

  import fpinscala._05laziness.Stream

  def check(p: => Boolean): Prop = Prop((_, _, _) => if (p) Proved else Falsified("()", 0))

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop { (maxSize, testCases, rng) =>
    val casesPerSize = (testCases + (maxSize - 1)) / maxSize
    val props: Stream[Prop] = Stream.from(0).take((testCases min maxSize) + 1).map(i => forAll(g(i))(f))
    val prop: Prop = props.map(p => Prop((max, _, rng) => p.run(max, casesPerSize, rng))).toList.reduce(_ && _)
    prop.run(maxSize, testCases, rng)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop((maxSize, testCases, rng) => {
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

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = LinearCongruentalRNG(currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
}

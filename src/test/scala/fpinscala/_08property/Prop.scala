package fpinscala._08property

import fpinscala._06state.{RNG, State}
import fpinscala._08property.Prop.{FailedCase, SuccessCount}

trait PropWithBooleanCheck {

  def check: Boolean

  def &&(prop: PropWithBooleanCheck): PropWithBooleanCheck = new PropWithBooleanCheck {
    def check: Boolean = PropWithBooleanCheck.this.check && prop.check
  }
}

object Prop {

  type SuccessCount = Int
  type FailedCase = String
}

trait Prop {

  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

case class Gen[A](sample: State[RNG, A])

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(randomInt => randomInt % (stopExclusive - start) + start))
  }

  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.nonNegativeInt).map(randomInt => randomInt % 2 == 1))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }
}

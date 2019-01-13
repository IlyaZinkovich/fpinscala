package fpinscala._06state

import fpinscala._06state.Machine.simulateMachine
import org.scalatest.{FlatSpec, Matchers}

class MachineExercise extends FlatSpec with Matchers {

  "Exercise 6.11" should "simulate machine" in {
    val machine = Machine(locked = true, candies = 10, coins = 0)

    simulateMachine(List(Turn)).run(machine)._1 should be((10, 0))
    simulateMachine(List(Coin)).run(machine)._1 should be((10, 1))
    simulateMachine(List(Coin, Turn)).run(machine)._1 should be((9, 1))
    simulateMachine(List(Coin, Turn, Turn)).run(machine)._1 should be((9, 1))
    simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Coin, Coin)).run(machine)._1 should be((8, 3))
  }
}

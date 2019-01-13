package fpinscala._06state

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  import State._

  def update(input: Input, machine: Machine): Machine = (input, machine) match {
    case (Coin, Machine(true, candies, coins)) => Machine(locked = false, candies, coins + 1)
    case (Turn, Machine(false, candies, coins)) => Machine(locked = true, candies - 1, coins)
    case (Coin, Machine(false, _, _)) => machine
    case (Turn, Machine(true, _, _)) => machine
    case (_, Machine(_, 0, _)) => machine
  }

  def stateUpdate(input: Input): State[Machine, (Int, Int)] = State(machine => {
    val updatedMachine = update(input, machine)
    ((updatedMachine.candies, updatedMachine.coins), updatedMachine)
  })

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    sequence(inputs map (input => stateUpdate(input))).map(_.last)
}

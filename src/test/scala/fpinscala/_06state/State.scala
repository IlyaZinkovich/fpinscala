package fpinscala._06state

case class State[S, +A](run: S => (A, S)) {

  import State._

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(currentState => {
    val (a, nextState) = run(currentState)
    f(a).run(nextState)
  })

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  def get: State[S, S] = State(currentState => (currentState, currentState))

  def set(nextState: S): State[S, Unit] = State(_ => ((), nextState))

  def modify(f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

object State {

  def unit[S, A](a: A): State[S, A] = State(state => (a, state))

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] =
    states.foldRight(unit[S, List[A]](Nil))((state, acc) => state.map2(acc)(_ :: _))
}



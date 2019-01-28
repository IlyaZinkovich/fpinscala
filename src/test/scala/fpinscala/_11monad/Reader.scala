package fpinscala._11monad

case class Reader[R, A](run: R => A)

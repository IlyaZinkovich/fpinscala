package fpinscala._13io

import java.nio.channels.AsynchronousFileChannel

import fpinscala._07parallelism.Par.Par
import fpinscala._12applicative.Monad
import org.scalatest.{FlatSpec, Matchers}

class FreeExercises extends FlatSpec with Matchers {

  "Exercise 13.1" should "map, flatMap and monad" in {
    def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] = new Monad[({type f[a] = Free[F, a]})#f] {
      def unit[A](a: => A): Free[F, A] = ReturnFree(a)

      def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa flatMap f
    }
  }

  "Exercise 13.2" should "runTrampoline" in {
    @annotation.tailrec
    def runTrampoline[A](a: Free[Function0, A]): A = a match {
      case ReturnFree(a1) => a1
      case SuspendFree(r1) => r1()
      case FlatMapFree(x1, f1) => x1 match {
        case ReturnFree(a2) => runTrampoline {
          f1(a2)
        }
        case SuspendFree(r2) => runTrampoline {
          f1(r2())
        }
        case FlatMapFree(x2, f2) => runTrampoline {
          x2 flatMap { x3 => f2(x3) flatMap f1 }
        }
      }
    }

    runTrampoline(ReturnFree(1)) should be(1)
  }

  "Exercise 13.3" should "run generic" in {
    @annotation.tailrec
    def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
      case FlatMapFree(FlatMapFree(x, f), g) => step(x flatMap (a => f(a) flatMap g))
      case FlatMapFree(ReturnFree(x), f) => step(f(x))
      case _ => a
    }

    def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
      case ReturnFree(a) => F.unit(a)
      case SuspendFree(r) => r
      case FlatMapFree(SuspendFree(r), f) => F.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible, since `step` eliminates these cases")
    }
  }

  "Exercise 13.4" should "translate and runConsole" in {

  }

  "Exercise 13.5" should "read from AsynchronousFileChannel" in {
    def read(file: AsynchronousFileChannel, fromPosition: Long,
             numBytes: Int): Par[Either[Throwable, Array[Byte]]] = ???
  }
}
package fpinscala._13io

import fpinscala._12applicative.Monad

import scala.io.StdIn

trait PrimitiveIO[A] {
  self =>

  def run: A

  def map[B](f: A => B): PrimitiveIO[B] = new PrimitiveIO[B] {
    def run: B = f(self.run)
  }

  def flatMap[B](f: A => PrimitiveIO[B]): PrimitiveIO[B] = new PrimitiveIO[B] {
    def run: B = f(self.run).run
  }
}

object PrimitiveIO extends Monad[PrimitiveIO] {

  def flatMap[A, B](fa: PrimitiveIO[A])(f: A => PrimitiveIO[B]): PrimitiveIO[B] = fa flatMap f

  def apply[A](a: => A): PrimitiveIO[A] = unit(a)

  def unit[A](a: => A): PrimitiveIO[A] = new PrimitiveIO[A] {
    def run: A = a
  }

  def forever[A, B](a: PrimitiveIO[A]): PrimitiveIO[B] = {
    lazy val b: PrimitiveIO[B] = forever(a)
    a flatMap (_ => b)
  }
}

object PrimitiveIOMain extends App {

  def converter: PrimitiveIO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def fahrenheitToCelsius(d: Double) = (d - 32) * 5.0 / 9.0

  def ReadLine: PrimitiveIO[String] = PrimitiveIO {
    StdIn.readLine()
  }

  def PrintLine(msg: String): PrimitiveIO[Unit] = PrimitiveIO {
    println(msg)
  }

  //  converter.run
  PrimitiveIO.forever(PrimitiveIO(println("run"))).run
}

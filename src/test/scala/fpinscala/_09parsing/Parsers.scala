package fpinscala._09parsing

import fpinscala._08property.Prop.forAll
import fpinscala._08property.{Gen, Prop}

import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+ _]] {
  self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  implicit def operators[A](p: Parser[A]): ParsersOps[A] = ParsersOps(p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParsersOps[String] = ParsersOps(f(a))

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n == 0) succeed(Nil) else map2(p, listOfN(n - 1, p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(Nil)

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(a => succeed(f(a)))

  def slice[A](p: Parser[A]): Parser[String]

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p1)(a => p2.map(b => (a, b)))

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    product(p1, p2).map(tuple => f(tuple._1, tuple._2))

  def map2ViaFlatMap[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p1)(a => p2.map(b => f(a, b)))

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  case class ParsersOps[A](parser: Parser[A]) {

    def |[B >: A](otherParser: Parser[B]): Parser[B] = self.or(parser, otherParser)

    def or[B >: A](otherParser: Parser[B]): Parser[B] = self.or(parser, otherParser)

    def many: Parser[List[A]] = self.many(parser)

    def map[B](f: A => B): Parser[B] = self.map(parser)(f)

    def slice: Parser[String] = self.slice(parser)

    def product[B](otherParser: Parser[B]): Parser[(A, B)] = self.product(parser, otherParser)

    def **[B](otherParser: Parser[B]): Parser[(A, B)] = self.product(parser, otherParser)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(parser)(f)
  }

  object Laws {

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)

    def productLaw[A, B](p1: Parser[A], p2: Parser[B])(in: Gen[String]): Prop =
      equal(product(p1, p2), product(p2, p1))(in)
  }

}

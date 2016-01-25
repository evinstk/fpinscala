package fpinscala.practice.parsing

import fpinscala.testing._
import scala.util.matching.Regex
import scala.language.higherKinds
import scala.language.implicitConversions


trait Parsers[ParseError,Parser[+_]] { self =>

  // Primitives
  def run[A](p: Parser[A])(s: String): Either[ParseError, A]
  def string(s: String): Parser[String]
  def flatMap[A,B](a: Parser[A])(f: A => Parser[B]): Parser[B]
//  def join[A](a: Parser[Parser[A]]): Parser[A]
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def slice[A](p: Parser[A]): Parser[String]
  implicit def regex(r: Regex): Parser[String]

  // Derived
  def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))
  def succeed[A](a: A): Parser[A] =
    string("") map { _ => a }
//  def flatMap[A,B](a: Parser[A])(f: A => Parser[B]): Parser[B] =
//    join(a.map(f))
  def product[A,B](a: Parser[A], b: => Parser[B]): Parser[(A,B)] = for {
    x <- a
    y <- b
  } yield (x,y)
  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))
  def map2[A,B,C](a: Parser[A], b: => Parser[B])(f: (A,B) => C): Parser[C] = for {
    x <- a
    y <- b
  } yield f(x,y)
//    (a**b) map { case (a,b) => f(a,b) }
  def many[A](a: Parser[A]): Parser[List[A]] =
    map2(a, many(a))(_ :: _) or succeed(Nil)
  def many1[A](a: Parser[A]): Parser[List[A]] =
    map2(a, many(a))(_ :: _)
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    sequence(List.fill(n)(p))
  def sequence[A](ps: List[Parser[A]]): Parser[List[A]] =
    ps.foldRight[Parser[List[A]]](succeed(Nil))(map2(_,_)(_ :: _))

  // Implicitly promote strings to parsers
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  // Convenient infix aliases
  case class ParserOps[A](p: Parser[A]) {
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
  }
  implicit def operators[A](p: Parser[A]): ParserOps[A] =
    ParserOps(p)

  object ParserLaws {

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p map (a => a))(in)
  }
}

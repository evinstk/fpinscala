package fpinscala.practice.monoids.Monoid

import fpinscala.testing._
import fpinscala.parallelism.Nonblocking._


trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(s1: String, s2: String): String = s1 + s2
    def zero: String = ""
  }

  val intAddition = new Monoid[Int] {
    def op(i1: Int, i2: Int): Int = i1 + i2
    def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(i1: Int, i2: Int): Int = i1 * i2
    def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean): Boolean = b1 || b2
    def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean): Boolean = b1 && b2
    def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(o1: Option[A], o2: Option[A]): Option[A] = o1 orElse o2
    def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A): A => A = f compose g
    def zero: A => A = a => a
  }

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b,a) => m.op(b,f(a)))

  def foldMapV[A,B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.length == 1) f(as.head)
    else {
      val (asl, asr) = as.splitAt(as.length / 2)
      m.op(foldMapV(asl, m)(f), foldMapV(asr, m)(f))
    }

  def productMonoid[A,B](a: Monoid[A], b: Monoid[B]): Monoid[(A,B)] = new Monoid[(A,B)] {
    def op(x: (A,B), y: (A,B)): (A,B) = (a.op(x._1,y._1), b.op(x._2,y._2))
    def zero: (A,B) = (a.zero, b.zero)
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    Prop.forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x,y,z)){
      case(x,y,z) => m.op(m.op(x,y),z) == m.op(x,m.op(y,z))
    } && Prop.forAll(gen)(a => m.op(a, m.zero) == a)
}


package fpinscala.practice.applicative

import fpinscala.practice.monads._
import scala.language.higherKinds


trait Applicative[F[_]] extends Functor[F] {

  // Primitive combinators
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]
  def unit[A](a: => A): F[A]

  // Derived combinators
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    apply[B,C](apply[A,B => C](unit(a => f.curried(a)))(fa))(fb)

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply[A,B](unit(f))(fa)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a,acc) => map2(f(a),acc)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa,fb)((_,_))


  // Alternative implementations
  def mapViaMap2[A,B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a,_) => f(a))
  def applyViaMap2[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab,fa)((ab,a) => ab(a))
}

object Applicative {
}


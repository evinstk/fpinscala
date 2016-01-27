package fpinscala.practice.Monads

import scala.language.higherKinds


trait Functor[F[_]] {

  def map[A,B](fa: F[A])(f: A => B): F[B]
}

object Functor {

  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] =
      as.map(f)
  }
}


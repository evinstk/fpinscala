package fpinscala.practice.monads.instances

import fpinscala.practice.monads.Monad


case class Reader[R, A](run: R => A) {
  import Reader._

  def flatMap[B](f: A => Reader[R,B]): Reader[R,B] =
    readerMonad.flatMap(this)(f)
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def flatMap[A,B](ra: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
      Reader(r => f(ra.run(r)).run(r))
    def unit[A](a: => A): Reader[R,A] = Reader(_ => a)
  }

  def unit[R,A](a: => A): Reader[R,A] = readerMonad[R].unit(a)
}


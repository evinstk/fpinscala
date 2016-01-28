package fpinscala.practice.monads

import scala.language.higherKinds
import fpinscala.testing._
import fpinscala.state._
import fpinscala.practice.applicative._


trait Functor[F[_]] {

  def map[A,B](fa: F[A])(f: A => B): F[B]
}

object Functor {

  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] =
      as.map(f)
  }
}

trait Monad[F[_]] extends Applicative[F] {

  override def apply[A,B](mab: F[A => B])(ma: F[A]): F[B] =
    map2(mab,ma)((ab,a) => ab(a))

  // Minimal implementation of Monad must implement
  // `unit' and either `flatMap' or both `map' and
  // `join'.
  def unit[A](a: => A): F[A]

  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
  override def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  override def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))

  /* The following implementations are unnecessary
   * if Monad extends Applicative
  // Default functor implementation
  def map[A,B](a: F[A])(f: A => B): F[B] =
    flatMap(a)(a => unit(f(a)))
  def map2[A,B,C](a: F[A], b: F[B])(f: (A,B) => C): F[C] =
    flatMap(a)(x => map(b)(y => f(x,y)))

  // Combinators
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(Nil): F[List[A]])((ma, acc) =>
      flatMap(ma)(a => map(acc)(as => a :: as)))
  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la map { a => f(a) })
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))
  def product[A,B](ma: F[A], mb: F[B]): F[(A,B)] =
    map2(ma,mb)((_,_))
  */

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    map(sequence(ms.map(a => map(f(a))((_,a)))))(_.flatMap{ case(b,x) =>
      if (b) List(x) else Nil
    })
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(b => g(b))
}

object Monad {

  val listMonad = new Monad[List] {
    override def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
      as.flatMap(f)
    def unit[A](a: => A): List[A] = List(a)
  }

  val genMonad = new Monad[Gen] {
    override def flatMap[A,B](a: Gen[A])(f: A => Gen[B]): Gen[B] =
      a flatMap f
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
  }

  val optionMonad = new Monad[Option] {
    override def flatMap[A,B](a: Option[A])(f: A => Option[B]): Option[B] =
      a flatMap f
    def unit[A](a: => A): Option[A] = Some(a)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
    def unit[A](a: => A): State[S,A] = State(s => (a,s))
    override def flatMap[A,B](ma: State[S,A])(f: A => State[S,B]): State[S,B] =
      ma.flatMap(f)
  }

  def eitherMonad[E] = new Monad[({type f[x] = Either[E,x]})#f] {
    def unit[A](a: => A): Either[E,A] = Right(a)
    override def flatMap[A,B](e: Either[E,A])(f: A => Either[E,B]): Either[E,B] = e match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  }
}

case class Id[A](value: A) {

  val idMonad = new Monad[Id] {
    override def flatMap[A,B](a: Id[A])(f: A => Id[B]): Id[B] = f(a.value)
    def unit[A](a: => A): Id[A] = Id(a)
  }

  def flatMap[B](f: A => Id[B]): Id[B] = idMonad.flatMap(this)(f)
  def map[B](f: A => B): Id[B] = idMonad.map(this)(f)
}


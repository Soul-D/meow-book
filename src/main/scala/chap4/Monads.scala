package chap4

import cats.Monad
import cats.Id
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.instances.option._
import cats.syntax.either._

import scala.annotation.tailrec

object Monads extends App {

  trait MyMonad[F[_]] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def pure[A](a: A): F[A]
    def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(f.andThen(pure))
  }

  def sumSquares[F[_] : Monad](as: F[Int], bs: F[Int]): F[Int] = {
    for {
      a <- as
      b <- bs
    } yield a * b
  }

  type MyId[A] = A

  implicit val myIdMonad = new Monad[MyId] {
    def pure[A](x: A) = x
    def flatMap[A, B](fa: MyId[A])(f: A => MyId[B]) = f(fa)

    @tailrec
    def tailRecM[A, B](a: A)(f: A => MyId[Either[A, B]]): MyId[B] =
      f(a) match {
      case Left(aa) => tailRecM(aa)(f)
      case Right(b) => b
    }
  }

  val x = sumSquares(Option(2), Option(3))
  println(x)

  println(sumSquares(3: Id[Int], 4: Id[Int]))

  val one = 1.asRight[String]
  val ten = 10.asRight[String]

  val eleven = for {
    o <- one
    t <- ten
  } yield {
    o + t
  }

  println(eleven)

  val recovered: Either[String, Int] = "bad things".asLeft[Int].recover {
    case s: String if s.startsWith("bad") => 10
  }

  val reallyRecovered: Either[String, Int] = "bad things".asLeft[Int].recoverWith {
    case s: String if s.startsWith("bad") => 10.asRight
  }
}

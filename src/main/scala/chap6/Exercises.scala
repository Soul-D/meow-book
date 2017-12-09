package chap6

import cats._
import cats.implicits._

object Exercises extends App{
  def product[M[_]: Monad, A, B](m1: M[A], m2: M[B]): M[(A, B)] =
    for {
      a <- m1
      b <- m2
    } yield (a, b)

  println(product(Option(1), Option(2)))
}

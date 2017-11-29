package chap4

import cats._
import cats.implicits._

object MonadErrorApp extends App {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  val monadErrorE = MonadError[Either[String, ?], String]

  val success: Either[String, Int] = monadErrorE.pure(42)
  println(success)
  val errorE: Either[String, Int] = monadErrorE.raiseError("Oh dear")
  println(errorE)
  val handled = errorE.handleError(_ => 42)
  println(handled)

  println(handled.ensure("Crap")(_ != 42))

  // Has to be unit as None doesn't have a paramter
  val monadErrorO = MonadError[Option, Unit]

  val successO = monadErrorO.pure(42)
  println(successO)
  val errorO = monadErrorO.raiseError(())
  println(errorO)

}

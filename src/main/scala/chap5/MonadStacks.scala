package chap5

import scala.concurrent.Future
import cats._
import cats.data._
import cats.implicits._
import scala.concurrent.ExecutionContext.Implicits.global

object MonadStacks extends App {

  val oneLevel = OptionT(Right(Option(1)))
  println(oneLevel)

  val actualValue = Future.successful(Right(Option(1)))
  println(actualValue)


  type FutureEitherOption[A] = OptionT[EitherT[Future, String, ?], A]

  val y = EitherT(Future.successful[Either[String, Int]](Right(1)))


  def doSomething(value: Int): FutureEitherOption[Int] = {
    for {
      a <- value.pure[FutureEitherOption]
      b <- 20.pure[FutureEitherOption]
    } yield a + b
  }

  def doSomethingNoSyntax(value: Int): FutureEitherOption[Int] = {
    OptionT(EitherT(Future.successful[Either[String, Option[Int]]](Right(Option(value)))))
      .flatMap(a => 20.pure[FutureEitherOption].map(b => a + b))
  }

  println(doSomething(10))
  println(doSomethingNoSyntax(10))

  val omgLol = 123.pure[OptionT[EitherT[Future, String, ?], ?]]
  println(omgLol)
  println(omgLol.map(_ * 10))

  type ErrorOr[A] = Either[Vector[String], A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  val errorStack2 = 32.pure[ErrorOrOption]
  println(errorStack2)
  println(errorStack2.map(_ * 10))
  println(errorStack2.value.map(_.getOrElse(20)))


}

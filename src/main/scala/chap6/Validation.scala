package chap6

import cats.data.Validated
import cats.data.NonEmptyList

import cats.Semigroupal

import cats.syntax.validated._
import cats.syntax.apply._

import cats.instances.int._
import cats.instances.string._
import cats.instances.list._

object Validation extends App {

  type AllErrorsOrElse[A] = Validated[NonEmptyList[String], A]

  val allTheErrors = Semigroupal[AllErrorsOrElse].product(
    Validated.invalid(NonEmptyList.of("Crap")),
    Validated.invalid(NonEmptyList.of("Oh dear"))
  )
  println(allTheErrors)

  val noErrors = Semigroupal[AllErrorsOrElse].product(
    Validated.valid("Woohoo"),
    Validated.valid(10)
  )
  println(noErrors)

  val noErrorsSyntax = (123.valid, "cats".valid[String]).tupled
  println(noErrorsSyntax)

}

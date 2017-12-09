package chap6

import cats.data._
import cats._
import cats.implicits._

object ValidationExercise extends App {

  type Result[A] = Either[NonEmptyList[String], A]
  type ValidatedResult[A] = Validated[NonEmptyList[String], A]

  case class User(name: String, age: Int)

  def readString(input: Map[String, String], field: String): Result[String] =
    input.get(field)
      .toRight(NonEmptyList.of(s"$field not specified"))

  def parseInt(input: String): Result[Int] =
    Either.catchOnly[NumberFormatException](input.toInt)
      .left.map(_ => NonEmptyList.of("Invalid number"))

  def nonBlank(input: String): Result[String] =
    Either.right(input).ensure(NonEmptyList.of("blank field"))(_.nonEmpty)

  def readName(input: Map[String, String]): Result[String] =
    readString(input, "name")
        .flatMap(nonBlank)


  def readAge(input: Map[String, String]): Either[NonEmptyList[String], Int] =
    readString(input, "age")
      .flatMap(parseInt)

  def user(input: Map[String, String]): Validated[NonEmptyList[String], User] = {
//    Semigroupal[ValidatedResult].product(readName(input).toValidated, readAge(input).toValidated)
//      .map { case (name, age) => User(name, age) }
    (readName(input).toValidated, readAge(input).toValidated).mapN(User.apply)
  }


  val goodUserInput = Map(
    "name" -> "Christopher",
    "age" -> "32"
  )
  val missingUserInput = Map.empty[String, String]

  val malformedUserInput = Map(
    "name" -> "Christopher",
    "age" -> "cat"
  )

  val multipleErrorsInput = Map(
    "age" -> "cat"
  )

  println(user(goodUserInput))
  println(user(missingUserInput))
  println(user(malformedUserInput))
  println(user(multipleErrorsInput))

}

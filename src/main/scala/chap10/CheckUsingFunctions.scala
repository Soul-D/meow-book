package chap10

import cats._
import cats.data.Validated.{Invalid, Valid}
import cats.data._
import cats.implicits._
import org.typelevel.discipline.Predicate

object CheckUsingFunctions extends App {

  case class Check[E, A](f: A => Validated[E, A]) {
    def apply(value: A): Validated[E, A] = f(value)

    def and(other: Check[E, A])(implicit ev: Semigroup[E]): Check[E, A] =
      Check { a =>
        (this (a), other(a)).tupled.map(_ => a)
      }

    def or(other: Check[E, A])(implicit ev: Semigroup[E]): Check[E, A] =
      Check { a =>
        (this (a), other(a)) match {
          case (Valid(_), _) => a.valid
          case (_, Valid(_)) => a.valid
          case bothInvalid => bothInvalid.tupled.map(_ => a)
        }
      }
  }

  // We need a semi group for E

  // No short circuit as then we can report as many errors as possible

  val positive = Check[NonEmptyList[String], Int](i => if (i > 0) i.valid else s"$i not positive".invalidNel)
  val even = Check[NonEmptyList[String], Int](i => if (i % 2 == 0) i.valid else s"$i not even".invalidNel)

  val positiveAndEven = positive and even
  val positiveOrEven = positive or even

  println(positive(2))
  println(positive(-1))
  println(even(2))
  println(even(1))
  println(positiveAndEven(2))
  println(positiveAndEven(3))
  println(positiveAndEven(-3))

  println(positiveOrEven(9))
  println(positiveOrEven(-4))
  println(positiveOrEven(-5))

}

object CheckUsingAdt extends App {
  sealed trait Predicate[E, A] {
    def apply(value: A)(implicit ev: Semigroup[E]): Validated[E, A] = this match {
      case PredPure(f) =>
        f(value)

      case And(left, right) =>
        (left(value), right(value)).mapN((_, _) => value)

      case Or(left, right) => (left(value), right(value)) match {
        case (Valid(_), _) => value.valid
        case (_, Valid(_)) => value.valid
        case bothInvalid => bothInvalid.tupled.map(_ => value)
      }
    }

    def and(other: Predicate[E, A])(implicit ev: Semigroup[E]): Predicate[E, A] =
      And(this, other)

    def or(other: Predicate[E, A])(implicit ev: Semigroup[E]): Predicate[E, A] =
      Or(this, other)
  }

  final case class PredPure[E, A](f: A => Validated[E, A]) extends Predicate[E, A]
  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  object Predicate {
    def lift[E, A](e: E, f: A => Boolean) = {
      PredPure[E, A](a => if (f(a)) a.valid else e.invalid)
    }
  }

  sealed trait Check[E, A, B] {
    import Check._

    def apply(value: A)(implicit ev: Semigroup[E]): Validated[E, B] = this match {
      case Pure(f) => f(value)
      // AFAICT this is a compiler bug
      case PurePredicate(p: Predicate[E, B with A]) => p(value.asInstanceOf[B with A])
      case Map(check, f) => check(value).map(f)
      case FlatMap(check, f) => check(value) match {
        case Valid(b) => f(b)(value)
        case Invalid(e) => e.invalid
      }
      case AndThen(first, second) => first(value).withEither(_.flatMap(b => second(b).toEither))
    }

    def map[C](f: B => C): Check[E, A, C] = Map[E, A, B, C](this, f)
    def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] = FlatMap[E, A, B, C](this, f)
    def andThen[C](that: Check[E, B, C]): Check[E, A, C] = AndThen(this, that)
  }


  object Check {
    def apply[E, A, B](f: A => Validated[E, B]): Check[E, A, B] = Pure(f)
    def apply[E, A, B](p: Predicate[E, A]): Check[E, A, A] = PurePredicate(p)

    case class Pure[E, A, B](f: A => Validated[E, B]) extends Check[E, A, B]
    case class PurePredicate[E, A](pred: Predicate[E, A]) extends Check[E, A, A]
    case class Map[E, A, B, C](chec: Check[E, A, B], f: B => C) extends Check[E, A, C]
    case class FlatMap[E, A, B, C](chec: Check[E, A, B], f: B => Check[E, A, C]) extends Check[E, A, C]
    case class AndThen[E, A, B, C](check: Check[E, A, B], check2: Check[E, B, C]) extends Check[E, A, C]
  }

  val positive = PredPure[NonEmptyList[String], Int](i => if (i > 0) i.valid else s"$i not positive".invalidNel)
  val even = PredPure[NonEmptyList[String], Int](i => if (i % 2 == 0) i.valid else s"$i not even".invalidNel)

  val positiveMultiplyTen = Check[NonEmptyList[String], Int, Int](positive.f)
  val positiveMultiplyHundred = Check[NonEmptyList[String], Int, Int](positive.f)
  val more = positiveMultiplyTen.map(_ - 2)
  println(positiveMultiplyTen(10))
  println(positiveMultiplyTen(-1))

  println(more(10))
  println(more(-1))

  val moreMore: Check[NonEmptyList[String], Int, Int] = more.flatMap { i =>
    if (i < 15)
      positiveMultiplyHundred
    else
      positiveMultiplyTen
  }

  println(moreMore(10))
  println(moreMore(1))
  val alot = positiveMultiplyTen andThen positiveMultiplyHundred
  println(alot(-1))
  println(alot(5))

  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.length > n)
  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit))
  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char))
  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.count(c => c == char) == 1)

  case class UserName(name: String)
  case class Email(email: String)

  val userNameValidation: Check[Errors, String, String] = Check(longerThan(3) and alphanumeric)
  val toUserName = userNameValidation.map(UserName)

  println(toUserName("cat"))
  println(toUserName("chbatey"))

  val splitEmail: Check[NonEmptyList[String], String, (String, String)] = Check((s: String) => s.split('@') match {
    case Array(name, domain) => (name, domain).validNel
    case _ => error("must contain only one @").invalid
  })

  val checkLeft: Check[Errors, String, String] = Check(longerThan(0))
  val checkRight: Check[Errors, String, String] = Check(longerThan(3) and contains('.'))

  val joinEmail = Check[Errors, (String, String), String] {
    (s: (String, String)) => (checkLeft(s._1), checkRight(s._2)).mapN(_ + "@" + _)
  }

  val validateEmail = (splitEmail andThen joinEmail).map(Email)

  println(validateEmail("christopher.batey@gmail.com"))
  println(validateEmail("@gmail.com"))
  println(validateEmail("a@a."))


}

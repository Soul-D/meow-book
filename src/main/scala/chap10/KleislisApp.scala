package chap10

import cats.data.{Kleisli, NonEmptyList, Validated}
import cats._
import cats.implicits._

object KleislisApp extends App {

  type Errors = NonEmptyList[String]

  type Predicate[E, A] = A => Either[E, A]

  type Check[E, A, B] = Kleisli[Either[E, ?], A, B]

  case class Email(s: String)
  case class UserName(u: String)
  case class User(u: UserName, e: Email)

  val notEmpty: Predicate[Errors, String] = { s =>
    if (s.isEmpty) Left(NonEmptyList.of("empty fool"))
    else Right(s)
  }
  def contains(c: Char): Predicate[Errors, String] = { s =>
    if (s.contains(c)) Right(s)
    else Left(NonEmptyList.of(s"No $c fool"))
  }


  def atLeast(chars: Int): Predicate[Errors, String] = { s =>
    if (s.length > chars) Right(s)
    else Left(NonEmptyList.of(s"must be $chars"))
  }

  val userNameValidation: Check[Errors, String, UserName] =
    Kleisli(notEmpty) andThen Kleisli(atLeast(3)).map(UserName)

  println(userNameValidation.run("cat dog"))

  val emailSplit: Check[Errors, String, (String, String)] = Kleisli((s: String) => {
    s.split('@') match {
      case Array(name, domain) => Right((name, domain))
      case _ => Left(NonEmptyList.of("Only one @"))
    }
  })

  val checkLeft: Check[Errors, String, String] = Kleisli(notEmpty)
  val checkRight: Check[Errors, String, String] = Kleisli(contains('.')) andThen Kleisli(atLeast(3))

  val joinEmail: Check[Errors, (String, String), String] = Kleisli {
    case (name, domain) => (checkLeft.run(name), checkRight.run(domain)).mapN( _ + "@" + _)
  }

  val validateEmail = (emailSplit andThen joinEmail).map(Email)

  def userValidation(name: String, email: String): Either[Errors, User] = {
    for {
      u <- userNameValidation(name)
      e <- validateEmail(email)
    } yield User(u, e)
  }

}

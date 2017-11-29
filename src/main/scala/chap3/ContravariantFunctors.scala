package chap3

import cats._
import cats.implicits._
import chap1.Printable

object ContravariantFunctors extends App {

  implicit val stringPrintable = new Printable[String] {
    def format(a: String) = a
  }

  implicit val booleanPrintable = new Printable[Boolean] {
    override def format(a: Boolean) = if (a) "yes" else "no"
  }

  implicit def optionPrintable[A](implicit ev: Printable[A]) = new Printable[Option[A]] {
    def format(oa: Option[A]) = oa match {
      case None => "None"
      case Some(a) => s"Some(${ev.format(a)})"
    }
  }

  val positive: (Int => Boolean) = i => if (i < 0) false else true

  implicit val newPrintable: Printable[Int] = booleanPrintable.contramap(positive)

  final case class Box[A](value: A)

  implicit def boxPrintable[A](implicit ev: Printable[A]): Printable[Box[A]] =
    ev.contramap(_.value)

  import chap1.PrintableSyntax._

  println(Box(1).format)
  println(Box("hello").format)
  println(Box(false).format)
  println(Option(Box(1)).format)
  println(Option(Box("cat")).format)
}

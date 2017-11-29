package chap3

import cats._
import cats.implicits._
import chap3.ContravariantFunctors.Box

trait Codec[A] {
  def encode(a: A): String
  def decode(s: String): A
}

object Codec {
  def encode[A](a: A)(implicit ev: Codec[A]): String =
    ev.encode(a)
  def decode[A](a: String)(implicit ev: Codec[A]): A =
    ev.decode(a)

  implicit val codecInvariant = new Invariant[Codec] {
    def imap[A, B](fa: Codec[A])(dec: A => B)(enc: B => A): Codec[B] = {
      new Codec[B] {
        def encode(b: B): String = fa.encode(enc(b))
        def decode(s: String): B = dec(fa.decode(s))
      }
    }
  }
}

object InvariantFunctors extends App {
  implicit val stringCodec = new Codec[String] {
    def encode(a: String) = a
    def decode(s: String) = s
  }

  implicit val intCodec = stringCodec.imap(_.toInt)(_.toString)
  def boxCodec[A](implicit ev: Codec[A]): Codec[Box[A]] =
    ev.imap(Box(_))(_.value)

  val b1: Box[Int] = Box(1)
  val b2: Box[String] = Box("hello")

  println(boxCodec[Int].encode(b1))
  println(boxCodec[String].encode(b2))

  val stringMonoid = Monoid[String]

  val symbolMonoid: Monoid[Symbol] = stringMonoid.imap(Symbol(_))(_.name)
}

package chap3

import cats._

sealed trait Tree[A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](a: A) extends Tree[A]

object Tree {
  implicit val functorInstances = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B) = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(a) => Leaf(f(a))
    }
  }
}

object TreeApp extends App {
  import cats.implicits._
  val t1: Tree[Int] = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
  println(t1)
  println(t1.map(_ * 10))
}

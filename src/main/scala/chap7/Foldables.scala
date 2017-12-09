package chap7

import cats._
import cats.implicits._

object Foldables extends App {

  // will reverse
  val fromTheLeft = List(1, 2, 3).foldLeft(List.empty[Int])((acc, next) => next :: acc)
  // will preserve
  val fromTheRight = List(1, 2, 3).foldRight(List.empty[Int])(_ :: _)

  println(fromTheLeft)
  println(fromTheRight)

  def map[A, B](as: List[A])(f: A => B): List[B] =
    as.foldRight(List.empty[B])((next, acc) => f(next) :: acc)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    as.foldRight(List.empty[B])((next, acc) => f(next) ++ acc)

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as.foldRight(List.empty[A])((next, acc) => if (f(next)) next :: acc else acc)

  println(map(List(1,2,3))(_ * 10))
  println(flatMap(List(1,2,3))(a => List(a, a)))
  println(filter(List(1,2,3))(_ % 2 == 0))
}

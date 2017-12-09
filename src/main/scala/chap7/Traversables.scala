package chap7

import cats._
import cats.instances._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Traversables extends App {

  def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
    fa.foldRight(Applicative[G].pure(List.empty[B]))((next, acc) => Applicative[G].map2(f(next), acc)(_ :: _))
  }

}

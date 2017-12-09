package chap5

import cats._
import cats.data.OptionT
import cats.implicits._

object MonadTransformers extends App {

  type ListOption[A] = OptionT[List, A]

  val hrmm = OptionT(List(Option(1), None, Option(2), Option(4)))
  println(hrmm)

  val mapped = hrmm.map(i => i * 2)
  println(mapped)

  val uhHuh = hrmm.flatMap(i => if (i % 2 == 0) OptionT(List(Option(i), Option(i))) else OptionT(List.empty[Option[Int]]))

  println(uhHuh)
}

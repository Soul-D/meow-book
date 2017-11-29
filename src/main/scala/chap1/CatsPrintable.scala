package chap1

import cats._
import cats.implicits._

object ShowInstances {
  implicit val show: Show[Cat] = Show.show(c => s"Hello from ${c.name}")
}

object CatsPrintable {
  import ShowInstances._

  val ruby = Cat("Ruby", 1, "Tortoise")
  println(ruby.show)

}

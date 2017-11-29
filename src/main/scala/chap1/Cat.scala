package chap1

import cats.kernel.Eq

final case class Cat(name: String, age: Int, colour: String)

object CatInstances {
  implicit val catEq = Eq.instance[Cat] { (c1, c2) =>
    c1 == c2
  }
}

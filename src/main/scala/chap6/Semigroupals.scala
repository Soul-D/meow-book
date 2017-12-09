package chap6

import cats._
import cats.implicits._

object Semigroupals extends App {

  println(Semigroupal[Option].product(None, None))
  println(Semigroupal[Option].product(None, Option(1)))
  println(Semigroupal[Option].product(Option(1), None))
  println(Semigroupal[Option].product(Option(1), Option(2)))
  println(Semigroupal.tuple2(Option(1), Option(2)))
  println(Semigroupal.tuple3(Option(1), Option(2), Option(3)))
  println(Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _))

  // with the apply syntax
  println((Option(1), Option(2)).tupled)
  println((Option(1), Option(2), Option(3)).tupled)
  println((Option(1), Option(2), Option(3)).mapN(_ * _ * _))


  case class Cat(name: String, yearOfBirth: Int, favFoods: List[String])

  val tupleToCat: (String, Int, List[String]) => Cat = Cat.apply

  val catToTuple: Cat => (String, Int, List[String]) =
    (c: Cat) => (c.name, c.yearOfBirth, c.favFoods)

  implicit val catMonoid: Monoid[Cat] = (Monoid[String], Monoid[Int], Monoid[List[String]])
    .imapN(tupleToCat)(catToTuple)

  val ruby = Cat("Ruby", 2016, List("Meat", "Treats"))
  val bella = Cat("Bella", 2016, List("Biscuits", "Treats", "Cheese"))

  println(ruby |+| bella)
  println(Monoid[Cat].empty)
  println(Monoid[Cat].empty |+| ruby)

}

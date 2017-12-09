package chap4

import cats._
import cats.data.Reader
import cats.implicits._

object Readers extends App {
    println("Welcome to readers")

    case class Cat(name: String, favFood: String)

    val ruby = Cat("Ruby", "MEAT")
    val bella = Cat("Bella", "yummy biscuits")

    val catName: Reader[Cat, String] = Reader(cat => cat.name)
    val catFood: Reader[Cat, String] = Reader(cat => cat.favFood)
    val greetTheKitty: Reader[Cat, String] = catName.map(name => s"Hello $name")

    val catDescription = for {
        name <- catName
        favFood <- catFood
    } yield s"The awesome cat $name loves some $favFood"

    val description = catDescription.run(ruby)
    println(description)

    println(greetTheKitty(bella))
}    
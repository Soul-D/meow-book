package chap4

import cats._
import cats.data.{Writer, WriterT}
import cats.implicits._

object Writers extends App {

  type Logged[A] = Writer[Vector[String], A]

  val forty = 40.pure[Logged]
  val two = 2.pure[Logged]

  println(forty)
  println(two)

  val answer: WriterT[Id, Vector[String], Int] = for {
    x <- forty
    y <- two
  } yield x + y

  println(answer)

  def hardStuff(i: Int): Logged[Int] = {
    (i * 3).writer(Vector("well crap, i did it once", "and another", "okay it is now 3 times bigger"))
  }

  val reallyHard = answer.flatMap(hardStuff)
  println(reallyHard)

  val oneMore = for {
    x <- 42.pure[Logged]
    _ <- Vector("I did some stuff", "And more").tell
  } yield x
  println(oneMore)

}

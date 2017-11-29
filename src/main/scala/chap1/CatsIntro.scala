package chap1

import cats._
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._

object ShowRipOff {

  def showR[A](f: A => String): Show[A] = new Show[A] {
    def show(a: A) = f(a)
  }

  def toStringToShow[A]: Show[A] = new Show[A] {
    override def show(a: A) = a.toString
  }
}

object CatsIntro extends App {

  val intShow = Show[Int]
  val stringShow = Show[String]

  println(123.show)
  println("ruby".show)
}

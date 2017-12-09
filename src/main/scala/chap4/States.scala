package chap4

import cats._
import cats.data.{IndexedStateT, State}
import cats.implicits._

object States extends App {

  val a = State[Int, String] { s =>
    (s, "some value")
  }

  println(a.run(1).value)
  println(a.runS(1).value)
  println(a.runA(1).value)

  val step1 = State[Int, String] { s =>
    (s + 1, "i did step 1")
  }

  val step2 = State[Int, String] { s =>
    (s + 1, "i did step 2")
  }

  val both = for {
    one <- step1
    _ <- State.modify[Int](_ * 10)
    stateAfterModify <- State.get[Int]
    two <- step2
  } yield (stateAfterModify, one, two)

  val bothAgain: State[Int, (String, String)] =
    step1.flatMap(one => step2.map(two => (one, two)))

  println("Running some state")
  println(both.run(0).value)

  val soPure = State.pure[Int, String]("Chris")
  println(soPure.run(10).value)

  import State._

  val program = for {
    _ <- set[Int](10)
    _ <- modify[Int](_ * 10)
    a <- get[Int]
    _ <- modify[Int](_ + 10)
    b <- get[Int]
    _ <- set[Int](200)
  } yield (a, b)

  println(program.run(-1).value)

}

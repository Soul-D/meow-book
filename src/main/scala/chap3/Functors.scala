package chap3

import cats._
import cats.implicits._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object FunctionFunctors {

  val func1: Int => Double = (x: Int) => x.toDouble

  val func2: Double => Double = _ * 2

  val func3a: Int => Double = func1 andThen func2

  val func3b = func1.map(func2)

  val func: Int => String = ((x: Int) => x.toDouble).map(_ + 1).map(_ * 2).map(_ + "!")
}

object FutureFunctors extends App {
  import scala.concurrent.ExecutionContext.Implicits._
  val f1 = Future {
    1
  }
  val f2 = Future.successful(2)

  val f3 = Functor[Future].map(f1)(_ * 10)

  f3.onComplete(println)
  Await.ready(f3, Duration.Inf)
}



package chap4

import cats._
import cats.data._
import cats.implicits._
import chap4.Writers.Logged

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object TrampoliningExercise extends App {
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    def loop(as: List[A]): Eval[B] = as match {
      case Nil => Eval.now(acc)
      case x :: xs => Eval.defer(loop(xs)).map(fn(x, _))
    }

    loop(as).value
  }

  val smallList = List(1, 2, 3, 4)
  println(smallList.foldRight(1)(_ - _))
  println(foldRight(smallList, 1)(_ - _))

  val bigList = List.fill(50000)(1)

  // el broken, not any more!
  println(foldRight(bigList, 0)(_ + _))
}

object WriterExercise extends App {

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Logged[Int] = {
    val ans: WriterT[Id, Vector[String], Int] = slowly {
      if (n == 0) 1.pure[Logged]
      else factorial(n - 1).map(_ * n)
    }
    for {
      a <- ans
      _ <- Vector(s"fact $n $a").tell
    } yield a
  }

  def logIt[A](logged: Logged[A]): Unit = {
    val (log, answer) = logged.run
    println("Answer is: " +  answer)
    println("How we got it: ")
    log.foreach(println)
  }

  val one = Await.result(Future(factorial(5)), Duration.Inf)
  val two = Await.result(Future(factorial(10)), Duration.Inf)
  logIt(one)
  logIt(two)
}

object ReadersExercise extends App {
  println("Welcome to readers exercise")

  case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def findPassword(username: String): DbReader[String] =
    Reader(db => db.passwords(username))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
    findUsername(userId).flatMap {
      case Some(userName) => findPassword(userName).map(_ == password)
      case None => Reader(_ => false)
    }
  }

  val db = Db(
    usernames = Map(1 -> "chbatey"),
    passwords = Map("chbatey" -> "catsrock")
  )

  val checkMe = checkLogin(1, "cats")
  val checkMeAgain = checkLogin(1, "catsrock")
  val checkNoOne = checkLogin(10, "silly")

  println(checkMe.run(db))
  println(checkMeAgain.run(db))
  println(checkNoOne.run(db))

}

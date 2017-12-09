package chap5

import cats._
import cats.data.EitherT
import cats.implicits._

import scala.concurrent.Await.result
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object MonadTransformerExercise extends App {

  type Response[A] = Future[Either[String, A]]

  val powerLevels = Map(
    "jazz" -> 6,
    "bumblebee" -> 8,
    "hot rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] =
    Future(powerLevels.get(autobot).toRight(s"Autobot $autobot not found"))

  println(result(getPowerLevel("jazz").map(e => e.map(_ * 11)), Duration.Inf))

  type ResponseT[A] = EitherT[Future, String, A]

  def getPowerLevelT(autobot: String): ResponseT[Int] =
    EitherT(Future(powerLevels.get(autobot).toRight(s"Autobot $autobot not found")))

  def canSpecialMove(ally1: String, ally2: String): ResponseT[Boolean] = {
    for {
      a1 <- getPowerLevelT(ally1)
      a2 <- getPowerLevelT(ally2)
    } yield a1 + a2 > 15
  }

  def tacticalReport(ally1: String, ally2: String): Future[String] = {
    canSpecialMove(ally1, ally2).value.map {
      case Right(false) => s"$ally1 and $ally2 need a recharge"
      case Right(true) => s"$ally1 and $ally2 ready to roll"
      case Left(s) => s
    }
  }

  println(result(getPowerLevelT("jazz").map(_ * 10).value, Duration.Inf))
  println(result(getPowerLevelT("chbatey").map(_ * 10).value, Duration.Inf))
  println(result(canSpecialMove("chbatey", "jazz").value, Duration.Inf))
  println(result(canSpecialMove("hot rod", "jazz").value, Duration.Inf))
  println(result(tacticalReport("hot rod", "jazz"), Duration.Inf))
  println(result(tacticalReport("cat", "jazz"), Duration.Inf))
  println(result(tacticalReport("jazz", "jazz"), Duration.Inf))
}

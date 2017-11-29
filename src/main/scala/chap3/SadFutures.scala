package chap3


import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.Random

object Futures extends App {
  val f1 = {
    val r = new Random(0L)
    val x = Future(r.nextInt)
    for {
      a <- x
      b <- x
    } yield (a, b)
  }

  // Same as f1, but I inlined `x`
  val f2 = {
    val r = new Random(0L)
    for {
      a <- Future(r.nextInt)
      b <- Future(r.nextInt)
    } yield (a, b)
  }

  f1.onComplete(println)
  f2.onComplete(println)

  Await.ready(Future.sequence(List(f1, f2)), Duration.Inf)
}
















object HappyTask extends App {
  import scalaz.concurrent.Task

  val task1 = {
    val r = new Random(0L)
    val x = Task.delay(r.nextInt)
    for {
      a <- x
      b <- x
    } yield (a, b)
  }

  val task2 = {
    val r = new Random(0L)
    for {
      a <- Task.delay(r.nextInt)
      b <- Task.delay(r.nextInt)
    } yield (a, b)
  }

  println(task1.unsafePerformSync)
  println(task2.unsafePerformSync)
}

package chap9

import cats._
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object MapReduce extends App {
  def foldMap[A, B: Monoid](as: Vector[A])(f: A => B): B = {
    as.map(f).foldLeft(Monoid[B].empty)(Monoid[B].combine)
  }

  def pFoldMap[A, B: Monoid](as: Vector[A])(f: A => B): Future[B] = {
    val nrCores = Runtime.getRuntime.availableProcessors()
    val groupSize = math.max(1, as.size / nrCores)
    val grouped: Iterator[Vector[A]] = as.grouped(groupSize)
    val comps: Iterator[Future[B]] = grouped map { g =>
      Future(foldMap(g)(f))
    }
    Future.sequence(comps).map { bs =>
      bs.foldLeft(Monoid[B].empty)(Monoid[B].combine)
    }
  }

  def catyParallelFoldMap[A, B: Monoid](as: Vector[A])(f: A => B): Future[B] = {
    val nrCores = Runtime.getRuntime.availableProcessors()
    val groupSize = math.max(1, as.size / nrCores)
    as.grouped(groupSize)
      .toVector
      .traverse((g: Vector[A]) => Future(g.foldMap(f)))
      .map(v => Monoid[B].combineAll(v))
  }

  val folded = catyParallelFoldMap(Vector(1, 2, 3))(identity)
  println(Await.result(folded, Duration.Inf))
}

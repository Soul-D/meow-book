package chap4

import cats.Monad
import cats.implicits._
import chap3.{Branch, Leaf, Tree}

import scala.annotation.tailrec

object TreeExercise extends App {

  implicit val treeMonad = new Monad[Tree] {
    def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      case Leaf(a) => f(a)
    }

    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      f(a) match {
        case Leaf(Left(aa)) => tailRecM(aa)(f)
        case Leaf(Right(bb)) => Leaf(bb)
        case Branch(left, right) =>
          Branch(
            flatMap(left) {
              case Left(aaa) => tailRecM(aaa)(f)
              case Right(bbb) => Leaf(bbb)
            },
            flatMap(right) {
              case Left(aaa) => tailRecM(aaa)(f)
              case Right(bbb) => Leaf(bbb)
            }
          )
      }
    }

    def pure[A](x: A): Tree[A] = Leaf(x)
  }

  val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
  println(tree)
  val sameTree = tree.flatMap(Monad[Tree].pure)
  println(tree)
  val f: Int => Tree[Double] = (i: Int) => Branch(Leaf(i.toDouble), Leaf(i.toDouble))
  println(tree.flatMap(f))
}


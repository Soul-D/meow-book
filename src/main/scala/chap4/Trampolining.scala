package chap4

import cats._

import scala.annotation.tailrec

object Trampolining extends App {

  def fac(n: Long): Eval[BigInt] = {
    if (n == 1)
      Eval.now(BigInt(1))
    else
      Eval.defer(fac(n - 1)).map(_ * n)
  }

  println(fac(1).value)
  println(fac(7).value)
  println(fac(50000).value)

}

object BrokenStack extends App {
  def fac2(n: Long): BigInt = {
    @tailrec
    def go(acc: BigInt, n: Long): BigInt = {
      if (n == 1) acc
      else go(acc * n, n - 1)
    }
    go(1, n)
  }
  def fac(n: Long): Long = {
    if (n == 1) 1
    else n * fac(n - 1)
  }
  println(fac(1))
  println(fac(7))
//  println(fac(50000))
  println(fac2(50000))
}

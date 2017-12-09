package chap4

import cats._
import cats.data.State
import cats.implicits._


object StateExercise extends App {

  type CalcState[A] = State[List[Int], A]

  def evalOne(s: String): CalcState[Int] = {

    def popAndCalc(stack: List[Int], calc: (Int, Int) => Int): (List[Int], Int) = {
      stack match {
        case x :: y :: tail => (calc(x, y) :: tail, calc(x, y))
        case _ => throw new IllegalStateException(s"stack: $stack s: $s")
      }
    }

    State[List[Int], Int] { oldStack =>
      val next: String = s.takeWhile(_ != ' ')
      next match {
        case "+" => popAndCalc(oldStack, _ + _)
        case "-" => popAndCalc(oldStack, _ - _)
        case "*" => popAndCalc(oldStack, _ * _)
        case "/" => popAndCalc(oldStack, _ / _)
        case _ => (next.toInt :: oldStack, 0)
      }
    }
  }

  def evalAll(s: List[String]): CalcState[Int] =
    s.map(evalOne).foldLeft(0.pure[CalcState]) { (left, right) => left.flatMap(_ => right) }

  def calculate(s: String): Int =
    evalAll(s.split(" ").toList).runA(Nil).value

  val program = for {
    _ <- evalOne("42")
    _ <- evalOne("1")
    ans <- evalOne("+")
  } yield ans

  println(program.run(List.empty[Int]).value)

  val ans: CalcState[Int] = evalAll(List("42", "1", "+"))
  println(ans.run(Nil).value)

  val program2 = for {
    ans1 <- evalAll(List("1", "2", "+"))
    ans2 <- evalAll(List("10", "20", "+"))
    ans3 <- evalAll(List("6", "5", "*"))
  } yield ans1 + ans2 + ans3

  println(program2.run(Nil).value)

  println(calculate("1 2 + 3 4 + *"))
}

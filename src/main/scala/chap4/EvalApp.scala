package chap4

import cats.Eval


object EvalApp extends App {

  val now = Eval.now({
    println("is it now??")
    math.random()
  })
  println(now)
  println(now)

  val later = Eval.later({
    println("is it now?? or later?")
    math.random()
  })

  println(later)
  println("later has not been evaluated")
  println(later.value)
  println(later.value)
  println("now it has")

  val always = Eval.always({
    println("Now or later? How about later and later?")
    math.random()
  })

  println(always)
  println("you'll have to do better than that")
  println(always.value)
  println(always.value)

  println("omg it is a monad")

  val answer: Eval[Int] = for {
    a <- Eval.now({
      println("Calculating 40. NOW!")
      40
    })
    b <- Eval.later({
      println("Calculating 2. Later...")
      2
    })
  } yield {
    println("Adding a and b")
    a + b
  }

  println(answer)
  println("Not yet...")
  println(answer.value)
  println("And again")
  println(answer.value)

  println("one more time")
  val answerAgain: Eval[Int] = Eval.now({
    println("Calculating 40. NOW!")
    40
  }).flatMap(a => Eval.later({
    println("Calculating 2. Later...")
    2
  }).map(b => {
    println("Adding a and b")
    a + b
  }))

  println(answerAgain)
  println("Not yet...")
  println(answerAgain.value)
  println("And again")
  println(answerAgain.value)

  println("Once more")
  val answerAgainAgain: Eval[Int] = Eval.now({
    println("Calculating 40. NOW!")
    40
  }).flatMap(a => Eval.later({
    println("Calculating 2. Later...")
    2
  }).map(b => {
    println("Adding a and b")
    a + b
  })).memoize


  println(answerAgainAgain)
  println("Not yet...")
  println(answerAgainAgain.value)
  println("And again")
  println(answerAgainAgain.value)

  val alwaysMeansAlwaysOrDoesIt = (for {
    n <- Eval.always {
      println("Calculating 10"); 10
    }
    y <- Eval.now(20)
  } yield n + y).memoize

  println(alwaysMeansAlwaysOrDoesIt.value)
  println(alwaysMeansAlwaysOrDoesIt.value)
}

package chap1

import java.util.Date

import cats._

object DateEq {

  implicit val dateEq = Eq.instance[Date] { (d1, d2) =>
    d1.getTime == d2.getTime
  }

}

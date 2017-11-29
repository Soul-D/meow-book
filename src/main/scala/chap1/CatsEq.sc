import java.util.Date

import cats._
import cats.implicits._
import chap1.Cat
import chap1.DateEq._
import chap1.CatInstances._

val eqInt = Eq[Int]

1 === 2
2 === 2
1 =!= 2

1.some === none[Int]

val d1 = new Date()
val d2 = new Date()
val d3 = new Date(d1.getTime)

d1 === d2
d1 === d2
d1 === d3

val ruby = Cat("Ruby", 1, "Tourtoise")
val bella = Cat("Bella", 1, "Tourtoise")

ruby === bella
ruby === ruby

val maybeRuby = ruby.some
val maybeBella = bella.some
val noCat = none[Cat]

maybeRuby === noCat
maybeRuby === maybeBella
maybeRuby === maybeRuby

Some(ruby) === ruby


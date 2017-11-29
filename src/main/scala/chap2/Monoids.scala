package chap2

import cats._
import cats.implicits._
import scala.language.higherKinds

case class CustomerOrder(totalCost: Double, quantity: Int)

object OrderInstances {
  implicit val customerOrderMonoid = new Monoid[CustomerOrder] {
    def empty = CustomerOrder(0,0)
    def combine(x: CustomerOrder, y: CustomerOrder): CustomerOrder =
      CustomerOrder(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }
}

object Monoids {
  def add[A:Monoid, F[_]: Traverse](items: F[A]) =
    items.foldLeft(Monoid[A].empty)(Monoid[A].combine)
}

object MonoidLaws {
  def associativity[A: Monoid : Eq](a1: A, a2: A): Boolean =
    (a1 |+| a2) === (a2 |+| a1)

  def identity[A: Monoid : Eq](a: A): Boolean =
    (a |+| Monoid[A].empty) === a
}

object BooleanMonoid {
  implicit val orMonoid = new Monoid[Boolean] {
    def empty: Boolean = false
    def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  implicit val andMonoid = new Monoid[Boolean] {
     def empty = true
     def combine(x: Boolean, y: Boolean) = x && y
  }
}

object SetMonoids {
  implicit def setUnionMonoid[A]() = new Monoid[Set[A]] {
    def empty = Set()
    def combine(x: Set[A], y: Set[A]) = x ++ y
  }

  implicit def setIntersectionSemigroup[A]() = new Semigroup[Set[A]] {
    def combine(x: Set[A], y: Set[A]) = x.intersect(y)
  }
}

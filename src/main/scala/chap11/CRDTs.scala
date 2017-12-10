package chap11

import cats._
import cats.implicits._

object CRDTs extends App {

  // A commutative idempotent monoid
  trait BoundedSemiLattice[A] {
    def combine(a1: A, a2: A): A
    def empty: A
  }

  implicit val intBoundedSemiLattice = new BoundedSemiLattice[Int] {
    override def combine(a1: Int, a2: Int): Int = math.max(a1, a2)
    override def empty = 0
  }

  implicit def setBoundedSemiLattice[A] = new BoundedSemiLattice[Set[A]] {
    override def combine(a1: Set[A], a2: Set[A]): Set[A] =
      a1 union a2
    override def empty = Set.empty[A]
  }

  trait GCounter[F[_, _], K, V] {
    def increment(f: F[K, V])(k: K, v: V): F[K, V]
    def merge(f: F[K, V], other: F[K, V]): F[K, V]
    def total(f: F[K, V]): V
  }

  object GCounter {
    def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) = counter
  }

  implicit def mapGCounter[A](implicit m: Monoid[A], bs: BoundedSemiLattice[A]) = new GCounter[Map, String, A] {

    def increment(f: Map[String, A])(k: String, v: A): Map[String, A] = {
      val x = m.combine(f.getOrElse(k, m.empty), v)
      f.updated(k, x)
    }

    def merge(f: Map[String, A], other: Map[String, A]): Map[String, A] =
      (f.keys ++ other.keys)
        .map(key => (key, bs.combine(f.getOrElse(key, bs.empty), other.getOrElse(key, bs.empty))))
        .toMap

    def total(f: Map[String, A]) =
      f.values.foldLeft(m.empty)(m.combine)
  }


  case class GCounterMap[A](counts: Map[String, A]) {
    def increment(machine: String, amount: A)(implicit a: Monoid[A]) =
      copy(counts = counts.updated(machine, a.combine(counts.getOrElse(machine, a.empty), amount)))

    def merge(other: GCounterMap[A])(implicit a: BoundedSemiLattice[A]): GCounterMap[A] = {
      GCounterMap((counts.keys ++ other.counts.keys)
        .map(key => (key, a.combine(counts.getOrElse(key, a.empty), other.counts.getOrElse(key, a.empty))))
        .toMap)
    }

    def total(implicit a: Monoid[A]): A = counts.values.foldLeft(a.empty)(a.combine)
  }

  implicit def gcounterMonoid[A: BoundedSemiLattice] = new Monoid[GCounterMap[A]] {
    override def empty = GCounterMap(Map.empty[String, A])
    override def combine(x: GCounterMap[A], y: GCounterMap[A]) = x.merge(y)
  }

  val counter1 = GCounterMap(Map("A" -> 3, "B" -> 2, "D" -> 1))
  println(counter1)
  println(counter1.total)
  val counter2 = GCounterMap(Map("A" -> 2, "B" -> 4, "E" -> 10))
  println(counter2)
  println(counter2.total)

  val combined = counter1 |+| counter2
  println(combined)
  println(combined.total)

  // with the generic version:

  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)
  val counter = GCounter[Map, String, Int]
  val merged = counter.merge(g1, g2)
  println(merged)
  val total = counter.total(merged)
  println(total)

}

package chap1

trait Printable[A] { self =>
  def format(a: A): String

  def contramap[B](f: B => A): Printable[B] = new Printable[B] {
    def format(b: B): String = self.format(f(b))
  }
}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    def format(a: String): String = a
  }

  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    def format(a: Int): String = a.toString
  }

  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    override def format(a: Cat) = s"Meow from ${a.name}"
  }
}

object Printable {
  def format[A](a: A)(implicit ev: Printable[A]): String =
    ev.format(a)

  def print[A](a: A)(implicit ev: Printable[A]): Unit =
    println(format(a))
}

object PrintableSyntax {
  implicit class PrintableOps[A: Printable](a: A) {
    def format: String = implicitly[Printable[A]].format(a)
  }
}

object PrintableApp extends App {
  import PrintableInstances._
  import PrintableSyntax._
  import Printable._

  print(50)
  print("Cats")

  val ruby = Cat("Ruby", 1, "tortoise")

  println(ruby.format)
}



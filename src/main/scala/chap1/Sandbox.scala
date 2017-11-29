package chap1

sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsNumber(get: Double) extends Json
final case class JsString(get: String) extends Json
case object JsNull extends Json

object Json {
  def toJson[A](a: A)(implicit ev: JsonWriter[A]): Json =
    ev.write(a)
}

object JsonSyntax {
  implicit class JsonWriterOps[A](a: A)(implicit ev: JsonWriter[A]) {
    def toJson: Json = ev.write(a)
  }
}

trait JsonWriter[-A] {
  def write(a: A): Json
}

object JsonWriter {
  implicit val stringJsonWriter: JsonWriter[String] = new JsonWriter[String] {
    def write(a: String): Json = JsString(a)
  }
}

final case class Person(name: String, email: String)

object JsonWriterInstances {

  implicit def optionJsonWriter[A](implicit ev: JsonWriter[A]): JsonWriter[Option[A]] = new JsonWriter[Option[A]] {
    override def write(oa: Option[A]) = oa match {
      case None => JsNull
      case Some(a) => implicitly[JsonWriter[A]].write(a)
    }
  }

  implicit val personJsonWriter: JsonWriter[Person] = new JsonWriter[Person] {
    def write(a: Person): Json = JsObject(Map(
      "name" -> JsString(a.name),
      "email" -> JsString(a.email)
    ))
  }

}


object Sandbox extends App {

  import JsonWriterInstances._
  import JsonSyntax._

  val me = Person("Christopher", "c@b.com")

  println(Json.toJson(me))
  println(me.toJson)

  println(Json.toJson("cats"))
  println("cats".toJson)

  println((None: Option[Person]).toJson)
  println(Option(me).toJson)
}


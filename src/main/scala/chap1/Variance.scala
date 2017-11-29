package chap1

object Variance extends App {
  sealed trait Shape
  case class Square() extends Shape
  case class Circle() extends Shape

  val shapeWriter = new JsonWriter[Shape] {
    def write(a: Shape): Json = JsString("who knows, some shape")
  }

  val squareWriter = new JsonWriter[Square] {
    def write(a: Square) = JsString("omg a square")
  }

  def iWantToWrite[A: JsonWriter](a: A): Json = {
    implicitly[JsonWriter[A]].write(a)
  }

  val square: Square = new Square
  val circle: Circle = new Circle
  val shape: Shape  =new Shape {}

  // even invariance works here
  iWantToWrite(square)(squareWriter)
  // obv no
  //iWantToWrite(circle)(squareWriter)
  // A shape writer should be able to write a square writer so it needs to be contravariant
  iWantToWrite(square)(shapeWriter)
  iWantToWrite(shape)(shapeWriter)
  // Whaa it could be any type of shape but we provide a square writer, covariance would be wrong here
  //iWantToWrite(shape)(squareWriter)

  {
    // This is a contravariant type class so the bottom of the type heirarchy for
    // JsonWriter
    implicit val sw: JsonWriter[Shape] = shapeWriter
    implicit val sqw: JsonWriter[Square] = squareWriter

    println("Explicit shape: " + iWantToWrite(square)(sw))
    println("Explicit square: " + iWantToWrite(square)(sqw))
    println("Implicit: " + iWantToWrite(square))
  }


}

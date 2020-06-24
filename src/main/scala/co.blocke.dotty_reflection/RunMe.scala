package co.blocke.dotty_reflection

// import impl._ 
import info._
import co.blocke.reflect._

case class ParamOption2[T](a: Option[T])
// Self-referencing
case class Shape(id: Int, parent: Option[Shape])
case class Drawer[T]( id: Int, nextInChain: Option[Drawer[T]], thing: T)

object RunMe extends App:

  // val r = Reflector.reflectOn[Shape]
  val r = Reflector.reflectOn[Drawer[Shape]]
  println(r)

  println("done.")

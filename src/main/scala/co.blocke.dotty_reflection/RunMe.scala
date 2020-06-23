package co.blocke.dotty_reflection

// import impl._ 
import info._
import co.blocke.reflect._

case class ParamOption2[T](a: Option[T])

object RunMe extends App:

  println(Reflector.reflectOn[ParamOption2[Int]])
  println(Reflector.reflectOn[ParamOption2[Boolean]])


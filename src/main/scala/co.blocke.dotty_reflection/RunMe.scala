package co.blocke.dotty_reflection

// import impl._ 
import info._
import co.blocke.reflect._

// case class Foom[T,U]( a: Int, b: List[T], c: U )
case class Blather[U]( b: U )
case class Foom[T,U]( a: List[U], b: T)


object RunMe extends App:

  println(Reflector.reflectOn[Foom[String, Either[Int,Boolean]]])
  // println(Reflector.reflectOn[Foom[Int]])

  println("done.")
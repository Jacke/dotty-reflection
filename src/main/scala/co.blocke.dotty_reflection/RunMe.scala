package co.blocke.dotty_reflection

// import impl._ 
import info._
import co.blocke.reflect._

// case class Foom[T,U]( a: Int, b: List[T], c: U )
case class Blather[U]( b: U )
case class Foom[T,U]( a: java.util.HashMap[String,U], b: T)


object RunMe extends App:

  // val m = Map("foom".asInstanceOf[TypeSymbol] -> PrimitiveType.Scala_Double )
  // val lift = new quoted.Liftable[Map[TypeSymbol, RType]]
  // val z = lift.toExpr(m)

  // println(Reflector.reflectOn[JavaParam[Double]])

  println("done.")
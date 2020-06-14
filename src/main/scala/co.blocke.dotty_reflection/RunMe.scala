package co.blocke.dotty_reflection

// import impl._ 
import info._
import co.blocke.reflect._

// case class Foom[T,U]( a: Int, b: List[T], c: U )
case class Blather[U]( b: U )
case class Foom[T,U]( a: java.util.HashMap[String,U], b: T)

trait Findable[F]{ val x: F }
case class Lost[L]( x: L ) extends Findable[L]

trait Base[B]{ val b: B }
trait Mid[M] extends Base[M]
case class Top[T]( b: T ) extends Mid[T]


object RunMe extends App:

  // val m = Map("foom".asInstanceOf[TypeSymbol] -> PrimitiveType.Scala_Double )
  // val lift = new quoted.Liftable[Map[TypeSymbol, RType]]
  // val z = lift.toExpr(m)

  // val f = Reflector.reflectOn[Findable[Int]].asInstanceOf[TraitInfo]
  // println(Reflector.reflectOnClass(Class.forName("co.blocke.dotty_reflection.Lost"), Some(f)))

  val f = Reflector.reflectOn[Base[Int]].asInstanceOf[TraitInfo]
  println(Reflector.reflectOnClass(Class.forName("co.blocke.dotty_reflection.Top"), Some(f)))

  println("done.")
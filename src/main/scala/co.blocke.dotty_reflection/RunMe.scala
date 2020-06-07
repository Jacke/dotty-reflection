package co.blocke.dotty_reflection

// import impl._ 
import info._

// opaque type Mepo = Person
// case class Boom(a: Mepo)
case class Blap(b: Array[Int])


object RunMe extends App:

  // println(Reflector.reflectOnClass(Class.forName("co.blocke.dotty_reflection.WeekDay")))
  val i = Reflector.reflectOn[Blap]
  println(i)

  // println(Reflector.reflectOn[VehicleHolder2])
  // println(Reflector.reflectOn[Animal[Int]])
  // println(Reflector.reflectOn[Car])
  println("done.")
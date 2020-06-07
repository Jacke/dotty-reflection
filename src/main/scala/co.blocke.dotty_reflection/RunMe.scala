package co.blocke.dotty_reflection

// import impl._ 
import info._

// opaque type Mepo = Person
// case class Boom(a: Mepo)
case class Blap(b: Any = "nope")


object RunMe extends App:

  // println(Reflector.reflectOnClass(Class.forName("co.blocke.dotty_reflection.WeekDay")))
  val i = Reflector.reflectOn[Blap].asInstanceOf[ScalaCaseClassInfo]
  println(i)
  println("Default: "+i.fields.find(_.name == "b").get.asInstanceOf[ScalaFieldInfo].defaultValue)

  // println(Reflector.reflectOn[VehicleHolder2])
  // println(Reflector.reflectOn[Animal[Int]])
  // println(Reflector.reflectOn[Car])
  println("done.")
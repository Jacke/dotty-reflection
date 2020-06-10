package co.blocke.dotty_reflection

// import impl._ 
import info._
import co.blocke.reflect._

class FooRa(val x: Int, val y: Boolean) {
  @FieldAnno(idx=99)
  var z: Int = 5

  private var _q: HooLoo =null
  @FieldAnno(idx=3)
  def q: HooLoo = _q
  def q_=(s:HooLoo) = _q = s

}


object RunMe extends App:

  println(Reflector.reflectOn[FooRa])

  val c = Class.forName("co.blocke.dotty_reflection.FooRa")
  println("Methods: "+c.getMethods.toList.mkString("\n   "))

  // println(Reflector.reflectOnClass(Class.forName("co.blocke.dotty_reflection.WeekDay")))
  // val i = Reflector.reflectOn[Birthday]
  // println(i)

  // println(Reflector.reflectOn[VehicleHolder2])
  // println(Reflector.reflectOn[Animal[Int]])
  // println(Reflector.reflectOn[Car])
  println("done.")
package co.blocke.dotty_reflection

// import impl._ 
import info._

case class Holder2[T](value: T)


object RunMe extends App:

  val r = Reflector.reflectOn[Holder2[java.lang.Number]]
  println(r)
  val r2 = Reflector.reflectOn[Holder2[Double]]
  println(r2)
  val r3 = Reflector.reflectOn[Holder2[Int]]
  println(r3)
  val r4 = Reflector.reflectOn[Holder2[List[String]]]
  println(r4)
  val r5 = Reflector.reflectOn[Holder2[Option[Byte]]]
  println(r5)


  println("done.")

  def methods(clazz: Class[_]): String = 
    s"=== Methods: ${clazz.getName} ===\n   " + clazz.getMethods.toList.mkString("\n   ")
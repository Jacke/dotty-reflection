package co.blocke.dotty_reflection

// import impl._ 
// import info._

opaque type Mepo = Int
case class Boom(a: Mepo)
case class Blap(b: Any)


object RunMe extends App:

  println(Reflector.reflectOn[Boom])
  println(Reflector.reflectOn[Blap])

  // println(Reflector.reflectOn[VehicleHolder2])
  // println(Reflector.reflectOn[Animal[Int]])
  // println(Reflector.reflectOn[Car])
  println("done.")
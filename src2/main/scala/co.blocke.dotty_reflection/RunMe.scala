package co.blocke.dotty_reflection

// import impl._ 
// import info._


case class HooLoo(name: String, more: HooLoo)
case class Person(name: String, age:Int)


object RunMe extends App:

  println(Reflector.reflectOn[HooLoo])
  println(Reflector.reflectOn[Person])
  println("done.")
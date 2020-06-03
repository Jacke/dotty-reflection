package co.blocke.dotty_reflection

// import impl._ 
// import info._



object RunMe extends App:

  println(Reflector.reflectOn[Meep])
  // println(Reflector.reflectOn[Animal[Int]])
  // println(Reflector.reflectOn[Car])
  println("done.")
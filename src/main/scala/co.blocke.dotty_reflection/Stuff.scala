package co.blocke.dotty_reflection


case class HooLoo(name: String, more: HooLoo)
case class Person(name: String, age:Int)
case class Foom[T](a:T)

trait Animal[T] {
  val foom: T
}

trait Car {
  val woof: Int
}

/*
Interesting!  A trait w/no type params is a class type with flags set as trait.
If the trait has type params then it is an AppliedType!  Woof!

Same for classes!  Its an Applied type of it has type parameters
*/
package co.blocke.dotty_reflection

import co.blocke.reflect._

case class HooLoo(name: String, more: HooLoo)
case class Person(name: String, age:Int)
case class Foom[T](a:T)

trait Animal[T] {
  val foom: T
}

trait Car {
  val woof: Int
}


class Foundation( 
  @FieldAnno(idx=5) frame: String 
)

trait BuildableThing

@ClassAnno(name="Home")
case class House(
  frame: String 
) extends Foundation(frame) with BuildableThing

/*
Interesting!  A trait w/no type params is a class type with flags set as trait.
If the trait has type params then it is an AppliedType!  Woof!

Same for classes!  Its an Applied type of it has type parameters
*/

// Scala 2.x style Enumeration
object WeekDay extends Enumeration {
  type WeekDay = Value
  val Monday = Value(1)
  val Tuesday = Value(2)
  val Wednesday = Value(3)
  val Thursday = Value(4)
  val Friday = Value(5)
  val Saturday = Value(6)
  val Sunday = Value(-3)
}
import WeekDay._

// Scala 3 Enum
enum Month {
  case Jan, Feb, Mar
}

// case class Meep( a: WeekDay )
// case class Meep( b: Month)
case class Meep( a: WeekDay, b: Month)

opaque type EMP_ID2 = Person
case class Employee2(eId: EMP_ID2, age: Int)
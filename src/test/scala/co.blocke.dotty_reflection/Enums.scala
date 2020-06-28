package co.blocke.dotty_reflection

import munit._
import info._
import PrimitiveType._

class Enums extends munit.FunSuite:

  test("Java Enums") {
    val result = Reflector.reflectOn[co.blocke.reflect.JavaEnum]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.JavaEnum):
    |   fields:
    |      (0) color: JavaEnumInfo(co.blocke.reflect.Color)
    |""".stripMargin)
  }

  test("Scala Enums (old and new)") {
    val result = Reflector.reflectOn[Birthday]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.dotty_reflection.Birthday):
    |   fields:
    |      (0) m: ScalaEnumInfo(co.blocke.dotty_reflection.Month) with values [Jan,Feb,Mar]
    |      (1) d: ScalaEnumerationInfo(co.blocke.dotty_reflection.WeekDay) with values [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]
    |""".stripMargin)
  }

  test("Scala Enum methods") {
    val result = Reflector.reflectOn[Birthday]
    result match {
      case sc: ScalaCaseClassInfo => 
        val e = sc.fields(0).asInstanceOf[ScalaFieldInfo].fieldType.asInstanceOf[ScalaEnumInfo]
        assertEquals( e.valueOf("Jan"), Month.Jan )
        assertEquals( e.ordinal("Feb"), 1 )
        assertEquals( e.valueOf(2), Month.Mar )
      case _ => false
    }
  }

  test("Scala2 Enumeration methods") {
    val result = Reflector.reflectOn[Birthday]
    result match {
      case sc: ScalaCaseClassInfo => 
        val e = sc.fields(1).asInstanceOf[ScalaFieldInfo].fieldType.asInstanceOf[ScalaEnumerationInfo]
        assertEquals( e.valueOf("Monday"), WeekDay.Monday )
        assertEquals( e.ordinal("Wednesday"), 99 )
        assertEquals( e.valueOf(99), WeekDay.Wednesday )
      case _ => false
    }
  }
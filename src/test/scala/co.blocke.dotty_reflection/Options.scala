package co.blocke.dotty_reflection

import munit._
import info._
import PrimitiveType._
import java.util.Optional

class Options extends munit.FunSuite:

  test("Scala optional field") {
    val result = Reflector.reflectOn[NormalOption]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.NormalOption):
    |   fields:
    |      (0) a: Option of scala.Int""".stripMargin)
  }

  test("Java optional field") {
    val result = Reflector.reflectOn[co.blocke.reflect.JavaOption1]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.JavaOption1):
    |   fields:
    |      (0) fld: Optional of java.lang.Integer""".stripMargin)
  }

  test("Scala nested optional field") {
    val result = Reflector.reflectOn[NestedOption]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.NestedOption):
    |   fields:
    |      (0) a: Option of Option of scala.Int""".stripMargin)
  }

  test("Java nested optional field") {
    val result = Reflector.reflectOn[co.blocke.reflect.JavaOption2]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.JavaOption2):
    |   fields:
    |      (0) fld: Optional of Optional of java.lang.Integer""".stripMargin)
  }

  test("Scala optional parameterized field") {
    val result = Reflector.reflectOn[ParamOption[Char]]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.ParamOption[T]):
    |   fields:
    |      (0) a: Option of scala.Char""".stripMargin)
  }

  test("Java optional parameterized field") {
    val result = Reflector.reflectOn[co.blocke.reflect.JavaOption3[Char]]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.JavaOption3[T]):
    |   fields:
    |      (0) fld: Optional of scala.Char""".stripMargin)
  }

  test("Option assignments in union type") {
    val r = Reflector.reflectOn[UnionHavingOption].asInstanceOf[ScalaClassInfo]
    assert(
      r.constructWith[UnionHavingOption](List(None,Optional.empty())) == UnionHavingOption(None,Optional.empty())
    )
    assert(
      r.constructWith[UnionHavingOption](List(Some(3),Optional.of(3))) == UnionHavingOption(Some(3),Optional.of(3))
    )
  }

  test("Option of a union") {    
    val result = Reflector.reflectOn[OptionHavingUnion]
    assertEquals( result.show(), """ScalaClassInfo(co.blocke.dotty_reflection.OptionHavingUnion):
    |   fields:
    |      (0) a: Option of Union:
    |         left--scala.Boolean
    |         right--java.lang.String""".stripMargin)
  }

  test("Option of a union assignment") {    
    val r = Reflector.reflectOn[OptionHavingUnion].asInstanceOf[ScalaClassInfo]
    assert(
      r.constructWith[OptionHavingUnion](List(None)) == OptionHavingUnion(None)
    )
    assert(
      r.constructWith[OptionHavingUnion](List(Some(true))) == OptionHavingUnion(Some(true))
    )
    assert(
      r.constructWith[OptionHavingUnion](List(Some("wow"))) == OptionHavingUnion(Some("wow"))
    )
  }

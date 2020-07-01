package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class ScalaArrayExtractor() extends TypeInfoExtractor[ArrayInfo]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean =
    // Try here because non-library symbol won't have a class and will explode.
    scala.util.Try( ScalaArrayClazz.isAssignableFrom( Class.forName(symbol.fullName) ) ).toOption.getOrElse(false)


  def extractInfo(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    symbol: reflect.Symbol): RType =

    val elementType = paramMap.values.head
    val mangled = mangleArrayClassName(elementType)
    ArrayInfo(
      mangled,
      elementType)


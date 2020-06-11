package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class TryExtractor() extends TypeInfoExtractor[TryInfo]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean = symbol.fullName == TryClazz.getName


  def emptyInfo(reflect: Reflection)(symbol: reflect.Symbol, paramMap: Map[TypeSymbol,RType]): TryInfo =
    val classDef = symbol.tree.asInstanceOf[reflect.ClassDef]
    val tryParamSymName = classDef.constructor.paramss.head.head.toString
    val tryParamType = paramMap.getOrElse(
      tryParamSymName.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(tryParamSymName)
      )
    TryInfo(
      symbol.fullName, 
      tryParamType
      )


  def extractInfo(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    symbol: reflect.Symbol): RType =

      TryInfo(
        t.classSymbol.get.fullName,
        Reflector.unwindType(reflect, paramMap)(tob.head.asInstanceOf[reflect.TypeRef])
      )

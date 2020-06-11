package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class OptionExtractor() extends TypeInfoExtractor[ScalaOptionInfo]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean = symbol.fullName == OptionClazz.getName


  def emptyInfo(reflect: Reflection)(symbol: reflect.Symbol, paramMap: Map[TypeSymbol,RType]): ScalaOptionInfo =
    val classDef = symbol.tree.asInstanceOf[reflect.ClassDef]
    val optionParamSymName = classDef.constructor.paramss.head.head.toString
    val optionParamType = paramMap.getOrElse(
      optionParamSymName.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(optionParamSymName)
      )

    ScalaOptionInfo(
      symbol.fullName, 
      optionParamType
      )


  def extractInfo(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    symbol: reflect.Symbol): RType =

      ScalaOptionInfo(t.classSymbol.get.fullName, Reflector.unwindType(reflect, paramMap)(tob.head.asInstanceOf[reflect.TypeRef]))

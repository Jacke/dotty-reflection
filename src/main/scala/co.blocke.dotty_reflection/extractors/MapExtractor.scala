package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class MapExtractor() extends TypeInfoExtractor[MapLikeInfo]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean =
    // Try here because non-library symbol won't have a class and will explode.
    scala.util.Try( MapClazz.isAssignableFrom( Class.forName(symbol.fullName) ) ).toOption.getOrElse(false)


  def emptyInfo(reflect: Reflection)(symbol: reflect.Symbol, paramMap: Map[TypeSymbol,RType]): MapLikeInfo =
    val classDef = symbol.tree.asInstanceOf[reflect.ClassDef]
    val keyParamSymName = classDef.constructor.paramss.head(0).toString
    val keyParamType = paramMap.getOrElse(
      keyParamSymName.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(keyParamSymName)
      )
    val valueParamSymName = classDef.constructor.paramss.head(1).toString
    val valueParamType = paramMap.getOrElse(
      valueParamSymName.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(valueParamSymName)
      )
    MapLikeInfo(
      symbol.fullName,
      keyParamType,
      valueParamType
      )


  def extractInfo(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    symbol: reflect.Symbol): RType =

    MapLikeInfo(
      t.classSymbol.get.fullName,
      Reflector.unwindType(reflect)(tob(0).asInstanceOf[reflect.TypeRef]),
      Reflector.unwindType(reflect)(tob(1).asInstanceOf[reflect.TypeRef]))

package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class EitherExtractor() extends TypeInfoExtractor[EitherInfo]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean = symbol.fullName == EitherClazz.getName


  def emptyInfo(reflect: Reflection)(symbol: reflect.Symbol, paramMap: Map[TypeSymbol,RType]): EitherInfo =
    val classDef = symbol.tree.asInstanceOf[reflect.ClassDef]
    val eitherParamSymNames = classDef.constructor.paramss.head.map(_.toString.asInstanceOf[TypeSymbol])
    val eitherParamTypes = eitherParamSymNames.map( et => paramMap.getOrElse(
      et, 
      TypeSymbolInfo(et.toString)
      ))

    EitherInfo(symbol.fullName, eitherParamTypes(0), eitherParamTypes(1))


  def extractInfo(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    symbol: reflect.Symbol): RType =

    val tparms = EitherClazz.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])
    EitherInfo(
      t.classSymbol.get.fullName,
      paramMap(tparms(0)),
      paramMap(tparms(1))
    )

package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection
import scala.util.matching.Regex

case class TupleExtractor() extends TypeInfoExtractor[TupleInfo]:

  val tupleFullName: Regex = """scala.Tuple(\d+)""".r

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean = tupleFullName.matches(symbol.fullName)


  def emptyInfo(reflect: Reflection)(symbol: reflect.Symbol, paramMap: Map[TypeSymbol,RType]): TupleInfo =
    val classDef = symbol.tree.asInstanceOf[reflect.ClassDef]
    val tupleTypeSyms = classDef.constructor.paramss.head.map(_.toString)
    val tupleParamTypes = tupleTypeSyms.map( et => paramMap.getOrElse(
      et.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(et)
      ))
    TupleInfo(
      symbol.fullName, 
      tupleParamTypes.toArray)


  def extractInfo(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    symbol: reflect.Symbol): RType =

    val elementTypes = tob.map( t => Reflector.unwindType(reflect)(t.asInstanceOf[reflect.TypeRef]) )
    TupleInfo(t.classSymbol.get.fullName, elementTypes.toArray)


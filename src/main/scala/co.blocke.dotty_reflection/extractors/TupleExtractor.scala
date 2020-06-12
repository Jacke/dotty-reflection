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


  def extractInfo(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    symbol: reflect.Symbol): RType =

    val tparms = Class.forName(t.classSymbol.get.fullName).getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])
    val elementTypes = tparms.map( p => paramMap(p) ).toArray
    TupleInfo(t.classSymbol.get.fullName, elementTypes)


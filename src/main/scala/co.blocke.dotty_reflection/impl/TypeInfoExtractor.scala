package co.blocke.dotty_reflection
package impl

import info._
import scala.tasty.Reflection

trait TypeInfoExtractor[T <: RType]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean

  def emptyInfo(reflect: Reflection)(symbol: reflect.Symbol, paramMap: Map[TypeSymbol,RType]): T

  def extractInfo(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    symbol: reflect.Symbol): RType

package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class TryExtractor() extends TypeInfoExtractor[TryInfo]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean = symbol.fullName == TryClazz.getName


  def extractInfo(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
    t: reflect.Type, 
    tob: List[reflect.TypeOrBounds], 
    symbol: reflect.Symbol): RType =

      TryInfo(
        t.classSymbol.get.fullName,
        paramMap.values.head
      )

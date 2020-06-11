package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class OptionalExtractor(): // extends TypeInfoExtractor[JavaOptionalInfo]:

  def emptyInfo(clazz: Class[_], paramMap: Map[TypeSymbol,RType]): JavaOptionalInfo = 
    val optionParamSymName = clazz.getTypeParameters.toList.head.getName 
    val optionParamType = paramMap.getOrElse(
      optionParamSymName.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(optionParamSymName)
      )
    JavaOptionalInfo(
      clazz.getName, 
      optionParamType
      )

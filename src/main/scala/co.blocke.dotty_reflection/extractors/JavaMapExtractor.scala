package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class JavaMapExtractor(): // extends TypeInfoExtractor[JavaMapInfo]:

  def emptyInfo(clazz: Class[_], paramMap: Map[TypeSymbol,RType]): JavaMapInfo = 
    val keyParamSymName = clazz.getTypeParameters()(0).getName 
    val keyParamType = paramMap.getOrElse(
      keyParamSymName.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(keyParamSymName)
      )
    val valueParamSymName = clazz.getTypeParameters()(1).getName 
    val valueParamType = paramMap.getOrElse(
      valueParamSymName.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(valueParamSymName)
      )
    JavaMapInfo(
      clazz.getName, 
      clazz.getTypeParameters.map(_.getName.asInstanceOf[TypeSymbol]).toList, 
      keyParamType,
      valueParamType
      )

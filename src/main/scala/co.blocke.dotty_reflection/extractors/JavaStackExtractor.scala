package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class JavaStackExtractor(): // extends TypeInfoExtractor[JavaStackInfo]:

  def emptyInfo(clazz: Class[_], paramMap: Map[TypeSymbol,RType]): JavaStackInfo = 
    val elemParamSymName = clazz.getTypeParameters.toList.head.getName 
    val elemParamType = paramMap.getOrElse(
      elemParamSymName.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(elemParamSymName)
      )
    JavaStackInfo(
      clazz.getName, 
      clazz.getTypeParameters.map(_.getName.asInstanceOf[TypeSymbol]).toList, 
      elemParamType
    )

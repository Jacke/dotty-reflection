package co.blocke.dotty_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class JavaStackExtractor() extends TypeInfoExtractor[JavaStackInfo]:

  def matches(clazz: Class[_]): Boolean = clazz <:< JStackClazz

  def emptyInfo(clazz: Class[_], paramMap: Map[TypeSymbol,RType]): JavaStackInfo = 
    val elemParamSymName = clazz.getTypeParameters.toList.head.getName 
    val elemParamType = paramMap.getOrElse(
      elemParamSymName.asInstanceOf[TypeSymbol], 
      TypeSymbolInfo(elemParamSymName)
      )
    JavaStackInfo(
      clazz.getName, 
      clazz, 
      clazz.getTypeParameters.map(_.getName.asInstanceOf[TypeSymbol]).toList, 
      elemParamType
    )

  def extractInfo(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
      t: reflect.Type, 
      tob: List[reflect.TypeOrBounds], 
      className: String, 
      clazz: Class[_], 
      typeInspector: ScalaClassInspector
    ): RType =

      JavaStackInfo(
          className, 
          clazz,
          clazz.getTypeParameters.map(_.getName.asInstanceOf[TypeSymbol]).toList, 
          typeInspector.inspectType(reflect, paramMap)(tob.head.asInstanceOf[reflect.TypeRef]))

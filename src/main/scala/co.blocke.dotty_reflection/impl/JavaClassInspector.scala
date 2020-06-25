package co.blocke.dotty_reflection
package impl

import info._
import extractors._
import java.lang.reflect.{Type=>JType,_}
import java.lang.annotation.Annotation
import java.beans.{ Introspector, PropertyDescriptor }
import Clazzes._

/** Inspects core Java classes including JavaBeans, basic collections (Map/List/Set/Queue flavors), Optional, and primitive types.
 *  Other "extra" classes, e.g. UUID, woudld be handled by creating & registering a custom TypeInfoExtractor, and would be processed in 
 *  ScalaClassInspector by default.  This is *NOT* run in a macro!  It's executed at runtime, when the java class files are available.
 */
object JavaClassInspector:
  def inspectJavaClass(c: Class[?], paramMap: TypeSymbolMap, returnProxy: Boolean = false): RType =
    // We must detect and handle any top-level Java collections or they'll be "dumbed-down" to JavaClassInfo, which isn't what we want.
    c match {
      case z if z =:= OptionalClazz => OptionalExtractor().emptyInfo(z,paramMap)
      case z if z <:< JStackClazz   => JavaStackExtractor().emptyInfo(z, paramMap)
      case z if z <:< JListClazz    => JavaListExtractor().emptyInfo(z, paramMap)
      case z if z <:< JMapClazz     => JavaMapExtractor().emptyInfo(z, paramMap)
      case z if z <:< JQueueClazz   => JavaQueueExtractor().emptyInfo(z, paramMap)
      case z if z <:< JSetClazz     => JavaSetExtractor().emptyInfo(z, paramMap)
      case _ =>
        // Primitive type?
        PrimitiveType.unapply(c.getName).getOrElse{
          // Nope--not primitive... dive in!
          val annos:List[Annotation] = c.getAnnotations.toList
          val allAnnos = annos.map(a => parseAnno(a)).toMap
          if returnProxy then
            JavaClassInfoProxy(c.getName,  inspectJavaFields(c, paramMap).toArray, typeParamSymbols(c), allAnnos)
          else
            JavaClassInfo(c.getName, paramMap)
        }
    }

  private def parseAnno( annoClass: Annotation): (String,Map[String,String]) = 
    val methods = annoClass.annotationType.getDeclaredMethods.toList.map( m => (m.getName, m.invoke(annoClass).toString)).toMap
    (annoClass.annotationType.getName, methods)

  private def typeParamSymbols(c: Class[_]): List[TypeSymbol] = c.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  private def inspectJavaFields(clazz: Class[?], paramMap: TypeSymbolMap): List[JavaFieldInfo] = 
    Introspector
      .getBeanInfo(clazz)
      .getPropertyDescriptors
      .toList
      .filterNot(_.getName == "class")
      .collect { 
        case desc if desc.getReadMethod() != null && desc.getWriteMethod() != null =>
          val getter = desc.getReadMethod()
          val setter = desc.getWriteMethod()
          val getterAnnos = getter.getAnnotations.map(a => parseAnno(a)).toMap
          val setterAnnos = setter.getAnnotations.map(a => parseAnno(a)).toMap
          val fieldAnnos = getterAnnos ++ setterAnnos
          val fieldName = s"${setter.getName.charAt(3).toLower}${setter.getName.drop(4)}"
          val fieldType = inspectJavaType(clazz.getTypeParameters.toList, getter.getGenericReturnType, paramMap)

          JavaFieldInfo(0,fieldName, fieldType, fieldAnnos, getter, setter, None)
      }.toList.zipWithIndex.map{
        (f,i) => f.copy(index = i)
      }

  private def inspectJavaType(mainTypeParams: List[TypeVariable[_]], fieldType: JType, paramMap: TypeSymbolMap): RType =
    fieldType match {
      case g: GenericArrayType => 
        JavaArrayInfo(inspectJavaType(mainTypeParams, g.getGenericComponentType, paramMap))

      // All this stuff gets triggered if there are Java collections *in a Java class*.  They don't get triggered
      // if we're inspecting a top-level collection, i.e. a Java collection that is a member of a Scala class.
      case p: ParameterizedType if p.getRawType.isInstanceOf[Class[_]] => 
        p.getRawType.asInstanceOf[Class[_]] match {
          case c if c =:= OptionalClazz =>
            val optionType = inspectJavaType(mainTypeParams, p.getActualTypeArguments.head, paramMap) match {
              case t: TypeSymbolInfo if paramMap.contains(t.name.asInstanceOf[TypeSymbol]) => paramMap(t.name.asInstanceOf[TypeSymbol])
              case t => t
            }
            JavaOptionalInfo(c.getName, optionType)
          case c if c <:< JMapClazz =>
            val params = p.getActualTypeArguments.toList
            JavaMapInfo(c.getName, typeParamSymbols(c), inspectJavaType(mainTypeParams, params(0), paramMap), inspectJavaType(mainTypeParams, params(1), paramMap))
          case c if c <:< JStackClazz =>
            JavaStackInfo(c.getName, typeParamSymbols(c), inspectJavaType(mainTypeParams, p.getActualTypeArguments.head, paramMap))
          case c if c <:< JListClazz =>
            JavaListInfo(c.getName, typeParamSymbols(c), inspectJavaType(mainTypeParams, p.getActualTypeArguments.head, paramMap))
          case c if c <:< JQueueClazz =>
            JavaQueueInfo(c.getName, typeParamSymbols(c), inspectJavaType(mainTypeParams, p.getActualTypeArguments.head, paramMap))
          case c if c <:< JSetClazz =>
            JavaSetInfo(c.getName, typeParamSymbols(c), inspectJavaType(mainTypeParams, p.getActualTypeArguments.head, paramMap))
          case c =>
            val params = p.getActualTypeArguments.toList.map(p => Reflector.reflectOnClass(p.asInstanceOf[Class[_]]))
            inspectJavaClass(c, typeParamSymbols(c).zip(params).toMap)
        }
      case v: TypeVariable[_] => 
        paramMap.getOrElse(v.getName.asInstanceOf[TypeSymbol],TypeSymbolInfo(v.getName) )
      case w: WildcardType => throw new ReflectException("Wildcard types not currently supported in reflection library")
      case other if other.isInstanceOf[Class[_]] => 
        other.asInstanceOf[Class[_]] match {
          case c if c.isArray => JavaArrayInfo(inspectJavaType(mainTypeParams, c.getComponentType, paramMap))
          case c if c.isEnum => JavaEnumInfo(c.getName)
          case c => Reflector.reflectOnClass(c)
        }
      case u =>
        throw new ReflectException("Unknown Java type "+u)  // This isn't a Class so we can't use UnknownInfo here
    }
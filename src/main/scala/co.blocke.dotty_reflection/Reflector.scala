package co.blocke.dotty_reflection

import info._ 
import impl._
import scala.jdk.CollectionConverters._
import Clazzes._
import scala.quoted._
import scala.tasty.Reflection

case class TypeStructure( className: String, paramMaps: List[TypeSymbolMap] )

/*  Maybe a cool way to involke a constructor from within a macro???
run {
  val qctx = summon[QuoteContext]
  import qctx.tasty.{_,given}
  val constr = typeOf[Tuple1[Int]].classSymbol.get.tree.asInstanceOf[ClassDef].constructor.symbol
  New(Inferred(typeOf[Tuple1[Int]])).select(constr).appliedToType(typeOf[Int]).appliedTo(Literal(Constant(1))).seal
}
*/

object Reflector:

  //------------------------
  //  <<  MACRO ENTRY >>
  //------------------------
  inline def reflectOn[T]: RType = ${ reflectOnImpl[T]() }

  def reflectOnImpl[T]()(implicit qctx: QuoteContext, ttype: scala.quoted.Type[T]): Expr[RType] = 
    import qctx.tasty.{_, given _}

    Expr( unwindType(qctx.tasty, Map.empty[TypeSymbol, RType])(typeOf[T]) )


  //============================== Support Functions ===========================//

  private def extractParams(reflect: Reflection, _paramMap: TypeSymbolMap)(aType: reflect.Type): (List[RType], TypeSymbolMap, TypeStructure) =
    import reflect.{_, given _}
    aType match {
      case AppliedType(t,tob) => 
        val syms = getTypeParameters(reflect)(t.classSymbol.get)
        val pl: List[RType] = 
          tob.map( tpe => tpe.asInstanceOf[Type].typeSymbol.fullName match {
            case typesymregx(ts) if _paramMap.contains(ts.asInstanceOf[TypeSymbol]) => _paramMap(ts.asInstanceOf[TypeSymbol]) 
            case oneTob => unwindType(reflect, _paramMap)(tpe.asInstanceOf[Type])
          })
        val pMap = syms.zip(pl).toMap
        (pl, pMap, TypeStructure(aType.classSymbol.get.fullName,List(pMap)))
      case tr: TypeRef => 
        val className = tr.classSymbol.get.fullName
        if className == ENUM_CLASSNAME then
          (Nil, Map.empty[TypeSymbol,RType], TypeStructure(tr.qualifier.asInstanceOf[TypeRef].termSymbol.moduleClass.fullName.dropRight(1),Nil))
        else
          (Nil, Map.empty[TypeSymbol,RType], TypeStructure(className,Nil))
      case OrType(left,right) =>
        val( _, tmLeft, tsLeft ) = extractParams(reflect,_paramMap)(left.asInstanceOf[Type])
        val( _, tmRight, tsRight ) = extractParams(reflect,_paramMap)(right.asInstanceOf[Type])
        (Nil, Map.empty[TypeSymbol,RType], TypeStructure(tsLeft.className+"|"+tsRight.className, List(tmLeft,tmRight)))
      case AndType(left,right) =>
        val( _, tmLeft, tsLeft ) = extractParams(reflect,_paramMap)(left.asInstanceOf[Type])
        val( _, tmRight, tsRight ) = extractParams(reflect,_paramMap)(right.asInstanceOf[Type])
        (Nil, Map.empty[TypeSymbol,RType], TypeStructure(tsLeft.className+"&"+tsRight.className, List(tmLeft,tmRight)))
      case _ => 
        (Nil, Map.empty[TypeSymbol,RType], TypeStructure(aType.classSymbol.get.fullName, Nil))
    }


  def unwindType(reflect: Reflection, _paramMap: TypeSymbolMap)(aType: reflect.Type): RType =
    import reflect.{_, given _}

    // If this is the initial entry, may need to "seed" paramMap if this is an AppliedType (parameterized)
    val (paramList, paramMap, structure) = extractParams(reflect, _paramMap)(aType)

    // val structure = TypeStructure(className, List(paramMap)) //discoverStructure(reflect,paramMap)(aType)
    this.synchronized {
      Option(cache.get(structure)).getOrElse{ 
        // Any is a special case... It may just be an "Any", or something else, like a opaque type alias.
        // In either event, we don't want to cache the result.
        if structure.className == "scala.Any" then
          TastyReflection(reflect, paramMap)(aType).reflectOn
        else
          cache.put(structure, SelfRefRType(structure.className, paramList.toArray))
          val reflectedRtype = TastyReflection(reflect, paramMap ++ _paramMap)(aType).reflectOn
          cache.put(structure, reflectedRtype)
          reflectedRtype
      }
    }


  /** Same as reflectOn, except given a Class object instead of a type, T.
   *  NOTE: If Class is parameterized, this call can't infer the types of the parameters.  In that case, call reflectOnClassWithParams
   *  NOTE: This is *NOT* a macro!
   */
  def reflectOnClass(clazz: Class[_], inTermsOf: Option[TraitInfo] = None): RType =
    val className = clazz.getName
    // See if this is a top-level Scala 2 Enumeration... cumbersome, I know...
    val isEnumeration = scala.util.Try(clazz.getMethod("values")).toOption.map( _.getReturnType.getName == "scala.Enumeration$ValueSet").getOrElse(false)
    if isEnumeration then
      val enumVals: Set[_] = clazz.getMethod("values").invoke(clazz).asInstanceOf[Set[_]]
      ScalaEnumerationInfo(className, enumVals.map(_.toString).toList)
    else
      val structure = TypeStructure(className, Nil)
      this.synchronized {
        val cacheHit = if inTermsOf.isEmpty then Option(cache.get(structure)) else None
        cacheHit.getOrElse{
          // Primitive Type?
          className match {
            case PrimitiveType(primType) =>
              cache.put(structure, primType)
              primType
            case _ =>
              cache.put(structure, SelfRefRType(className))
              val tc = new TastyInspection(clazz, inTermsOf)
              tc.inspect("", List(className))
              val found = tc.inspected
              cache.put(structure, found)
              found
          }
        }
      }


  /** Same as reflectOn, except given a Class object instead of a type, T.
   *  NOTE: If Class is parameterized, this call can't infer the types of the parameters.  In that case, call reflectOnClassWithParams
   *  NOTE: This is *NOT* a macro!
   */
  def reflectOnClassWithParams(clazz: Class[_], params: Array[RType]): RType =
    val className = clazz.getName
    val typeSyms = clazz.getTypeParameters.map(_.getName.asInstanceOf[TypeSymbol]).toList
    val paramMap = typeSyms.zip(params).toMap
    val structure = TypeStructure(className, Nil)
    this.synchronized {
      Option(cache.get(structure)).getOrElse{
        cache.put(structure, SelfRefRType(className, params))
        val tc = new TastyInspection(clazz, None, paramMap)
        tc.inspect("", List(className))
        val found = tc.inspected
        cache.put(structure, found)
        found
      }
    }


  // pre-loaded with known language primitive types
  private val cache = new java.util.concurrent.ConcurrentHashMap[TypeStructure, RType]()


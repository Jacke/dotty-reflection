package co.blocke.dotty_reflection

import info._ 
import impl._
import scala.jdk.CollectionConverters._
import Clazzes._
import scala.quoted._
import scala.tasty.Reflection

case class TypeStructure( className: String, params: List[TypeStructure] )

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

  var gz = 0
  def unwindType(reflect: Reflection, _paramMap: TypeSymbolMap)(aType: reflect.Type): RType =
    import reflect.{_, given _}

    println(tabs(gz)+"Class "+aType.classSymbol.get+" with "+_paramMap.map{(k,v)=>(k,v.name)})
    // If this is the initial entry, may need to "seed" paramMap if this is an AppliedType (parameterized)
    val (paramList, paramMap) = aType match {
      case AppliedType(t,tob) if _paramMap.isEmpty =>
        val syms = getTypeParameters(reflect)(t.classSymbol.get)
        gz += 1
        val pl: List[RType] = tob.map( oneTob => unwindType(reflect, _paramMap)(oneTob.asInstanceOf[Type]))
        gz -= 1
        // sew in _paramMap here if non-empty!
        (pl, syms.zip(pl).toMap)
      case _ => (Nil,Map.empty[TypeSymbol,RType])
    }
    println(tabs(gz)+"   Map: "+paramMap.map{(k,v)=>(k,v.name)})

    // PROBLEM:  Drawer is somehow getting recreated with Any as the parameter--loses its earlier (correct)
    // parameter of Shape.  This seems to have something to do with populationg Option[Drawer[Shape]] 
    // (becomes Option[Drawer[Any]])
    //
    // Solution: For embedded AppliedTypes, we need to "sew" the type symbols from _paramMap into the discovered
    // (embedded) map.  For example Option[Drawer[T]] _paramMap defines T as Shape.  We need to sew A in Option into T so it resolves!

    val structure = discoverStructure(reflect,paramMap)(aType)
    println(tabs(gz)+"   >> S: "+structure)
    this.synchronized {
      Option(cache.get(structure)).getOrElse{ 
        // Any is a special case... It may just be an "Any", or something else, like a opaque type alias.
        // In either event, we don't want to cache the result.
        if structure.className == "scala.Any" then
          TastyReflection(reflect, paramMap)(aType).reflectOn
        else
          cache.put(structure, SelfRefRType(structure.className, paramList.toArray))
          val reflectedRtype = TastyReflection(reflect, paramMap)(aType).reflectOn
          cache.put(structure, reflectedRtype)
          reflectedRtype
      }
    }

    
  def discoverStructure(reflect: Reflection, paramMap: TypeSymbolMap)(aType: reflect.Type): TypeStructure =
    import reflect.{_, given _}

    aType match {
      case AppliedType(t,tob) =>
        val className = t.asInstanceOf[TypeRef].classSymbol.get.fullName
        val res = tob.map(_.asInstanceOf[TypeRef].classSymbol.get.fullName)
        val params = tob.map{ tpe => tpe.asInstanceOf[reflect.Type].typeSymbol.fullName match {
          case typesymregx(ts) if paramMap.contains(ts.asInstanceOf[TypeSymbol]) => paramMap(ts.asInstanceOf[TypeSymbol]).toTypeStructure
          case _ => discoverStructure(reflect,paramMap)(tpe.asInstanceOf[Type]) 
        }}
        TypeStructure(className, params)
      case tr: TypeRef => 
        val className = tr.classSymbol.get.fullName
        if className == ENUM_CLASSNAME then
          TypeStructure(tr.qualifier.asInstanceOf[TypeRef].termSymbol.moduleClass.fullName.dropRight(1), Nil)
        else
          TypeStructure(className, Nil)
      case OrType(left,right) =>
        val resolvedLeft = discoverStructure(reflect,paramMap)(left.asInstanceOf[Type])
        val resolvedRight = discoverStructure(reflect,paramMap)(right.asInstanceOf[Type])
        TypeStructure(UNION_CLASS, List(resolvedLeft, resolvedRight))
      case AndType(left,right) =>
        val resolvedLeft = discoverStructure(reflect,paramMap)(left.asInstanceOf[Type])
        val resolvedRight = discoverStructure(reflect,paramMap)(right.asInstanceOf[Type])
        TypeStructure(INTERSECTION_CLASS, List(resolvedLeft, resolvedRight))
      case z @ TermRef(tob, name) =>
        TypeStructure(name, Nil)
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
      val structure = TypeStructure(className,Nil)
      this.synchronized {
        val cacheHit = if inTermsOf.isEmpty then Option(cache.get(structure)) else None
        cacheHit.getOrElse{
          cache.put(structure, SelfRefRType(className))
          val tc = new TastyInspection(clazz, inTermsOf)
          tc.inspect("", List(className))
          val found = tc.inspected
          cache.put(structure, found)
          found
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
    val structure = TypeStructure(className,Nil)
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
  private val cache = new java.util.concurrent.ConcurrentHashMap[TypeStructure, RType](Map(
    TypeStructure("boolean",Nil)              -> PrimitiveType.Scala_Boolean,
    TypeStructure("Boolean",Nil)              -> PrimitiveType.Scala_Boolean,
    TypeStructure("scala.Boolean",Nil)        -> PrimitiveType.Scala_Boolean,
    TypeStructure("java.lang.Boolean",Nil)    -> PrimitiveType.Java_Boolean,
    TypeStructure("byte",Nil)                 -> PrimitiveType.Scala_Byte,
    TypeStructure("Byte",Nil)                 -> PrimitiveType.Scala_Byte,
    TypeStructure("scala.Byte",Nil)           -> PrimitiveType.Scala_Byte,
    TypeStructure("java.lang.Byte",Nil)       -> PrimitiveType.Java_Byte,
    TypeStructure("char",Nil)                 -> PrimitiveType.Scala_Char,
    TypeStructure("Char",Nil)                 -> PrimitiveType.Scala_Char,
    TypeStructure("scala.Char",Nil)           -> PrimitiveType.Scala_Char,
    TypeStructure("java.lang.Character",Nil)  -> PrimitiveType.Java_Char,
    TypeStructure("double",Nil)               -> PrimitiveType.Scala_Double,
    TypeStructure("Double",Nil)               -> PrimitiveType.Scala_Double,
    TypeStructure("scala.Double",Nil)         -> PrimitiveType.Scala_Double,
    TypeStructure("java.lang.Double",Nil)     -> PrimitiveType.Java_Double,
    TypeStructure("float",Nil)                -> PrimitiveType.Scala_Float,
    TypeStructure("Float",Nil)                -> PrimitiveType.Scala_Float,
    TypeStructure("scala.Float",Nil)          -> PrimitiveType.Scala_Float,
    TypeStructure("java.lang.Float",Nil)      -> PrimitiveType.Java_Float,
    TypeStructure("int",Nil)                  -> PrimitiveType.Scala_Int,
    TypeStructure("Int",Nil)                  -> PrimitiveType.Scala_Int,
    TypeStructure("scala.Int",Nil)            -> PrimitiveType.Scala_Int,
    TypeStructure("java.lang.Integer",Nil)    -> PrimitiveType.Java_Int,
    TypeStructure("long",Nil)                 -> PrimitiveType.Scala_Long,
    TypeStructure("Long",Nil)                 -> PrimitiveType.Scala_Long,
    TypeStructure("scala.Long",Nil)           -> PrimitiveType.Scala_Long,
    TypeStructure("java.lang.Long",Nil)       -> PrimitiveType.Java_Long,
    TypeStructure("short",Nil)                -> PrimitiveType.Scala_Short,
    TypeStructure("Short",Nil)                -> PrimitiveType.Scala_Short,
    TypeStructure("scala.Short",Nil)          -> PrimitiveType.Scala_Short,
    TypeStructure("java.lang.Short",Nil)      -> PrimitiveType.Java_Short,
    TypeStructure("java.lang.String",Nil)     -> PrimitiveType.Scala_String,
    TypeStructure("java.lang.Object",Nil)     -> PrimitiveType.Java_Object,
    TypeStructure("java.lang.Number",Nil)     -> PrimitiveType.Java_Number
  ).asJava)

  // parameterized class cache
  private val paramerterizedClassCache = new java.util.concurrent.ConcurrentHashMap[(Class[_],List[RType]), RType]

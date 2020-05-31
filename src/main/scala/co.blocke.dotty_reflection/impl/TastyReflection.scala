package co.blocke.dotty_reflection
package impl

import info._
// import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
  

case class TastyReflection(reflect: Reflection)(aType: reflect.Type):
  // extends ScalaClassReflectorLike:
  // with ParamGraph:
  import reflect.{_, given _}

  // def reflectOn(qctx: QuoteContext)(aType: qctx.tasty.Type): RType =
  //   qctx.tasty.rootContext match {
  //     case ctx if ctx.isJavaCompilationUnit() => JavaClassInspector.inspectClass(clazz, initialParamMap)      
  //     case ctx if ctx.isScala2CompilationUnit() => UnknownInfo(clazz)  // Can't do much with Scala2 classes--not Tasty
  //     case ctx if ctx.isAlreadyLoadedCompilationUnit() => 
  //       ExtractorRegistry.extractors.collectFirst {
  //         case e if e.matches(clazz) => inspected = e.emptyInfo(clazz, initialParamMap)
  //       }
  //     case _ => inspectClass(clazz.getName, reflect, initialParamMap)(root)
  //   }
  //   BogusInfo()

  val className =
    aType match {
      case AppliedType(t,tob) => t.asInstanceOf[TypeRef].classSymbol.get.fullName
      case tr: TypeRef => tr.classSymbol.get.fullName
    }

    /*
    All the good stuff is in SymbolOps: companion object, case fields, etc.

    Steps:
    1) reflect on the type (class, case class, collection, primitive, ...)
    2) reflect on class, extract goodies, then...
    3) reflect on each field, in turn calling reflect on type
    */


  def reflectOn: RType = 
    // println(aType)
    // println("Base: "+aType.baseClasses.map(c => "("+c.fullName+","+c.companionClass+")"))
    reflectOnType(reflect, Map.empty[TypeSymbol,RType])(aType.asInstanceOf[TypeRef])


  private def reflectOnType(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(typeRef: reflect.TypeRef): RType = 
    import reflect.{_, given _}

      typeRef.classSymbol match {

        // Intersection types don't have a class symbol, so don't assume one!
        case None =>
          typeRef match {
            // Intersection Type
            //----------------------------------------
            case AndType(left,right) =>
              val resolvedLeft: RType = reflectOnType(reflect, paramMap)(left.asInstanceOf[reflect.TypeRef])
              val resolvedRight: RType = reflectOnType(reflect, paramMap)(right.asInstanceOf[reflect.TypeRef])
              IntersectionInfo(INTERSECTION_CLASS, resolvedLeft, resolvedRight)
            case u => throw new ReflectException("Unsupported TypeRef: "+typeRef)
          }

        case Some(classSymbol) =>
          // Handle gobbled non-class scala.Enumeration.Value (old 2.x Enumeration class values)
          val (is2xEnumeration, className) = classSymbol.fullName match { 
            case raw if raw == ENUM_CLASSNAME => 
              val enumerationClass = typeRef.typeSymbol.fullName
              if( enumerationClass == ENUM_CLASSNAME ) then
                // If caller did NOT defined a type member (type X = Value) inside their Enumeration class
                val enumClassName = typeRef.qualifier.asInstanceOf[reflect.TermRef].termSymbol.moduleClass.fullName.dropRight(1) // chop the '$' off the end!
                (true, enumClassName)
              else
                // If caller defined a type member (type X = Value) inside their Enumeration class
                (true, enumerationClass.dropRight(enumerationClass.length - enumerationClass.lastIndexOf('$')))
            case _  => (false, classSymbol.fullName)
          }

          typeRef match {
            // Scala3 opaque type alias
            //----------------------------------------
            case named: dotty.tools.dotc.core.Types.NamedType if typeRef.isOpaqueAlias =>
              reflectOnType(reflect, paramMap)(typeRef.translucentSuperType.asInstanceOf[reflect.TypeRef]) match {
                case t: TypeSymbolInfo => throw new ReflectException("Opaque aliases for type symbols currently unsupported")
                case t => AliasInfo(typeRef.show, t)
              }

            // Scala3 Tasty-equipped type incl. primitive types
            // Traits and classes w/type parameters are *not* here... they're AppliedTypes
            //----------------------------------------
            case named: dotty.tools.dotc.core.Types.NamedType => 
              val isTypeParam = typeRef.typeSymbol.flags.is(Flags.Param)   // Is 'T' or a "real" type?  (true if T)
              val anySymbol = Symbol.classSymbol("scala.Any")
              classSymbol match {
                case cs if isTypeParam     => 
                  // See if we can resolve the type symbol
                  paramMap.get(typeRef.name.asInstanceOf[TypeSymbol]).getOrElse(
                    TypeSymbolInfo(typeRef.name)  // TypeSymbols Foo[T] have typeRef of Any
                    )
                case cs if cs == anySymbol => PrimitiveType.Scala_Any
                case cs => reflectOnClass(reflect, paramMap)(typeRef)
                // case cs =>
                //   Class.forName(className) match {
                //     case c if c <:< EnumClazz => ScalaEnumInfo(className, c)
                //     case c if is2xEnumeration => ScalaEnumerationInfo(className, c)
                //     case c                    => Reflector.reflectOnClass(c)  // it's some other class, likely a Java or 2.x Scala class
                //   }
              }

            // Union Type
            //----------------------------------------
            case OrType(left,right) =>
              val resolvedLeft = reflectOnType(reflect, paramMap)(left.asInstanceOf[reflect.TypeRef])
              val resolvedRight = reflectOnType(reflect, paramMap)(right.asInstanceOf[reflect.TypeRef])
              UnionInfo(UNION_CLASS, resolvedLeft, resolvedRight)
          
            // Most other "normal" Types
            //----------------------------------------
            /*
            case AppliedType(t,tob) => 
              val clazz = Class.forName(className)

              val foundType: Option[RType] = ExtractorRegistry.extractors.collectFirst {
                case e if e.matches(clazz) => e.extractInfo(reflect, paramMap)(t, tob, className, clazz, this)   
              }
              foundType.getOrElse{
                // Some other class we need to descend into, including a parameterized Scala class
                Reflector.reflectOnClassWithParams(clazz, tob.map(typeP => 
                  reflectOnType(reflect, paramMap)(typeP.asInstanceOf[reflect.TypeRef])
                ))
              }
              */
          
            case x => 
              println("oops... "+x)
              UnknownInfo(className)
          }
      }


  private def reflectOnClass(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(typeRef: reflect.TypeRef): RType = 
    import reflect.{_, given _}

    val symbol = typeRef.classSymbol.get
  
    // Get any type parameters
    // println(s"<Sig ${symbol.fullName}> "+symbol.paramSymss)
    // val typeParams = clazz.getTypeParameters.map(_.getName.asInstanceOf[TypeSymbol]).toList

    if(symbol.flags.is(reflect.Flags.Trait)) then
      println("Trait! "+symbol.fullName)
      // === Trait ===
      // if typeRef.classSymbol.flags.is(reflect.Flags.Sealed) then
      //   SealedTraitInfo(
      //     className, 
      //     typeParams, 
      //     t.symbol.children.map(c => Reflector.reflectOnClass(Class.forName(c.fullName))))
      // else
      // val actualTypeParams = typeParams.map(_ match {
      //   case p if paramMap.contains(p) => paramMap(p)
      //   case p => TypeSymbolInfo(p.asInstanceOf[String])
      // })
      // val traitInfo = TraitInfo(className, typeParams, actualTypeParams)

      // Now figure out type parameter graph
      // registerParents(reflect)(t, traitInfo)

      // traitInfo
      UnknownInfo("a trait")
    else
      if symbol.flags.is(reflect.Flags.Case) then
        // case classes
        val caseFields = symbol.caseFields.zipWithIndex.map(f => reflectOnField(reflect, paramMap)(f))
        println("Fields: "+caseFields)

        UnknownInfo("case class boop")
      else
        UnknownInfo("boop")


  private def reflectOnField(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(sym: (reflect.Symbol, Int)): FieldInfo = 
    sym._1.tree match {
      case valDef: reflect.ValDef => 
        val fieldAnnos = {
          val baseAnnos = 
            if valDef.symbol.flags.is(reflect.Flags.Override) then
              // TODO: Get Base annos for inheritance!
              // dad.asInstanceOf[ScalaClassInfo].fields.find(_.name == valDef.name).map(_.annotations).get
              Map.empty[String,Map[String,String]]
            else
              Map.empty[String,Map[String,String]]
          baseAnnos ++ valDef.symbol.annots.map{ a => 
            val reflect.Apply(_, params) = a
            val annoName = a.symbol.signature.resultSig
            (annoName,(params collect {
              case NamedArg(argName, Literal(Constant(argValue))) => (argName.toString, argValue.toString)
            }).toMap)
          }.toMap
        }

        // val fieldType: RType = reflectOnType(reflect, paramMap)(valDef.tpt.tpe.asInstanceOf[reflect.TypeRef])
        //def reflectOnImpl[T]()(implicit qctx: QuoteContext, ttype:scala.quoted.Type[T])
        val fieldType = Reflector.unwindType(reflect)(valDef.tpt.tpe)

        ScalaFieldInfo(sym._2, valDef.name, fieldType, fieldAnnos, null, None, None)
    }

    /*
    case class ScalaFieldInfo(
    index:                Int,
    name:                 String,
    fieldType:            RType,
    annotations:          Map[String,Map[String,String]],
    valueAccessor:        Method,
    defaultValueAccessor: Option[()=>Object],
    originalSymbol:       Option[TypeSymbol]
  )
  */
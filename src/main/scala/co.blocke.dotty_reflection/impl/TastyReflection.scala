package co.blocke.dotty_reflection
package impl

import info._
import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
import scala.util.Try


case class TastyReflection(reflect: Reflection, paramMap: TypeSymbolMap)(aType: reflect.Type) extends NonCaseClassInspector:
  import reflect.{_, given _}

  val className =
    aType match {
      case AppliedType(t,tob) => t.asInstanceOf[TypeRef].classSymbol.get.fullName
      case tr: TypeRef => tr.classSymbol.get.fullName
      case _ => ""
    }


  def reflectOn: RType = 
    reflectOnType(reflect, paramMap)(aType.asInstanceOf[TypeRef])

  
  protected def reflectOnType(reflect: Reflection, paramMap: TypeSymbolMap)(typeRef: reflect.TypeRef): RType = 
    import reflect.{_, given _}

    typeRef.classSymbol match {

      // Intersection types don't have a class symbol, so don't assume one!
      case None =>
        typeRef match {
          // Intersection Type
          //----------------------------------------
          case AndType(left,right) =>
            val resolvedLeft: RType = Reflector.unwindType(reflect, paramMap)(left.asInstanceOf[reflect.TypeRef])
            val resolvedRight: RType = Reflector.unwindType(reflect, paramMap)(right.asInstanceOf[reflect.TypeRef])
            IntersectionInfo(INTERSECTION_CLASS, resolvedLeft, resolvedRight)

          // Union Type
          //----------------------------------------
          case OrType(left,right) =>
            val resolvedLeft: RType = Reflector.unwindType(reflect, paramMap)(left.asInstanceOf[reflect.TypeRef])
            val resolvedRight: RType = Reflector.unwindType(reflect, paramMap)(right.asInstanceOf[reflect.TypeRef])
            UnionInfo(UNION_CLASS, resolvedLeft, resolvedRight)

          case u => 
            throw new ReflectException("Unsupported TypeRef: "+typeRef)
        }

      case Some(classSymbol) =>
        // Handle gobbled non-class scala.Enumeration.Value (old 2.x Enumeration class values)
        val (is2xEnumeration, className) = classSymbol.fullName match { 
          case raw if raw == ENUM_CLASSNAME => 
            val enumerationClass = typeRef.typeSymbol.fullName
            if( enumerationClass == ENUM_CLASSNAME ) then
              // If caller did NOT define a type member (type X = Value) inside their Enumeration class
              val enumClassName = typeRef.qualifier.asInstanceOf[reflect.TermRef].termSymbol.moduleClass.fullName.dropRight(1) // chop the '$' off the end!
              (true, enumClassName)
            else
              // If caller defined a type member (type X = Value) inside their Enumeration class
              (true, enumerationClass.dropRight(enumerationClass.length - enumerationClass.lastIndexOf('$')))
          case _  => (false, classSymbol.fullName)
        }

        typeRef match {
          case named: dotty.tools.dotc.core.Types.NamedType if classSymbol == Symbol.classSymbol("scala.Any") =>
            // Scala3 opaque type alias
            //----------------------------------------
            if typeRef.isOpaqueAlias then
              val translucentSuperType = typeRef.translucentSuperType
              AliasInfo(typeRef.show, Reflector.unwindType(reflect, paramMap)(translucentSuperType))

            // Any Type
            //----------------------------------------
            else
              PrimitiveType.Scala_Any

          // Scala3 Tasty-equipped type incl. primitive types
          // Traits and classes w/type parameters are *not* here... they're AppliedTypes
          //----------------------------------------
          case named: dotty.tools.dotc.core.Types.NamedType => 
            val isTypeParam = typeRef.typeSymbol.flags.is(Flags.Param)   // Is 'T' or a "real" type?  (true if T)
            classSymbol match {
              case cs if isTypeParam     => 
                // See if we can resolve the type symbol
                paramMap.get(typeRef.name.asInstanceOf[TypeSymbol]).getOrElse(
                  TypeSymbolInfo(typeRef.name)  // TypeSymbols Foo[T] have typeRef of Any
                  )
              case cs if is2xEnumeration => 
                val enumerationClassSymbol = typeRef.qualifier.asInstanceOf[reflect.TermRef].termSymbol.moduleClass
                ScalaEnumerationInfo(typeRef.name, enumerationClassSymbol.fields.map( _.name ))  // get the values of the Enumeration
              case cs => 
                reflectOnClass(reflect, paramMap)(typeRef)
            }

          // Union Type
          //----------------------------------------
          case OrType(left,right) =>
            val resolvedLeft = Reflector.unwindType(reflect, paramMap)(left.asInstanceOf[reflect.TypeRef])
            val resolvedRight = Reflector.unwindType(reflect, paramMap)(right.asInstanceOf[reflect.TypeRef])
            UnionInfo(UNION_CLASS, resolvedLeft, resolvedRight)
        
          // Most other "normal" Types
          //----------------------------------------
          case a @ AppliedType(t,tob) => 
            val actualArgRTypes = 
              tob.map{ tpe => tpe.asInstanceOf[reflect.Type].typeSymbol.fullName match {
                case typesymregx(ts) if paramMap.contains(ts.asInstanceOf[TypeSymbol]) => paramMap(ts.asInstanceOf[TypeSymbol])
                case _ => Reflector.unwindType(reflect, paramMap)(tpe.asInstanceOf[reflect.Type]) 
              }}

            // Sigh--Array is "different".  Getting type params crashes, likely because its a wrap of a Java Array?
            val typeMap = t match {
              case _ if classSymbol.fullName == Clazzes.ScalaArrayClazz.getName => Map("A".asInstanceOf[TypeSymbol] -> actualArgRTypes(0))
              case _ => getTypeParameters(reflect)(t.classSymbol.get).zip(actualArgRTypes).toMap
            }

            val foundType: Option[RType] = ExtractorRegistry.extractors.collectFirst {
              case e if e.matches(reflect)(classSymbol) => e.extractInfo(reflect, typeMap)(t, tob, classSymbol)   
            }

            foundType.getOrElse{
              // === Some other class we need to descend into, including a parameterized Scala class
              reflectOnClass(reflect, typeMap)(t.asInstanceOf[TypeRef])
            }
        
          case x => 
            // === No idea!  Unkonwn entity...
            UnknownInfo(className)
        }
    }


  private def reflectOnClass(reflect: Reflection, paramMap: TypeSymbolMap)(typeRef: reflect.TypeRef): RType = 
    import reflect.{_, given _}

    object DefaultMethod {
      val reg = """\$lessinit\$greater\$default\$(\d+)""".r
      def unapply(s: reflect.Symbol): Option[Int] = reg.findFirstIn(s.toString) match {
        case Some(reg(a)) => Some(a.toInt)
        case _ => None
      }
    }

    val symbol = typeRef.classSymbol.get
 
    if symbol.flags.is(reflect.Flags.Scala2X) then
      Scala2Info(symbol.fullName)

    else if symbol.flags.is(reflect.Flags.Trait) then
      // === Trait ===
      val typeSymbols = getTypeParameters(reflect)(symbol)
      //     >> Sealed Traits
      if symbol.flags.is(reflect.Flags.Sealed) then
        val kidsRTypes = symbol.children.map{ c => 
          c.tree match {
            case b: Bind => ObjectInfo(b.pattern.symbol.fullName)  // sealed object implementation
            case _ =>   // sealed case class implementation
              val typeDef: dotty.tools.dotc.ast.Trees.TypeDef[_] = c.tree.asInstanceOf[dotty.tools.dotc.ast.Trees.TypeDef[_]]
              Reflector.unwindType(reflect, paramMap)(typeDef.typeOpt.asInstanceOf[reflect.Type])
          }
        }
        SealedTraitInfo(
          className, 
          typeSymbols, 
          kidsRTypes.toArray)
      else
        //  >> Normal (unsealed) traits
        TraitInfo(
          className, 
          typeSymbols, 
          if paramMap.isEmpty then new Array[RType](0) else typeSymbols.map(paramMap(_)).toArray)

    else if symbol.flags.is(reflect.Flags.Enum) then // Found top-level enum (i.e. not part of a class), e.g. member of a collection
      val enumClassSymbol = typeRef.classSymbol.get
      enumClassSymbol.companionClass.methods // <-- This shouldn't "do" anything!  For some reason it is needed or Enums test explodes.
      val enumValues = enumClassSymbol.children.map(_.name)
      ScalaEnumInfo(symbol.name, enumValues)

    // === Java Class ===
    // User-written Java classes will have the source file.  Java library files will have <no file> for source
    else if symbol.pos.sourceFile.toString.endsWith(".java") || symbol.pos.sourceFile.toString == "<no file>" then
      // Reflecting Java classes requires the materialized Class, which may be available (e.g. Java collections) or not (e.g. user-written class).
      // See if we can get it...  If not create a proxy.
      scala.util.Try {
        JavaClassInspector.inspectJavaClass(Class.forName(symbol.fullName), paramMap)
      }.toOption.getOrElse(JavaClassInfo(symbol.fullName, paramMap))

    // === Scala Classes ===
    else if symbol.isClassDef then
      // Get field annotatations (from body of class--they're not on the constructor fields)
      val classDef = symbol.tree.asInstanceOf[ClassDef]
      val typeSymbols = getTypeParameters(reflect)(symbol)

      // Class annotations -> annotation map
      val annoSymbol = symbol.annots.filter( a => !a.symbol.signature.resultSig.startsWith("scala.annotation.internal."))
      val classAnnos = annoSymbol.map{ a => 
        val reflect.Apply(_, params) = a
        val annoName = a.symbol.signature.resultSig
        (annoName,(params collect {
          case NamedArg(argName, Literal(Constant(argValue))) => (argName.toString, argValue.toString)
        }).toMap)
      }.toMap

      val isValueClass = classDef.parents.collectFirst {
        case t:TypeTree if t.tpe.typeSymbol.name == "AnyVal" => t
      }.isDefined

      // Get superclass' field annotations--if any
      val dad = classDef.parents.headOption match {
        case Some(tt: TypeTree) => 
          reflectOnClass(reflect,paramMap)(tt.tpe.asInstanceOf[TypeRef]) match {
            case ci: ClassInfo => Some(ci) // Any kind of class
            case _ => None // e.g. Unknown
          }
        case _ => None
      }

      val constructorParamz = classDef.constructor.paramss
      // Get any case field default value accessor method names (map by field index)
      val fieldDefaultMethods = symbol.companionClass match {
        case dotty.tools.dotc.core.Symbols.NoSymbol => Map.empty[Int, (String,String)]
        case s: Symbol => symbol.companionClass.methods.collect {
          case DefaultMethod(defaultIndex) => defaultIndex-1 -> (className+"$", ("$lessinit$greater$default$"+defaultIndex))
        }.toMap
      }

      // All this mucking around in the constructor.... why not just get the case fields from the symbol?
      // Because:  symbol's case fields lose the annotations!  Pulling from contstructor ensures they are retained.
      val caseFields = constructorParamz.head.zipWithIndex.map( p => reflectOnField(reflect, paramMap)(p._1, p._2, dad, fieldDefaultMethods) )

      val orderedTypeParameters = getTypeParameters(reflect)(symbol)

      // Find any type members matching a class type parameter
      val typeMembers = classDef.body.collect {
        case TypeDef(typeName, typeTree) if paramMap.contains(typeTree.asInstanceOf[TypeTree].tpe.asInstanceOf[TypeBounds].low.typeSymbol.name.asInstanceOf[TypeSymbol]) =>
          val typeSym = typeTree.asInstanceOf[TypeTree].tpe.asInstanceOf[TypeBounds].low.typeSymbol.name.asInstanceOf[TypeSymbol]
          TypeMemberInfo(
            typeName,
            typeSym,
            paramMap.getOrElse(typeSym, TypeSymbolInfo(typeSym.toString)
            )
          )
      }

      if symbol.flags.is(reflect.Flags.Case) then
        // === Case Classes ===
        ScalaCaseClassInfo(
          className, 
          orderedTypeParameters, 
          if paramMap.isEmpty then new Array[RType](0) else typeSymbols.map(paramMap(_)).toArray,
          typeMembers.toArray, 
          caseFields.toArray, 
          classAnnos, 
          classDef.parents.map(_.symbol.fullName), 
          isValueClass)
      else
        // === Non-Case Classes ===
        
        // ensure all constructur fields are vals
        if symbol.fields.filter( _.flags.is(Flags.ParamAccessor)).map(_.flags.is(Flags.PrivateLocal)).foldLeft(false)(_|_) then
          throw new ReflectException(s"Class [${symbol.fullName}]: Non-case class constructor arguments must all be 'val'")

        inspectNonCaseClass(reflect, paramMap)(
          symbol, 
          classDef, 
          dad,
          className, 
          fieldDefaultMethods,
          orderedTypeParameters,
          if paramMap.isEmpty then new Array[RType](0) else typeSymbols.map(paramMap(_)).toArray,
          typeMembers.toArray,
          caseFields.toArray, 
          classAnnos,
          classDef.parents.map(_.symbol.fullName),
          isValueClass)

    // === Other kinds of classes (non-case Scala) ===
    else
      UnknownInfo(symbol.fullName)


  private def reflectOnField(reflect: Reflection, paramMap: TypeSymbolMap)(
    valDef: reflect.ValDef, 
    index: Int, 
    dad: Option[ClassInfo],
    fieldDefaultMethods: Map[Int, (String,String)]
  ): FieldInfo = 
    val fieldAnnos = {
      val baseAnnos = dad.flatMap( _.fields.find(_.name == valDef.name) ).map(_.annotations).getOrElse(Map.empty[String,Map[String,String]])
      baseAnnos ++ valDef.symbol.annots.map{ a => 
        val reflect.Apply(_, params) = a
        val annoName = a.symbol.signature.resultSig
        (annoName,(params collect {
          case NamedArg(argName, Literal(Constant(argValue))) => (argName.toString, argValue.toString)
        }).toMap)
      }.toMap
    }

    // Figure out the original type symbols, i.e. T, (if any)
    val valTypeRef = valDef.tpt.tpe.asInstanceOf[reflect.TypeRef]
    val isTypeParam = valTypeRef.typeSymbol.flags.is(reflect.Flags.Param)
    val originalTypeSymbol = if isTypeParam then Some(valTypeRef.name.asInstanceOf[TypeSymbol]) else None
    val fieldType = originalTypeSymbol.map( ots => 
      paramMap.getOrElse(ots, TypeSymbolInfo(ots.toString))
    ).getOrElse( Reflector.unwindType(reflect, paramMap)(valDef.tpt.tpe) )

    ScalaFieldInfo(index, valDef.name, fieldType, fieldAnnos, fieldDefaultMethods.get(index), originalTypeSymbol)

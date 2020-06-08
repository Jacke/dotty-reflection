package co.blocke.dotty_reflection
package impl

import info._
import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
import scala.util.Try


case class TastyReflection(reflect: Reflection)(aType: reflect.Type):
  import reflect.{_, given _}

  val className =
    aType match {
      case AppliedType(t,tob) => t.asInstanceOf[TypeRef].classSymbol.get.fullName
      case tr: TypeRef => tr.classSymbol.get.fullName
      case _ => ""
    }


  def reflectOn: RType = 
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
              val resolvedLeft: RType = Reflector.unwindType(reflect)(left.asInstanceOf[reflect.TypeRef])
              val resolvedRight: RType = Reflector.unwindType(reflect)(right.asInstanceOf[reflect.TypeRef])
              IntersectionInfo(INTERSECTION_CLASS, resolvedLeft, resolvedRight)

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
                AliasInfo(typeRef.show, Reflector.unwindType(reflect)(translucentSuperType))

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
              val resolvedLeft = Reflector.unwindType(reflect)(left.asInstanceOf[reflect.TypeRef])
              val resolvedRight = Reflector.unwindType(reflect)(right.asInstanceOf[reflect.TypeRef])
              UnionInfo(UNION_CLASS, resolvedLeft, resolvedRight)
          
            // Most other "normal" Types
            //----------------------------------------
            case a @ AppliedType(t,tob) => 
              val foundType: Option[RType] = ExtractorRegistry.extractors.collectFirst {
                case e if e.matches(reflect)(classSymbol) => e.extractInfo(reflect, paramMap)(t, tob, classSymbol)   
              }
              foundType.getOrElse{
                // Some other class we need to descend into, including a parameterized Scala class
                val actualArgRTypes = a.args.map( arg => Reflector.unwindType(reflect)(arg.asInstanceOf[Type]) )
                val typeSymbols = a.tycon.classSymbol.get.primaryConstructor.paramSymss.head.map(_.name.toString.asInstanceOf[TypeSymbol])
                reflectOnClass(reflect, typeSymbols.zip(actualArgRTypes).toMap)(t.asInstanceOf[TypeRef])
              }
          
            case x => 
              println("oops... "+x)
              UnknownInfo(className)
          }
      }


  private def reflectOnClass(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(typeRef: reflect.TypeRef): RType = 
    import reflect.{_, given _}

    object DefaultMethod {
      val reg = """\$lessinit\$greater\$default\$(\d+)""".r
      def unapply(s: reflect.Symbol): Option[Int] = reg.findFirstIn(s.toString) match {
        case Some(reg(a)) => Some(a.toInt)
        case _ => None
      }
    }

    val symbol = typeRef.classSymbol.get
 
    if(symbol.flags.is(reflect.Flags.Trait)) then
      // === Trait ===
      val typeSymbols = symbol.primaryConstructor.paramSymss.head.map(_.name.toString.asInstanceOf[TypeSymbol])
      //     >> Sealed Traits
      if symbol.flags.is(reflect.Flags.Sealed) then
        val kidsRTypes = symbol.children.map{ c => 
          c.tree match {
            case b: Bind => ObjectInfo(b.pattern.symbol.fullName)  // sealed object implementation
            case _ =>   // sealed case class implementation
              val typeDef: dotty.tools.dotc.ast.Trees.TypeDef[_] = c.tree.asInstanceOf[dotty.tools.dotc.ast.Trees.TypeDef[_]]
              Reflector.unwindType(reflect)(typeDef.typeOpt.asInstanceOf[reflect.Type])
          }
        }
        SealedTraitInfo(
          className, 
          typeSymbols, 
          kidsRTypes.toArray)
      else
        //  >> Normal (unsealed) traits
        TraitInfo(className, typeSymbols, typeSymbols.map(paramMap(_)).toArray) 

    else if symbol.flags.is(reflect.Flags.Enum) then // Found top-level enum (i.e. not part of a class), e.g. member of a collection
      val enumClassSymbol = typeRef.classSymbol.get
      enumClassSymbol.companionClass.methods // <-- This shouldn't "do" anything!  For some reason it is needed or Enums test explodes.
      val enumValues = enumClassSymbol.children.map(_.name)
      ScalaEnumInfo(symbol.name, enumValues)

    // === Case Classes ===
    else if symbol.flags.is(reflect.Flags.Case) then
      // Get field annotatations (from body of class--they're not on the constructor fields)
      val classDef = symbol.tree.asInstanceOf[ClassDef]

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
      val dad = classDef.parents.head match {
        case tt: TypeTree => 
          reflectOnClass(reflect,paramMap)(tt.tpe.asInstanceOf[TypeRef]) match {
            case ci: ClassInfo => Some(ci) // Any kind of class
            case _ => None // e.g. Unknown
          }
        case _ => None
      }

      // Get any case field default value accessor method names (map by field index)
      val fieldDefaultMethods = symbol.companionClass.methods.collect {
        case DefaultMethod(defaultIndex) => defaultIndex-1 -> (className+"$", ("$lessinit$greater$default$"+defaultIndex))
      }.toMap

      // All this mucking around in the constructor.... why not just get the case fields from the symbol?
      // Because:  symbol's case fields lose the annotations!  Pulling from contstructor ensures they are retained.
      val constructorParamz = classDef.constructor.paramss
      val classMembers = classDef.body.collect {
        case vd: reflect.ValDef => vd
      }.map(f => (f.name->f)).toMap
      val caseFields = constructorParamz.head.zipWithIndex.map( p => reflectOnField(reflect, paramMap)(p._1, p._2, dad, fieldDefaultMethods) )

      ScalaCaseClassInfo(
        className, 
        Nil, 
        Nil, 
        caseFields.toArray, 
        classAnnos, 
        classDef.parents.map(_.symbol.fullName), 
        isValueClass)

    // === Java Class ===
    // User-written Java classes will have the source file.  Java library files will have <no file> for source
    else if symbol.pos.sourceFile.toString.endsWith(".java") || symbol.pos.sourceFile.toString == "<no file>" then
      // Reflecting Java classes requires the materialized Class, which may be available (e.g. Java collections) or not (e.g. user-written class).
      // See if we can get it...  If not create a proxy.
      scala.util.Try {
        JavaClassInspector.inspectClass(Class.forName(symbol.fullName), paramMap)
      }.toOption.getOrElse(JavaClassInfo(symbol.fullName))

    // === Other kinds of classes (non-case Scala) ===
    else
      UnknownInfo(symbol.fullName)


  private def reflectOnField(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
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

    val fieldType = Reflector.unwindType(reflect)(valDef.tpt.tpe)

    // See if there's default values specified -- look for gonzo method on companion class.  If exists, default value is available.
    // val defaultAccessor = 
    //   fieldType match {
    //     case _: TypeSymbolInfo => None
    //     case _ =>
    //       scala.util.Try{
    //         val companionClazz = Class.forName(className+"$") // This will fail for non-case classes, including Java classes
    //         val defaultMethod = companionClazz.getMethod("$lessinit$greater$default$"+(index+1)) // This will fail if there's no default value for this field
    //         val const = companionClazz.getDeclaredConstructor()
    //         const.setAccessible(true)
    //         ()=>defaultMethod.invoke(const.newInstance())
    //       }.toOption
    //   }

    ScalaFieldInfo(index, valDef.name, fieldType, fieldAnnos, null, fieldDefaultMethods.get(index), None)

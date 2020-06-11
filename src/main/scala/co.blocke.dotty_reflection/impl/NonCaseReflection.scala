package co.blocke.dotty_reflection
package impl

import info._
import scala.tasty.Reflection


trait NonCaseClassInspector:
  self: TastyReflection =>

  def inspectNonCaseClass(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
    symbol:                reflect.Symbol,
    classDef:              reflect.ClassDef,
    superClass:            Option[ClassInfo],
    name:                  String,
    fieldDefaultMethods:   Map[Int, (String,String)],
    orderedTypeParameters: List[TypeSymbol],
    typeMembers:           List[TypeMemberInfo],
    fields:                Array[FieldInfo],
    annotations:           Map[String, Map[String,String]],
    mixins:                List[String],
    isValueClass:          Boolean
  ): ScalaClassInfo = 
    import reflect.{_, given _}

    var index: Int = fields.length - 1

    val fieldNames = fields.map(_.name)

    val varAnnos = scala.collection.mutable.Map.empty[String,Map[String, Map[String,String]]]
    val varDefDeclarations = classDef.body.collect{
        // We just want public var definitions here
        case s: ValDef if !s.symbol.flags.is(reflect.Flags.Private) 
          && !s.symbol.flags.is(reflect.Flags.Protected) 
          && !fieldNames.contains(s.name) 
          && s.symbol.flags.is(reflect.Flags.Mutable) => 
            val annoSymbol = s.symbol.annots.filter( a => !a.symbol.signature.resultSig.startsWith("scala.annotation.internal."))
            val fieldAnnos = 
              annoSymbol.map{ a => 
                val reflect.Apply(_, params) = a
                val annoName = a.symbol.signature.resultSig
                (annoName,(params collect {
                  case NamedArg(argName, Literal(Constant(argValue))) => (argName.toString, argValue.toString)
                }).toMap)
              }.toMap
            varAnnos.put(s.name, fieldAnnos) // yes, this is a side-effect but it saves mutliple field scans!
            s.name -> s.tpt.tpe.asInstanceOf[reflect.TypeRef]

        // We just want public def definitions here
        // WARNING: These defs may also include non-field functions!  Filter later...
        case d: DefDef if !d.symbol.flags.is(reflect.Flags.Private) 
          && !d.symbol.flags.is(reflect.Flags.Protected) 
          && !d.name.endsWith("_=") => d.name -> d.returnTpt.tpe.asInstanceOf[reflect.TypeRef]
    }.toMap

    val numConstructorFields = fields.length

    // Include inherited methods (var & def), including inherited!
    val baseAnnos = superClass match {
      case Some(c: ScalaClassInfo) => c.nonConstructorFields.map( f => f.name -> f.annotations ).toMap
      case _ => Map.empty[String,Map[String, Map[String,String]]]
    }

    // Include inherited methods (var & def), including inherited!
    // Produces (val <field>, method <field>_=)
    val getterSetter: List[(Symbol,Symbol)] = symbol.methods.filter(_.name.endsWith("_=")).map{ m => 
      // Trying to get the setter... which could be a val (field) if declared is a var, or it could be a method 
      // in the case of user-written getter/setter
      symbol.field(m.name.dropRight(2)) match {
        case dotty.tools.dotc.core.Symbols.NoSymbol => (symbol.method(m.name.dropRight(2)).head, m)
        case s: Symbol => (s,m)
      }
    }

    val knownAnnos = baseAnnos ++ getterSetter.map{ (fGet, fSet) =>
      val both = fGet.annots ++ fSet.annots
      val annoMap = both.map{ a => 
        val reflect.Apply(_, params) = a
        val annoName = a.symbol.signature.resultSig
        (annoName,(params collect {
          case NamedArg(argName, Literal(Constant(argValue))) => (argName.toString, argValue.toString)
        }).toMap)
      }.toMap
      val allMap = 
        annoMap ++ varAnnos.getOrElse(fGet.name, Map.empty[String,Map[String,String]]) match {
          case m if m.isEmpty => baseAnnos.getOrElse(fGet.name, Map.empty[String,Map[String,String]])
          case m => m
        }
      (fGet.name -> allMap)
    }.toMap

    val nonConstructorFields = getterSetter.map { (fGet, fSet) =>
      val fieldName = fGet.name

      // Figure out the original type symbols, i.e. T, (if any)
      val originalTypeSymbol = 
        // TOOD...
        // if paramMap.contains(fGet.getGenericReturnType.toString.asInstanceOf[TypeSymbol])
        //   Some(fGet.getGenericReturnType.toString.asInstanceOf[TypeSymbol])
        // else
          None

      val rtype = 
        originalTypeSymbol.flatMap( ots => paramMap.get(ots) ).getOrElse{
          if varDefDeclarations.contains(fieldName) then
            Reflector.unwindType(reflect, paramMap)(varDefDeclarations(fieldName))
          else
            fGet.tree match {
              case dd: DefDef => Reflector.unwindType(reflect, paramMap)(dd.returnTpt.tpe)
              case vd: ValDef => Reflector.unwindType(reflect, paramMap)(vd.tpt.tpe)
            }
        }
  
      index += 1

      ScalaFieldInfo(
        index,
        fieldName,
        rtype,
        knownAnnos(fieldName),
        fieldDefaultMethods.get(index),
        originalTypeSymbol
      )
    }.toList

    ScalaClassInfo(
      name,
      orderedTypeParameters,
      typeMembers,
      fields,
      nonConstructorFields.toArray,
      annotations,
      mixins,
      isValueClass
    )
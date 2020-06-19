package co.blocke.dotty_reflection
package impl

import info._
import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector
import dotty.tools.dotc.ast.Trees.AppliedTypeTree
  

class TastyInspection(clazz: Class[_], inTermsOf: Option[TraitInfo]) 
    extends TastyInspector:
  import Clazzes._

  var inspected: RType = UnknownInfo(clazz.getName)
  val inTermsOfParamMap = inTermsOf.map(ito => ito.orderedTypeParameters.zip(ito.actualParameterTypes).toMap).getOrElse(Map.empty[TypeSymbol, RType])

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = 
    import reflect.{_, given _}


    type PathSymbol = (String, TypeSymbol)
    val TypeSymPath = """(.*)\..+$""".r

    def findTypeSymValues( lookForSyms: List[PathSymbol], inClass: Symbol, typedTrait: TraitInfo ): Option[TypeSymbolMap] =
      // it's me
      if inClass.fullName == typedTrait.name then
        Some(typedTrait.orderedTypeParameters.zip( typedTrait.actualParameterTypes ).toMap)

      // it's somewhere in my parentage...
      else
        inClass.tree.asInstanceOf[ClassDef].parents.findMap( parent => { // return Option[TypeSymbolMap]
          parent match {
            case att: dotty.tools.dotc.ast.Trees.AppliedTypeTree[_] =>
              val applied = att.tpe.asInstanceOf[AppliedType]
              val classSymbol = applied.tycon.asInstanceOf[reflect.TypeRef].classSymbol.get
              // println("Da Class: "+classSymbol)
              val AppliedType(t,tob) = applied
              val classTypesParamList = t.typeSymbol.primaryConstructor.paramSymss.head.map{ x =>
                val TypeSymPath(tsn) = x.fullName
                (tsn, x.name.asInstanceOf[TypeSymbol])
              }
              // Get the AppliedType param types (nested Traits)
              val nestedTraits = tob.zipWithIndex.findMap( a => a match {
                case (AppliedType(t2,tob2), i: Int) => 
                  val childClassSym = t2.classSymbol.get
                  val classTypesParamList2 = t2.typeSymbol.primaryConstructor.paramSymss.head.map{ x =>
                    val TypeSymPath(tsn) = x.fullName
                    (tsn, x.name.asInstanceOf[TypeSymbol])
                  }
                  findTypeSymValues(classTypesParamList2, childClassSym, typedTrait.actualParameterTypes(i).asInstanceOf[TraitInfo])
                case _ => None
              })
              println("Nested: "+nestedTraits)
              // Get the simple types 'T'
              val symMap = tob.zipWithIndex.collect {
                case (tr: TypeRef,i) => 
                  val TypeSymPath(tsn) = tr.typeSymbol.fullName
                  (classTypesParamList(i), (tsn, tr.typeSymbol.name.asInstanceOf[TypeSymbol]))
              }.toMap
              // val symMap = classTypesParamList.zip(tobResolved).toMap
              println("Class Param List : "+classTypesParamList)
              // println("tob resolved: "+tobResolved)
              println("Sym Map: "+symMap)
              val simpleSymbols = findTypeSymValues(symMap.keySet.toList, classSymbol, typedTrait).map( typeSymMap => {
                println(" >>> TSM: "+typeSymMap)
                val TypeSymPath(tsn) = classSymbol.fullName
                typeSymMap.map{ (k,v) => (symMap((classSymbol.fullName,k))._2, v) }.toMap
              })
              println("SimpleSymbols: "+simpleSymbols)
              // Now merge the two Some[TypeSymbolMap]'s together
              // (nestedTraits, simpleSymbols) match {
              //   case (None,None) => None
              //   case (Some(n:TypeSymbolMap), None) => Some(n)
              //   case (None, Some(s:TypeSymbolMap)) => Some(s)
              //   case (Some(n:TypeSymbolMap), Some(s:TypeSymbolMap)) => Some( s ++ n )
              // }
              None
            case _ => None
          }
        })

    //--------------------------------------------------------------
      
    reflect.rootContext match {
      case ctx if ctx.isJavaCompilationUnit() => inspected = JavaClassInspector.inspectJavaClass(clazz, inTermsOfParamMap)      
      case ctx if ctx.isScala2CompilationUnit() => inspected = UnknownInfo(clazz.getName)  // Can't do much with Scala2 classes--not Tasty
      case _ => 
        val tpe = Type(clazz)
        val initialParamMap = 
          // If this is in terms of (some trait), map the class' unknown type symbol to the trait's known/typed param symbols.
          // For example:  
          //   trait Findable[F]
          //   case class Lost[L] extends Findable[L]
          // Upon call using Findable[Int], inTermsOf will be Some(TraitInfo(Findable, F->Int))
          // Now associate Lost.L -> Findable.F and finally L -> Int
          if inTermsOf.isDefined then   
            val classSymbol = tpe.classSymbol.get
            findTypeSymValues( getTypeParameters(reflect)(classSymbol).map(ts => (classSymbol.fullName,ts)), classSymbol, inTermsOf.get )
              .getOrElse(throw new ReflectException(s"${inTermsOf.get.name} is not a parent of ${clazz.getName}"))
            // inTermsOfParamMap
          else 
            inTermsOfParamMap

        val tastyReflection: TastyReflection = TastyReflection(reflect,initialParamMap)( tpe )
        inspected = tastyReflection.reflectOn
    }

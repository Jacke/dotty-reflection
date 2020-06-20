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


    def unpackAppliedType( applied: AppliedType, lookForSyms: List[PathSymbol], typedTrait: TraitInfo ): Option[TypeSymbolMap] =
      val classSymbol = applied.tycon.asInstanceOf[reflect.TypeRef].classSymbol.get
      val AppliedType(classType, classTypeArgValues) = applied
      val classTypesParamList = classType.typeSymbol.primaryConstructor.paramSymss.head.map{ x =>
        val TypeSymPath(tsn) = x.fullName
        (tsn, x.name.asInstanceOf[TypeSymbol])
      }
      // Resolve the simple types 'T' first
      val symMap = classTypeArgValues.zipWithIndex.collect {
        case (tr: TypeRef,i) => 
          val TypeSymPath(tsn) = tr.typeSymbol.fullName
          ((tsn, tr.typeSymbol.name.asInstanceOf[TypeSymbol]), classTypesParamList(i))
      }.toMap
      val invertedSymMap = symMap.map(_.swap)
      val newLookForSyms = lookForSyms.map( s => symMap.getOrElse(s, s) )
      val simpleSymbols = findTypeSymValues(newLookForSyms, classSymbol, typedTrait,-1).map( typeSymMap => {
        // Now unwind any chained types
        val TypeSymPath(tsn) = classSymbol.fullName
        typeSymMap.map{ (k,v) => 
            val maybeSym = invertedSymMap.get( (classSymbol.fullName,k) )
            maybeSym match {
              case Some((_,s)) => s -> v
              case None => k -> v
            }
          }.toMap
      })
      
      // Now resolve any nested Traits (AppliedTypes)
      val nestedTraits = classTypeArgValues.zipWithIndex.findMap( a => a match {
        case (a: AppliedType, i: Int) => 
          // do stuff to stage call...
          val AppliedType(t2,tob2) = a
          val childClassSym = t2.classSymbol.get
          val childClassTypesParamList = t2.typeSymbol.primaryConstructor.paramSymss.head.map{ cls =>
            val TypeSymPath(tsn) = cls.fullName
            (tsn, cls.name.asInstanceOf[TypeSymbol])
          }
          val nestedSymMap = tob2.zipWithIndex.collect {
            case (tr: TypeRef,i) => 
              val TypeSymPath(tsn) = tr.typeSymbol.fullName
              ((tsn, tr.typeSymbol.name.asInstanceOf[TypeSymbol]), childClassTypesParamList(i))
          }.toMap
          val newLookForSyms2 = lookForSyms.map( s =>
            nestedSymMap.getOrElse(s, s)
            )
          val invertedSymMap = nestedSymMap.map(_.swap)
          val diveIntoTrait = typedTrait.actualParameterTypes(i).asInstanceOf[TraitInfo]
          unpackAppliedType(a, newLookForSyms2, diveIntoTrait).map( typeSymMap => {
            // Now unwind any chained types
            val TypeSymPath(tsn) = childClassSym.fullName
            typeSymMap.map{ (k,v) => 
                val maybeSym = invertedSymMap.get( (childClassSym.fullName,k) )
                maybeSym match {
                  case Some((_,s)) => s -> v
                  case None => k -> v
                }
              }.toMap
          })
        case _ => None
      })
      (nestedTraits, simpleSymbols) match {
        case (None,None) => None
        case (Some(n:TypeSymbolMap), None) => Some(n)
        case (None, Some(s:TypeSymbolMap)) => Some(s)
        case (Some(n:TypeSymbolMap), Some(s:TypeSymbolMap)) => Some( s ++ n )
      }


    def findTypeSymValues(
        lookForSyms: List[PathSymbol], 
        inClass: Symbol, 
        typedTrait: TraitInfo, 
        // xsymMap: Map[PathSymbol,PathSymbol] = Map.empty[PathSymbol,PathSymbol],
        level: Int = 0
      ): Option[TypeSymbolMap] =
      
      // it's me
      if inClass.fullName == typedTrait.name then
        val myMap = typedTrait.orderedTypeParameters.zip( typedTrait.actualParameterTypes ).toMap
        Some(lookForSyms.collect{
            case s if myMap.contains(s._2) => (s._2 -> myMap(s._2))
          }.toMap)

      // it's somewhere in my parentage...
      else
        inClass.tree.asInstanceOf[ClassDef].parents.findMap( parent => { // return Option[TypeSymbolMap]
          parent match {
            case att: dotty.tools.dotc.ast.Trees.AppliedTypeTree[_] => unpackAppliedType( att.tpe.asInstanceOf[AppliedType], lookForSyms, typedTrait )
            case _ => None
          }
        })

      
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
          else 
            inTermsOfParamMap

        val tastyReflection: TastyReflection = TastyReflection(reflect,initialParamMap)( tpe )
        inspected = tastyReflection.reflectOn
    }

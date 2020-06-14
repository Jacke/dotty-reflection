package co.blocke.dotty_reflection
package impl

import info._
import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector
  

class TastyInspection(clazz: Class[_], inTermsOf: Option[TraitInfo]) 
    extends TastyInspector:
  import Clazzes._

  var inspected: RType = UnknownInfo(clazz.getName)
  val inTermsOfParamMap = inTermsOf.map(ito => ito.orderedTypeParameters.zip(ito.actualParameterTypes).toMap).getOrElse(Map.empty[TypeSymbol, RType])

  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = 
    import reflect.{_, given _}
    
    object ExtractAppliedType {
      def unapply(a: dotty.tools.dotc.ast.Trees.AppliedTypeTree[_]): Option[AppliedType] = Some(a.tpe.asInstanceOf[reflect.AppliedType])
    }

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
            val classDef = classSymbol.tree.asInstanceOf[ClassDef]
            classDef.parents.collectFirst {
              case ExtractAppliedType(a) if a.tycon.asInstanceOf[reflect.TypeRef].typeSymbol.fullName == inTermsOf.get.name => 
                val AppliedType(_, tob) = a
                val symMap = inTermsOf.get.orderedTypeParameters.zip(tob.map(_.asInstanceOf[TypeRef].typeSymbol.name.asInstanceOf[TypeSymbol])).toMap
                // now sew it all together...
                val fixedParamMap = inTermsOfParamMap.map{ (ts, rtpe) => (symMap(ts), rtpe) }.toMap
                println("Fixed: "+fixedParamMap)
                fixedParamMap
            }.getOrElse(inTermsOfParamMap)
          else 
            inTermsOfParamMap
        
        val tr = TastyReflection(reflect,initialParamMap)( tpe )
        inspected = tr.reflectOn
    }
    
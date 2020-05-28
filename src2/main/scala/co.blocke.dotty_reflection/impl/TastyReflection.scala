package co.blocke.dotty_reflection
package impl

import info._
import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
  

case class TastyReflection(qctx: QuoteContext)(aType: qctx.tasty.Type)
  extends ScalaClassInspectorLike 
  with ParamGraph:
  import qctx.tasty.{_, given _}

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

  val clazz: Class[_] = 
    aType match {
      case AppliedType(t,tob) =>
        val className = t.asInstanceOf[TypeRef].classSymbol.get.fullName
        Class.forName(className)
      case tr: TypeRef => 
        val className = tr.classSymbol.get.fullName
        Class.forName(className)
    }


  def reflectOn: RType =
    qctx.tasty.rootContext match {
      // case ctx if ctx.isJavaCompilationUnit() => JavaClassInspector.inspectClass(clazz, initialParamMap)      
      // case ctx if ctx.isScala2CompilationUnit() => UnknownInfo(clazz)  // Can't do much with Scala2 classes--not Tasty
      // case ctx if ctx.isAlreadyLoadedCompilationUnit() => 
      //   ExtractorRegistry.extractors.collectFirst {
      //     case e if e.matches(clazz) => inspected = e.emptyInfo(clazz, initialParamMap)
      //   }
      case ctx => 
        println(">>> "+ctx)
        BogusInfo()
        // inspectClass(clazz.getName, qctx.tasty, initialParamMap)(ctx.asInstanceOf[dotty.tools.dotc.core.Contexts.Context].tree.asInstanceOf[qctx.tasty.Tree])
    }
package co.blocke.dotty_reflection
package info

import java.lang.reflect._
import java.util.Optional
import impl._
import scala.tasty.Reflection


trait OptionInfo extends Transporter.RType:
  lazy val optionParamType: Transporter.RType


case class ScalaOptionInfo protected[dotty_reflection](
  name: String,
  _optionParamType: Transporter.RType
) extends OptionInfo:

  val fullName: String = name + "[" + _optionParamType.fullName  + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val optionParamType: Transporter.RType = _optionParamType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  override def toType(reflect: Reflection): reflect.Type = 
    import reflect.{_, given _}
    AppliedType(Type(infoClass), List(optionParamType.toType(reflect)))

  override def findPaths(findSyms: Map[TypeSymbol,Path], referenceTrait: Option[TraitInfo] = None): (Map[TypeSymbol, Path], Map[TypeSymbol, Path]) = 
    optionParamType match {
      case ts: TypeSymbolInfo if findSyms.contains(ts.name.asInstanceOf[TypeSymbol]) =>
        val sym = ts.name.asInstanceOf[TypeSymbol]
        (Map( ts.name.asInstanceOf[TypeSymbol] -> findSyms(sym).push(OptionPathElement()) ), findSyms - sym)
      case other => 
        other.findPaths(findSyms.map( (k,v) => k -> v.push(OptionPathElement()) ))
    }

  override def resolveTypeParams( paramMap: Map[TypeSymbol, Transporter.RType] ): Transporter.RType = 
    _optionParamType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => this.copy(_optionParamType = paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case pt: impl.PrimitiveType => this
      case other => this.copy(_optionParamType = other.resolveTypeParams(paramMap))
    }

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + "Option of " + optionParamType.show(newTab,name :: seenBefore,true)


case class JavaOptionalInfo protected[dotty_reflection](
  name: String,
  _optionParamType: Transporter.RType
) extends OptionInfo:

  val fullName: String = name + "[" + _optionParamType.fullName  + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val optionParamType: Transporter.RType = _optionParamType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  override def toType(reflect: Reflection): reflect.Type = 
    import reflect.{_, given _}
    AppliedType(Type(infoClass), List(optionParamType.toType(reflect)))

  override def resolveTypeParams( paramMap: Map[TypeSymbol, Transporter.RType] ): Transporter.RType = 
    _optionParamType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => this.copy(_optionParamType = paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case pt: impl.PrimitiveType => this
      case other => this.copy(_optionParamType = other.resolveTypeParams(paramMap))
    }
    
  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + "Optional of " + optionParamType.show(newTab,name :: seenBefore,true)

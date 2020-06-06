package co.blocke.dotty_reflection
package info

import java.lang.reflect._
import java.util.Optional


trait OptionInfo extends RType:
  lazy val optionParamType: RType


case class ScalaOptionInfo protected[dotty_reflection](
  name: String,
  _optionParamType: RType
) extends OptionInfo:

  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val optionParamType: RType = _optionParamType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  lazy val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + "Option of " + optionParamType.show(newTab,true)


case class JavaOptionalInfo protected[dotty_reflection](
  name: String,
  _optionParamType: RType
) extends OptionInfo:

  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val optionParamType: RType = _optionParamType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  lazy val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + "Optional of " + optionParamType.show(newTab,true)

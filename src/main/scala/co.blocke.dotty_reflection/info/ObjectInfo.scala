package co.blocke.dotty_reflection
package info

case class ObjectInfo protected[dotty_reflection](
    name: String
  ) extends RType:

  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val orderedTypeParameters: List[TypeSymbol] = Nil

  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name)\n"

package co.blocke.dotty_reflection
package info


/** This is for all the classes we don't inspect.  These may be "invalid" or just not reflectable.
  * Rather than toss our exception cookies, we just return UnknownInfo and let the caller decide
  * how serious this is.  In the case of ScalaJack, it may be completely fine, for example UUID.
  * We can make a ScalaJack TypeAdapter for UUID without needing to inspect the type.  For some
  * other application an UnknownInfo might be a serious problem.
  */
case class UnknownInfo(name: String) extends RType:
  lazy val infoClass: Class[_] = Class.forName(name)
  val orderedTypeParameters = Nil
  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name)\n"
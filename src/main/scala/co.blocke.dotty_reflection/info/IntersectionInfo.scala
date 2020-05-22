package co.blocke.dotty_reflection
package info


case class IntersectionInfo protected[dotty_reflection](
  val name: String,
  val _leftType: RType,
  val _rightType: RType
  ) extends RType:

    val orderedTypeParameters: List[TypeSymbol] = Nil

    val infoClass: Class[_] = Clazzes.AnyClazz

    lazy val leftType: RType = _leftType match {
      case e: SelfRefRType => Reflector.reflectOnClass(e.infoClass)
      case e => e
    }
    lazy val rightType: RType = _rightType match {
      case e: SelfRefRType => Reflector.reflectOnClass(e.infoClass)
      case e => e
    }

    def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
      val newTab = {if supressIndent then tab else tab+1}
      {if(!supressIndent) tabs(tab) else ""} + "Intersection:\n"
      + tabs(newTab)+ "left--" + leftType.show(newTab+1,true)
      + tabs(newTab)+ "right--" + rightType.show(newTab+1,true)


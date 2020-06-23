package co.blocke.dotty_reflection
package info

// import impl.ClassOrTrait

case class TraitInfo protected[dotty_reflection](
    name: String, 
    _orderedTypeParameters: List[TypeSymbol],
    actualParameterTypes: Array[RType]
  ) extends RType: // with ClassOrTrait:

  lazy val orderedTypeParameters = _orderedTypeParameters
  lazy val infoClass: Class[_] = Class.forName(name)

  lazy val typedParams = orderedTypeParameters.zip(actualParameterTypes).toMap

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    val both = orderedTypeParameters.zip(actualParameterTypes)

    if seenBefore.contains(name) then
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name) (self-ref recursion)\n"
    else
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
      + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}])""" else ")"}
      + {if actualParameterTypes.isEmpty then "\n" else ":\n"+ tabs(newTab) + "actualParamTypes:\n" 
      + both.map( (p,a) => tabs(newTab+1) + s"[$p] "+a.show(newTab+2,name :: seenBefore,true)).mkString}


case class SealedTraitInfo protected(
    name: String, 
    _orderedTypeParameters: List[TypeSymbol],
    children: Array[RType]
  ) extends RType: // with ClassOrTrait:

  lazy val orderedTypeParameters = _orderedTypeParameters
  lazy val infoClass: Class[_] = Class.forName(name)

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    if seenBefore.contains(name) then
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name) (self-ref recursion)\n"
    else
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
      + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}])""" else ")"}
      + {if children.isEmpty then "\n" else ":\n"+ tabs(newTab) + "children:\n" + children.map(_.show(newTab+1,name :: seenBefore)).mkString}
  
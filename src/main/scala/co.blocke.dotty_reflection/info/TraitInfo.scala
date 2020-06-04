package co.blocke.dotty_reflection
package info

// import impl.ClassOrTrait

case class TraitInfo protected[dotty_reflection](
    name: String, 
    orderedTypeParameters: List[TypeSymbol],
    actualParameterTypes: Array[RType]
  ) extends RType: // with ClassOrTrait:

  lazy val infoClass: Class[_] = Class.forName(name)

  def setActualTypeParameters( params: Array[RType] ) = this.copy(actualParameterTypes = params)
  lazy val typedParams = orderedTypeParameters.zip(actualParameterTypes).toMap

  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    val both = orderedTypeParameters.zip(actualParameterTypes)

    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}])""" else ")"}
    + {if actualParameterTypes.isEmpty then "\n" else ":\n"+ tabs(newTab) + "actualParamTypes:\n" 
    + both.map( (p,a) => tabs(newTab+1) + s"[$p] "+a.show(newTab+2,true)).mkString}
    // + actualParameterTypes.map(_.show(newTab+1)).mkString+"\n"}


case class SealedTraitInfo protected(
    name: String, 
    orderedTypeParameters: List[TypeSymbol],
    children: Array[RType]
  ) extends RType: // with ClassOrTrait:

  lazy val infoClass: Class[_] = Class.forName(name)

  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}])""" else ")"}
    + {if children.isEmpty then "\n" else ":\n"+ tabs(newTab) + "children:\n" + children.map(_.show(newTab+1)).mkString}
  
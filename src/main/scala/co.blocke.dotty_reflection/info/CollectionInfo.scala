package co.blocke.dotty_reflection
package info

/** Arity 1 Collections, e.g. List, Set, Seq */
case class SeqLikeInfo protected[dotty_reflection](
  name: String,
  _elementType: RType
) extends RType with CollectionType:

  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }


/** Arity 2 Collections, Map flavors, basiclly */
case class MapLikeInfo protected[dotty_reflection](
  name: String,
  _elementType: RType,
  _elementType2: RType
) extends RType with CollectionType:

  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }
  lazy val elementType2: RType = _elementType2 match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  override def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]):\n""" else "):\n"}
    + elementType.show(newTab)
    + elementType2.show(newTab)


/** Scala Array */
case class ArrayInfo protected[dotty_reflection](
  name: String,
  _elementType: RType
) extends RType:

  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val orderedTypeParameters = infoClass.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"array of " + elementType.show(newTab,name :: seenBefore,true)


/** Java Set dirivative */
case class JavaSetInfo protected[dotty_reflection](
  name: String,
  _orderedTypeParameters: List[TypeSymbol],
  _elementType: RType
) extends RType with CollectionType:

  lazy val orderedTypeParameters = _orderedTypeParameters
  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }


/** Java List dirivative */
case class JavaListInfo protected[dotty_reflection](
  name: String,
  _orderedTypeParameters: List[TypeSymbol],
  _elementType: RType
) extends RType with CollectionType:

  lazy val orderedTypeParameters = _orderedTypeParameters
  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

/** Java Array */
case class JavaArrayInfo protected[dotty_reflection](
  _elementType: RType
) extends RType:

  val name: String = JAVA_ARRAY_CLASS
  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val orderedTypeParameters: List[TypeSymbol] = Nil

  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"array of " + elementType.show(newTab,name :: seenBefore,true)


/** Java Queue dirivative */
case class JavaQueueInfo protected[dotty_reflection](
  name: String,
  _orderedTypeParameters: List[TypeSymbol],
  _elementType: RType
) extends RType with CollectionType:
  lazy val orderedTypeParameters = _orderedTypeParameters
  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

/** Java Stack dirivative */
case class JavaStackInfo protected[dotty_reflection](
  name: String,
  _orderedTypeParameters: List[TypeSymbol],
  _elementType: RType
) extends RType with CollectionType:
  lazy val orderedTypeParameters = _orderedTypeParameters
  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

/** Java Map dirivative */
case class JavaMapInfo protected[dotty_reflection](
  name: String,
  _orderedTypeParameters: List[TypeSymbol],
  _elementType: RType,
  _elementType2: RType
) extends RType with CollectionType:
  lazy val orderedTypeParameters = _orderedTypeParameters
  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }
  lazy val elementType2: RType = _elementType2 match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  override def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]):\n""" else "):\n"}
    + elementType.show(newTab,name :: seenBefore)
    + elementType2.show(newTab,name :: seenBefore)

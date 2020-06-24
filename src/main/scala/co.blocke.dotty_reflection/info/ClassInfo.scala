package co.blocke.dotty_reflection
package info


trait ClassInfo extends RType: 
  val name:                       String
  lazy val fields:                Array[FieldInfo]
  lazy val orderedTypeParameters: List[TypeSymbol]
  lazy val typeMembers:           Array[TypeMemberInfo]
  lazy val annotations:           Map[String, Map[String,String]]
  lazy val mixins:                List[String]

  def hasMixin(mixin: String): Boolean = mixins.contains(mixin)


case class ScalaCaseClassInfo protected[dotty_reflection] (
    name:                   String,
    _orderedTypeParameters: List[TypeSymbol],
    actualParameterTypes:   Array[RType],
    _typeMembers:           Array[TypeMemberInfo],
    _fields:                Array[FieldInfo],
    _annotations:           Map[String, Map[String,String]],
    _mixins:                List[String],
    isValueClass:           Boolean
  ) extends ClassInfo:

  // All this laziness is because Java classes use a proxy that isn't resolved until runtime.
  lazy val orderedTypeParameters = _orderedTypeParameters
  lazy val typeMembers = _typeMembers
  lazy val annotations = _annotations
  lazy val mixins = _mixins
  lazy val infoClass: Class[_] = Class.forName(name)

  // Fields may be self-referencing, so we need to unwind this...
  lazy val fields = _fields.map( f => f.fieldType match {
    case s: SelfRefRType => f.asInstanceOf[ScalaFieldInfo].copy(fieldType = s.resolve)
    case s => f
  })

  lazy val constructor = 
    infoClass.getConstructor(fields.map(_.asInstanceOf[ScalaFieldInfo].constructorClass):_*)

  // Used for ScalaJack writing of type members ("external type hints").  If some type members are not class/trait, it messes up any
  // type hint modifiers, so for the purposes of serialization we want to filter out "uninteresting" type members (e.g. primitives)
  def filterTraitTypeParams: ScalaCaseClassInfo = this.copy( _typeMembers = typeMembers.filter(tm => tm.memberType.isInstanceOf[TraitInfo] || tm.memberType.isInstanceOf[ScalaCaseClassInfo]) )

  def show(tab:Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    if seenBefore.contains(name) then
      val params = if actualParameterTypes.nonEmpty then actualParameterTypes.map(_.name).mkString("[",",","]") else ""
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name$params) (self-ref recursion)\n"
    else
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
      + {if isValueClass then "--Value Class--" else ""}
      + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]):\n""" else "):\n"}
      + tabs(newTab) + "fields:\n" + {if modified then fields.map(f => tabs(newTab+1) + f.name+s"<${f.fieldType.infoClass.getName}>\n").mkString else fields.map(_.show(newTab+1, name::seenBefore)).mkString}
      + {if annotations.nonEmpty then tabs(newTab) + "annotations: "+annotations.toString + "\n" else ""}
      + {if( typeMembers.nonEmpty ) tabs(newTab) + "type members:\n" + typeMembers.map(_.show(newTab+1,name :: seenBefore)).mkString else ""}

//------------------------------------------------------------


case class ScalaClassInfo protected[dotty_reflection] (
    name:                   String,
    _orderedTypeParameters: List[TypeSymbol],
    actualParameterTypes:   Array[RType],
    _typeMembers:           Array[TypeMemberInfo],
    _fields:                Array[FieldInfo],  // constructor fields
    nonConstructorFields:   Array[FieldInfo],
    _annotations:           Map[String, Map[String,String]],
    _mixins:                List[String],
    isValueClass:           Boolean
  ) extends ClassInfo:

  // All this laziness is because Java classes use a proxy that isn't resolved until runtime.
  lazy val orderedTypeParameters = _orderedTypeParameters
  lazy val typeMembers = _typeMembers
  lazy val annotations = _annotations
  lazy val mixins = _mixins
  lazy val infoClass: Class[_] = Class.forName(name)
 
  // Fields may be self-referencing, so we need to unwind this...
  lazy val fields = _fields.map( f => f.fieldType match {
    case s: SelfRefRType => f.asInstanceOf[ScalaFieldInfo].copy(fieldType = s.resolve)
    case s => f
  })
  
  lazy val constructor = 
    infoClass.getConstructor(fields.map(_.asInstanceOf[ScalaFieldInfo].constructorClass):_*)
    
  // Used for ScalaJack writing of type members ("external type hints").  If some type members are not class/trait, it messes up any
  // type hint modifiers, so for the purposes of serialization we want to filter out "uninteresting" type members (e.g. primitives)
  def filterTraitTypeParams: ScalaClassInfo = this.copy( _typeMembers = typeMembers.filter(tm => tm.memberType.isInstanceOf[TraitInfo] || tm.memberType.isInstanceOf[ScalaCaseClassInfo]) )

  def show(tab:Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    val showNCFields = {if !modified then nonConstructorFields else nonConstructorFields.sortBy(_.name) }

    if seenBefore.contains(name) then
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name) (self-ref recursion)\n"
    else
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
      + {if isValueClass then "--Value Class--" else ""}
      + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]):\n""" else "):\n"}
      + tabs(newTab) + "fields:\n" + fields.map(_.show(newTab+1,name :: seenBefore)).mkString
      + tabs(newTab) + "non-constructor fields:\n" + showNCFields.map(_.show(newTab+1,name :: seenBefore, supressIndent, modified)).mkString
      + {if annotations.nonEmpty then tabs(newTab) + "annotations: "+annotations.toString + "\n" else ""}
      + {if( typeMembers.nonEmpty ) tabs(newTab) + "type members:\n" + typeMembers.map(_.show(newTab+1,name :: seenBefore)).mkString else ""}


//------------------------------------------------------------


/** Java class reflection has a special problem... we need the class file, which isn't available during compilation (i.e. inside a macro).
 *  The best we can do is capture the name of the class and materialize/reflect on the class outside of the macro, lazy-like.
 */
case class JavaClassInfo protected[dotty_reflection] ( name: String, paramMap: TypeSymbolMap ) extends ClassInfo:
  lazy val infoClass: Class[_] = Class.forName(name)
  private lazy val proxy = impl.JavaClassInspector.inspectJavaClass(infoClass, paramMap, true).asInstanceOf[JavaClassInfoProxy]
  lazy val fields = proxy.fields
  lazy val orderedTypeParameters: List[TypeSymbol]                = proxy.orderedTypeParameters
  lazy val typeMembers:           Array[TypeMemberInfo]           = proxy.typeMembers
  lazy val annotations:           Map[String, Map[String,String]] = proxy.annotations
  lazy val mixins:                List[String]                    = proxy.mixins

  def field(name: String): Option[JavaFieldInfo] = fieldsByName.get(name)

  private lazy val fieldsByName = fields.map(f => (f.name, f.asInstanceOf[JavaFieldInfo])).toMap

  def show(tab:Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}

    if seenBefore.contains(name) then
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name) (self-ref recursion)\n"
    else
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
      + s"($name" + {if orderedTypeParameters.nonEmpty then s"""[${orderedTypeParameters.mkString(",")}]):\n""" else "):\n"}
      + tabs(newTab) + "fields:\n" + fields.map(_.show(newTab+1,name :: seenBefore)).mkString
      + {if annotations.nonEmpty then tabs(newTab) + "annotations: "+annotations.toString + "\n" else ""}
  

case class JavaClassInfoProxy protected[dotty_reflection] (
    name:                   String,
    _fields:                Array[FieldInfo],
    _orderedTypeParameters: List[TypeSymbol],
    _annotations:           Map[String, Map[String,String]]
  ) extends RType:

  lazy val annotations = _annotations
  lazy val orderedTypeParameters = _orderedTypeParameters
  lazy val infoClass: Class[_] = Class.forName(name)

  // Run up the interitance tree to the top (Object) to get all the superclasses and mixin interfaces of this one
  private def getSuperclasses(c: Class[_] = infoClass, stack:List[String] = List.empty[String]): List[String] = 
    val ammendedStack = (stack :+ c.getName) ::: c.getInterfaces.toList.map(_.getName)
    val sc = c.getSuperclass()
    if( sc == classOf[Object] || sc == null)
      ammendedStack
    else 
      getSuperclasses(sc, ammendedStack)

  lazy val mixins = getSuperclasses()
 

  // Fields may be self-referencing, so we need to unwind this...
  lazy val fields = _fields.map( f => f.fieldType match {
    case s: SelfRefRType => f.asInstanceOf[JavaFieldInfo].copy(fieldType = s.resolve)
    case s => f
  })

  val typeMembers: Array[TypeMemberInfo] = Nil.toArray  // unused for Java classes but needed on ClassInfo

  def show(tab:Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name)\n"

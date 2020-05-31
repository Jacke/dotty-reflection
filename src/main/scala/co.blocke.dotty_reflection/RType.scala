package co.blocke.dotty_reflection

/** A materializable type */
trait RType extends Serializable:
  val name: String         /** typically the fully-qualified class name */
  lazy val infoClass: Class[_]  /** the JVM class of this type */
  val orderedTypeParameters: List[TypeSymbol]  /** if this is a parameterized type,  list of type symbols in order of declaration */
  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String  // modified is "special", ie. don't show index & sort for nonconstructor fields
  inline final def isParameterized: Boolean = !orderedTypeParameters.isEmpty
  override def toString(): String = show()


case class TypeMemberInfo(name: String, typeSymbol: TypeSymbol, memberType: RType) extends RType {
  val orderedTypeParameters: List[TypeSymbol] = Nil
  lazy val infoClass = Clazzes.ObjectClazz
  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    {if(!supressIndent) tabs(tab) else ""} + name + s"[$typeSymbol]: "+ memberType.show(tab+1, true)
}


case class BogusInfo() extends RType {
  val name = "Bogus"
  lazy val infoClass = Class.forName("co.blocke.dotty_reflection.BogusInfo")
  val orderedTypeParameters: List[TypeSymbol] = Nil
  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = "Foom..." 
}


case class TypeSymbolInfo(name: String) extends RType:
  val orderedTypeParameters: List[TypeSymbol] = Nil
  lazy val infoClass = Clazzes.ObjectClazz
  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    {if(!supressIndent) tabs(tab) else ""} + name + "\n"


// Placeholder to be lazy-resolved, used for self-referencing types
// When one of this is encountered in the wild, just re-Reflect on the infoClass and you'll get the non-SelfRef (i.e. normal) RType
case class SelfRefRType(name: String, params: List[RType] = Nil) extends RType:
  lazy val infoClass = Class.forName(name)
  val orderedTypeParameters: List[TypeSymbol] = Nil
  def resolve = info.UnknownInfo(name) // TODO!  Need to reflect w/o having a Class object
  // def resolve = params match {
  //   case Nil => Reflector.reflectOnClass(infoClass)
  //   case p => Reflector.reflectOnClassWithParams(infoClass, p)
  // }
  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = s"SelfRefRType of $name" 



object RType:
  inline def of[T](implicit ct: scala.reflect.ClassTag[T]): RType = Reflector.reflectOn[T]


// Poked this here for now.  Used for show()
final inline def tabs(t:Int) = "   "*t

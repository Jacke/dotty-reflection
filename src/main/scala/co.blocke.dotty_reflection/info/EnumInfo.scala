package co.blocke.dotty_reflection
package info

/** Something to smooth the differences between the 2.x Enumeration class and the 3.x Enum class
 */
trait EnumInfo extends RType:
  lazy val infoClass: Class[_]
  val values: List[String]
  def ordinal(s: String): Int
  def valueOf(s: String): Any
  def valueOf(i: Int): Any
  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s" with values [${values.map(_.toString).mkString(",")}]\n"


case class ScalaEnumInfo protected[dotty_reflection](
  name: String,
  values: List[String]
) extends EnumInfo: 
  val orderedTypeParameters = Nil

  lazy val infoClass: Class[_] = Class.forName(name+"$")

  private lazy val companionConst = {
    val const = infoClass.getDeclaredConstructor()
    const.setAccessible(true)
    const
  }
  private lazy val companionInstance = companionConst.newInstance()
  private lazy val ordinalMethod = infoClass.getMethod("ordinal", classOf[Object])
  private lazy val valueOfMethod = infoClass.getMethod("valueOf", classOf[String])

  def ordinal(s: String): Int = 
    val target = valueOfMethod.invoke(companionInstance, s)
    ordinalMethod.invoke(companionInstance, target).asInstanceOf[Int]
  def valueOf(s: String): Any = valueOfMethod.invoke(companionInstance,s)
  def valueOf(i: Int): Any = values(i)


case class ScalaEnumerationInfo protected[dotty_reflection](
  name: String,
  values: List[String]
) extends EnumInfo:
  val orderedTypeParameters = Nil

  lazy val infoClass: Class[_] = Class.forName(name+"$")

  private lazy val companionConst = {
    val const = infoClass.getDeclaredConstructor()
    const.setAccessible(true)
    const
  }
  private lazy val companionInstance = companionConst.newInstance()
  private lazy val withNameMethod = infoClass.getMethod("withName", classOf[String])
  private lazy val applyMethod = infoClass.getMethod("apply", classOf[Int])

  def ordinal(s: String): Int = valueOf(s).asInstanceOf[Enumeration#Value].id
  def valueOf(s: String): Any = withNameMethod.invoke(companionInstance,s)
  def valueOf(i: Int): Any = applyMethod.invoke(companionInstance,i.asInstanceOf[Object])


  
case class JavaEnumInfo protected[dotty_reflection](
  name: String
) extends RType: 
  val orderedTypeParameters = Nil

  lazy val infoClass: Class[_] = Class.forName(name)

  def show(tab: Int = 0, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName +s"(${infoClass.getName})\n"

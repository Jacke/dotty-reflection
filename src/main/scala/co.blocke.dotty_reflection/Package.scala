package co.blocke.dotty_reflection
import scala.runtime.Statics.releaseFence

/** Mnemonic symbol for a type--typically a paramaterized type, e.g. Foo[T], where T is the symbol */
opaque type TypeSymbol = String 

type TypeSymbolMap = Map[TypeSymbol,RType]

/** A union type is resolved to AnyRef, which isn't helpful.  This is a marker class name to differentiate a union type */
val UNION_CLASS = "__union_type__"

/** An intersection type is resolved to AnyRef, which isn't helpful.  This is a marker class name to differentiate a union type */
val INTERSECTION_CLASS = "__intersection_type__"

/** Java Arrays devolve into java.util.List, which isn't quite the same thing, so we created this placeholder */
val JAVA_ARRAY_CLASS = "__array__"

/** Any is an abstract class in Scala, so Class.forName() won't work.  Need this marker. */
val ANY_CLASS = "scala.Any"

class ReflectException(msg: String) extends Exception(msg)

val ENUM_CLASSNAME = "scala.Enumeration.Value"

val typesymregx = """.*\.\_\$(.+)$""".r

def getTypeParameters(reflect: scala.tasty.Reflection)(symbol: reflect.Symbol): List[TypeSymbol] = 
  symbol.primaryConstructor.paramSymss match {
    case Nil => Nil
    case p if p.nonEmpty  => p.head.filter(_.isType).map(_.name.asInstanceOf[TypeSymbol])
    case _   => Nil
  }

extension ListOps on [A,B](xs: List[A]) {
  def findMap( p: (A) => Option[B] ): Option[B] = 
    var these: List[A] = xs
    while (!these.isEmpty) {
      val pRet = p(these.head)
      if pRet.isDefined then return pRet
      these = these.tail
    }
    None

  def filterMap(p: A => Option[B]): List[B] = 
    def doit(l: List[A], acc: List[B]): List[B] = {
      if (l.isEmpty)
        acc
      else {
        val retVal = p(l.head)
        val newAcc = if retVal.isDefined then
            acc :+ retVal.get
          else 
            acc
        doit(l.tail, newAcc)
      }
    }
    val result = doit(xs, Nil)
    releaseFence()
    result
}

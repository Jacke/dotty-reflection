package co.blocke.dotty_reflection

// import impl._ 
import info._
import co.blocke.reflect._

trait Baseless[X]{ val b: X }
trait Basement[R]{ val b: R }
trait Base[B] extends Basement[B]
trait Mid[M] extends Base[M]
case class Top[T]( b: T ) extends Mid[T]

trait T5[X, Y] { val thing1: X; val thing2: Y }
trait T10[X, Y] { val x: X; val y: Y }
trait T11[W, Z] { val w: W; val z: Z }
case class TFoo6[A, B, C, D](x: T11[C, T5[D, A]], y: B) extends T10[T11[C, T5[D, A]], B]
case class TBlah1[A, B](w: A, z: B) extends T11[A, B]
case class TBar7[A, B](thing1: A, thing2: B) extends T5[A, B]


trait Level2[W]{ val w: W }
trait Level1[T,U]{ val t: T; val u: U }
case class Level0[X,Y]( t: Level2[Y], u: X ) extends Level1[Level2[Y],X]
// Also try...
// case class Level0[X,Y]( t: Level2[Y,X], u: X ) extends Level1[Level2[Y,X],X]
// case class SomeChildOfLevel2[P]( w: P ) etends Level2[P]
// case class Level0[X,Y]( t: SomeChildOfLevel2[Y], u: X ) extends Level1[Level2[Y],X]
// case class Midi[M]( w: M ) extends Level2[M]

// trait Movable[M,N]{ val m: M; var n: N }
// trait Stackable[S,R,X] extends Movable[S,X] with Countable[R]
// trait Countable[C] { val c: C }
// case class Thing[T,U,V](m: T, c: Countable[U]) extends Stackable[T, Countable[U], Liftable[V]]

// reflectOn[Thing].inTermsOf(Movable[Int, Liftable[String]])
// At compile-time we know the refolved types of Movable M -> Int
// We also know that Thing has parameter T.  We don't know how to wind thu Stackable: T->S, S->M ==> T -> M ==> T -> Int
// Countable is an AppliedType, so we pull it apart and discover Thing.U in tob and make the association U -> C, but...
//    since we did inTermsOf[Movable[Int]] we have no knowledge of resolved type ofr Countable.C!  Thing.U remains unknown... perhaps that's ok?
//
// For Liftable, we need to associate Thing.V -> Stackable.X -> Movable.N, which resolves to Liftable[String], from which we further unpack and associate Thing.V -> String


trait Foom[T]{ val t: T }
case class Noom[N]( t: N ) extends Foom[N]

object RunMe extends App:

  // Simple multi-level case
  //--------------------------
  // val f = Reflector.reflectOn[Basement[Int]].asInstanceOf[TraitInfo]
  // println(Reflector.reflectOnClass(Class.forName("co.blocke.dotty_reflection.Top"), Some(f)))

  // 1 level of indirection
  //--------------------------
  val f = Reflector.reflectOn[Level1[Level2[Int],String]].asInstanceOf[TraitInfo]
  println(f)
  println(Reflector.reflectOnClass(Class.forName("co.blocke.dotty_reflection.Level0"), Some(f)))

  // val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
  // val r = Reflector.reflectOn[T10[T11[Int, T5[Double, Char]], String]].asInstanceOf[TraitInfo]
  // println(Reflector.reflectOnClass( inst.getClass, Some(r) ))

  // val f = Reflector.reflectOn[Foom[Int]].asInstanceOf[TraitInfo]
  // println(f)
  // println(Reflector.reflectOnClass(Class.forName("co.blocke.dotty_reflection.Noom"), Some(f)))
  // println("done.")

  /*
  TraitInfo(co.blocke.dotty_reflection.Level1[T,U]):
   actualParamTypes:
      [T] TraitInfo(co.blocke.dotty_reflection.Level2[W]):
         actualParamTypes:
            [W] scala.Int
      [U] java.lang.String

  Need to infer:
    Level0.Y -> Int
    Level0.X -> String
  */
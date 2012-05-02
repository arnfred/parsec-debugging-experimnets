package crash

object Test1 {

  trait SomeInfo
  
  case object NoInfo extends SomeInfo
  
  sealed abstract class Res[+T] {
    def append[U >: T](a: => Res[U]): Res[U]
  }

  case class OkRes[+T](v: T) extends Res[T] {
    def append[U >: T](a: => Res[U]): Res[U] = this
  }

  case object NotRes extends Res[Nothing] {
    def append[U >: Nothing](a: => Res[U]): Res[U] = a/*a match {
      case OkRes(v0) => a
      case NotRes    => this
    }*/
  }
 
  object Base {
    def apply[T](f: String => Res[T]): Base[T] = new Base[T]{ def apply(in: String) = f(in) }
  }

  abstract class Base[+T] {
    def apply(f: String): Res[T]
    def join[U >: T](a: => Base[U]): Base[U] = Base { in => this(in) append a(in) }
    // implicit crashes the compiler
    def |[U >: T](a: => Base[U], i: SomeInfo = NoInfo): Base[U] = null//join(a)
    def ~[U >: T](a: => Base[U]): Base[U] = this
  }

  implicit def fromStringTOBase(a: String): Base[String] = Base { in => if (in == a) OkRes(in) else NotRes }
  //implicit val z = NoInfo
  
  def Sample: Base[Any] = (
    "abc"~ rep ("zenek" | "halo")
   | "foobar"
   | "barbar")
   
  def rep[T](p: => Base[T]): Base[List[T]] = null // whatever

  def main(args: Array[String]) {


  }

}
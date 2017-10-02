import scala.language.implicitConversions

object polymorphism {
  //parametric polymorphism
  def head[A](xs: List[A]): A = xs(0)

  head(List(1, 2, 3))

  //subtype polymorphism
  //def plus[A](a1: A, a2: A): A = ???

  trait Plus[A] {
    def plus(a2: A): A
  }

  //it will work for custom types
  case class Inn(i: Int) extends Plus[Inn] {
    override def plus(a2: Inn): Inn = Inn(i + a2.i)
  }

  //but it wont work for int, string, etc.
  def plus[A <: Plus[A]](a1: A, a2: A): A = a1.plus(a2)

  plus(Inn(12), Inn(13))

  //mappend-hoc polymorphism
  trait PlusA[A] {
    def plus(a1: A, a2: A): A
  }

  def plusA[A](a1: A, a2: A)(implicit f: PlusA[A]): A = f.plus(a1, a2)

  implicit val int = new PlusA[Int] {
    override def plus(a1: Int, a2: Int) = a1 + a2
  }
  implicit val string = new PlusA[String] {
    override def plus(a1: String, a2: String) = a1 + a2
  }

  plusA(2, 3)
  plusA("2", "3")

  def plusAP[A: PlusA](a1: A, a2: A): A = implicitly[PlusA[A]].plus(a1, a2)

  trait Monoid[T] {
    def mappend(a1: T, a2: T): T

    val mzero: T
  }


  //Just a note on Monoid
  // it is a semigroup, append operation
  // that implies associativity
  // zeroth value
  // there can be multiple implementation of monoid for a given type
  //e.g.
  //Int is a Monoid for add operation with zeroth value as 0
  //Int is a Monoid for mul operation with zeroth value as 1

  implicit val i = new Monoid[Int] {
    override def mappend(a1: Int, a2: Int) = a1 + a2

    override val mzero = 0
  }

  trait FLeft[C[_]]{
    def foldLeft[A](xs:C[A],zero:A,f:(A,A)=>A):A
  }

  implicit val list = new FLeft[List] {
    override def foldLeft[A](xs: List[A], zero: A, f: (A, A) => A) =
      xs.foldLeft(zero)(f)
  }

  def sum[F[_]:FLeft ,T: Monoid](xs: F[T]): T = {
    val x = implicitly[Monoid[T]]
    val f = implicitly[FLeft[F]]
    f.foldLeft(xs, x.mzero, x.mappend)
  }

  //type class consists of 4 things
  /*
  * The interface it self (e.g. FLeft, Monoid, etc.)
  * Type which obeys the interface laws (e.g. Int, List etc.)
  * implementation or instance (e.g. int, list, etc.)
  * and interface method (which help to use the interface e.g. sum method)
  * */

  sum(List(1, 2, 3))

  trait MonoidOp[A] {
    val F: Monoid[A]
    val value: A
    def |+|(a2: A) = F.mappend(value, a2)
  }

  implicit def toMonoidOp[A: Monoid](a: A): MonoidOp[A] = new MonoidOp[A] {
    val F = implicitly[Monoid[A]]
    val value = a
  }

  3 |+| 4


}
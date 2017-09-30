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

  //add-hoc polymorphism
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

  def plusAP[A: PlusA](a1: A, a2: A): A = implicitly[PlusA[A]].plus(a1,a2)

}
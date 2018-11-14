object Main extends App {
  case class Salary(credits: Int, currency: String)

  trait Semigroup[T] {
    def combine(a: T, b: T): T
  }

  trait Monoid[T] extends Semigroup[T] {
    def empty: T
  }

  implicit val addIntSemigroup = new Semigroup[Int] {
    override def combine(a: Int, b: Int): Int = a + b
  }

  implicit val addIntMonoid = new Monoid[Int] {
    override def empty: Int = 0
    override def combine(a: Int, b: Int): Int = addIntSemigroup.combine(a, b)
  }

  val mulIntSemigroup = new Semigroup[Int] {
    override def combine(a: Int, b: Int): Int = a * b
  }

  /*implicit*/ val mulIntMonoid = new Monoid[Int] {
    override def empty: Int = 1
    override def combine(a: Int, b: Int): Int = mulIntSemigroup.combine(a, b)
  }

  val concatStringSemigroup = new Semigroup[String] {
    override def combine(a: String, b: String): String = a + b
  }

  implicit val concatStringMonoid = new Monoid[String] {
    override def combine(a: String, b: String): String = a + b
    override def empty: String = ""
  }

  val salaryAddSemigroup = new Semigroup[Salary] {
    override def combine(a: Salary, b: Salary): Salary = {
      Salary(credits = toUsd(a.credits, a.currency) + toUsd(b.credits, b.currency), currency = "USD")
    }

    def toUsd(credits: Int, currency: String): Int = {
      credits
    }
  }

  implicit val salaryAddMonoid = new Monoid[Salary] {
    override def combine(a: Salary, b: Salary): Salary = {
      Salary(credits = toUsd(a.credits, a.currency) + toUsd(b.credits, b.currency), currency = "USD")
    }

    def toUsd(credits: Int, currency: String): Int = {
      credits
    }

    override def empty: Salary = Salary(0, "USD")
  }

  implicit def optMonoid[T](implicit m: Monoid[T]): Monoid[Option[T]] = new Monoid[Option[T]] {
    override def empty: Option[T] = None
    override def combine(aOpt: Option[T], bOpt: Option[T]): Option[T] = {
      (aOpt, bOpt) match {
        case (Some(a), Some(b)) => Some(m.combine(a, b))
        case (Some(a), None)    => Some(a)
        case (None, Some(b))    => Some(b)
        case (None, None)       => None
      }
    }
  }

  def combineList[T](list: List[T])(implicit m: Monoid[T]): T = {
    list.foldLeft(m.empty)(m.combine)
  }

  println(combineList(List(1, 2, 5))(addIntMonoid))
  println(combineList(List("heelo", "world")))
  println(combineList(List(Salary(5, "USD"), Salary(1500, "EUR"))))

  println(combineList(List(Some(1), None, Some(2), Some(5))))

  println(combineList(List(Some("heelo"), None,  Some("world"))))
  println(combineList(List[Option[String]](None, None)))
  println(combineList(List(Option(Salary(5, "USD")), None, Option(Salary(1500, "EUR")))))
}

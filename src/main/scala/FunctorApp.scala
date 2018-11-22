import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

object FunctorApp extends App {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {

    /**
      * Allow use (syntax sugar [F[_] : Functor])
      *   def plusOne[F[_] : Functor](number: F[Int]): F[Int] = Functor[F].map(number)(_ + 1)
      * instead of
      *   def plusOne[F[_]](number: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(number)(_ + 1)
      */
    def apply[F[_]](implicit ft: Functor[F]): Functor[F] = ft
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit def futureFunctor(implicit ec: ExecutionContext): Functor[Future] = new Functor[Future] {
    override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  }

  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Node(value, leftNode, rightNode) => Node(f(value), map(leftNode)(f), map(rightNode)(f))
      case EmptyNode => EmptyNode
    }
  }

  /**
    * Implicit class for enrich container F with method '.map(...)'
    *
    * Now we can use
    *   Tree(1).map(_ * 2)
    * instead of
    *   Functor[Tree].map(Tree(1))(_ * 2)
    */
  implicit class FunctorEnrich[A, F[_]](val fa: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicit functor: Functor[F]): F[B] = functor.map(fa)(f)
  }

  case class OrderItem(name: String, price: Double)

  //  def withVat(item: Option[OrderItem]): Option[OrderItem] = item.map(i => i.copy(price = i.price * 1.2))
  //  def withVat(item: List[OrderItem]): List[OrderItem] = item.map(i => i.copy(price = i.price * 1.2))
  //  def withVat(item: Future[OrderItem])(implicit ec: ExecutionContext): Future[OrderItem] = item.map(i => i.copy(price = i.price * 1.2))

  def withVat[F[_] : Functor](item: F[OrderItem]): F[OrderItem] = Functor[F].map(item)(i => i.copy(price = i.price * 1.2))

  val item = OrderItem("MacBook", 2800)

  val optionWithVat = withVat[Option](Some(item))
  val listWithVat = withVat(List(item))
  val treeWithVat = withVat(Tree(item))

  println(optionWithVat)
  println(listWithVat)
  println(treeWithVat)

  //Use map for container Tree[T]
  val tree: Tree[Int] = Tree(1, Tree(2, Tree(4), Tree[Int]), Tree(3))
  println(tree)
  println(tree.map(_ * 3))

}

sealed trait Tree[+T]

case class Node[T](value: T, leftNode: Tree[T], rightNode: Tree[T]) extends Tree[T]

case object EmptyNode extends Tree[Nothing]

object Tree {
  def apply[T]: Tree[T] = EmptyNode

  def apply[T](value: T): Tree[T] = Node(value, Tree[T], Tree[T])

  def apply[T](value: T, leftNode: Tree[T], rightNode: Tree[T]): Tree[T] = Node(value, leftNode, rightNode)
}

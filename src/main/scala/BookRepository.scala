import cats.instances.future._
import cats.instances.list._
import cats.syntax.all._
import cats.{Id, Monad}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.higherKinds

case class Book(id: String, rank: Int = 0)

trait BookRepository[F[_]] {

  def create(book:Book): F[Unit]
  def getById(id: String): F[Option[Book]]
  def findAll: F[List[Book]]
  def delete(book: Book): F[Unit]

}

class BookRepositoryInMemory extends BookRepository[Id] {
  private var storage: Map[String, Book] = Map()

  override def create(book: Book): Id[Unit] = storage = storage + (book.id -> book)
  override def getById(id: String):Id[Option[Book]] = storage.get(id)
  override def findAll: Id[List[Book]] = storage.values.toList
  override def delete(book: Book): Id[Unit] = storage = storage - book.id
}

class BookRepositoryFuture extends BookRepository[Future] {
  private var storage: Map[String, Book] = Map()

  override def create(book: Book): Future[Unit] = Future.successful{storage = storage + (book.id -> book)}
  override def getById(id: String):Future[Option[Book]] = Future.successful{storage.get(id)}
  override def findAll: Future[List[Book]] = Future.successful{storage.values.toList}
  override def delete(book: Book): Future[Unit] = Future.successful{storage = storage - book.id}
}

class BookService[F[_] : Monad](repository: BookRepository[F]) {
  def setRank(book: Book, rank: Int): F[Unit] = {
    if (rank >= 0) {
      repository.getById(book.id).flatMap {
        case Some(b) =>
          for {
            _ <- repository.delete(b)
            _ <- repository.create(Book(b.id, rank))
          } yield ()
        case _ => Monad[F].unit // TODO: extend error reporting
      }
    }
    else Monad[F].unit // TODO: extend error reporting

  }
  def setRanks(books: List[Book], rank: Int): F[Unit] = books.map(setRank(_, rank)).sequence.map(_ => ())
  def create(book:Book): F[Unit] = repository.create(book)
  def getById(id: String): F[Option[Book]] = repository.getById(id: String)
  def findAll: F[List[Book]] = repository.findAll
  def delete(book: Book): F[Unit] = repository.delete(book)
}

object MonadApp extends App {
  val bookRepository = new BookRepositoryInMemory
  val bookService = new BookService[Id](bookRepository)

  val book1 = Book("Functional programming in Scala")
  val book2 = Book("Scala in depth")
  val book3 = Book("Scala for the impatient")

  bookService.create(book1)
  bookService.create(book2)
  bookService.create(book3)

  assert(bookService.findAll == Seq(book1, book2, book3))

  bookService.setRank(book2, 2)
  assert(bookService.getById(book2.id) == Some(Book(book2.id, 2)))

  bookService.delete(Book("Scala2", 2))
  assert(bookService.getById("Scala2") == None)

  val bookRepositoryF = new BookRepositoryFuture
  val bookServiceF = new BookService[Future](bookRepositoryF)

  def await[T](f: Future[T]) = Await.result(f, 2.seconds)

  await(bookServiceF.create(book1))
  await(bookServiceF.create(book2))
  await(bookServiceF.create(book3))

  assert(await(bookRepositoryF.findAll) == Seq(book1, book2, book3))

  await(bookServiceF.setRank(book2, 2))
  assert(await(bookServiceF.getById(book2.id)).contains(Book(book2.id, 2)))

  await(bookServiceF.delete(Book(book2.id, 2)))
  assert(await(bookServiceF.getById(book2.id)).isEmpty)

}
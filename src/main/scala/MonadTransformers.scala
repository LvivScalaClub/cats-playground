import cats.instances.future._
import cats.data.EitherT

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object MonadTransformers extends App {
  import Domain._

  sealed trait ApiError

  case class ResourceNotFound(msg: String) extends ApiError

  case class InvalidCredentials(msg: String) extends ApiError

  case class ApiUnavailable(msg: String) extends ApiError

  case class SerializationError(msg: String) extends ApiError

  case class ServiceBookingError(msg: String) extends ApiError

  object Domain {

    case class User(id: String, name: String)

    case class AuthToken(token: String, expiresAt: Long)

    case class Service(id: String, name: String)

    case class ServiceAvailability(id: String, available: Boolean)

    case class Booking(id: String, name: String, startTime: Long, endTime: Long)

  }

  trait AuthApi {
    def getAuthToken(username: String, password: String): Future[Either[ApiError, AuthToken]]
  }

  trait UserApi {
    def getUser(username: String, token: AuthToken): Future[Either[ApiError, User]]
  }

  trait ServiceApi {
    def getService(serviceName: String, token: AuthToken): Future[Either[ApiError, Service]]

    def getServiceAvailability(serviceId: String, from: Long, to: Long, token: AuthToken): Future[Either[ApiError, ServiceAvailability]]
  }

  trait BookingApi {
    def bookService(serviceId: String, userId: String, token: AuthToken): Future[Either[ApiError, Booking]]
  }

  class AuthApiImpl extends AuthApi {
    override def getAuthToken(username: String, password: String): Future[Either[ApiError, AuthToken]] =
      (username, password) match {
        case (u, p) if u == "petro" && p == "password" => Future.successful {
          Right(AuthToken("token", System.currentTimeMillis() + 3600))
        }
        case _ => Future.successful {
          Left(InvalidCredentials(s"User $username has provided invalid credentials"))
        }

      }
  }

  class UserApiImpl extends UserApi {
    override def getUser(username: String, token: AuthToken): Future[Either[ApiError, User]] = username match {
      case "petro" => Future.successful {
        Right(User("userid1", "Petro"))
      }
      case _ => Future.successful {
        Left(ResourceNotFound(s"$username not found"))
      }
    }
  }

  class ServiceApiImpl extends ServiceApi {
    override def getService(serviceName: String, token: AuthToken): Future[Either[ApiError, Service]] = serviceName match {
      case "service_a" => Future.successful {
        Right(Service("serviceid1", "Service 1"))
      }
      case _ => Future.successful {
        Left(ResourceNotFound(s"$serviceName not found"))
      }
    }
    override def getServiceAvailability(serviceId: String, from: Long, to: Long, token: AuthToken): Future[Either[ApiError, ServiceAvailability]] =
      (serviceId, from, to) match {
        case ("serviceid1", 8, 18) => Future.successful{
          Right(ServiceAvailability("serviceavailability1", available = true))
        }
        case ("serviceid1", _, _) => Future.successful{
          Right(ServiceAvailability("serviceavailability1", available = false))
        }
        case _ => Future.successful {
          Left(ResourceNotFound(s"$serviceId not found or not available"))
        }

      }
  }

  class BookingApiImpl extends BookingApi {
    def bookService(serviceId: String, userId: String, token: AuthToken): Future[Either[ApiError, Booking]] = serviceId match {
      case "serviceid1" => Future.successful {
        Right(Booking("bookingid1", "Booking 1", 8, 18))
      }
      case _ => Future.successful {
        Left(ResourceNotFound(s"$serviceId not found or not available"))
      }
    }
  }
  /**
    * goals:
    *    - implement *Api with stubs (memory objects)
    *      - service is available only from 8am to 6pm.
    *    - book a service (without MT)
    *      - in non-working hours
    *      - in working hours
    *    - book a service (using MT)
    *      - in non-working hours
    *      - in working hours
    *
    * note:
    *    - try different combinations: user doesn't exist, service doesn't exist, etc
    */

  val bookingApi = new BookingApiImpl
  val serviceApi = new ServiceApiImpl
  val userApi = new UserApiImpl
  val authApi = new AuthApiImpl

  val username = "petro"
  val password = "password"

  val from = 8
  val to = 18
  val serviceName = "service_a"

  type EitherTApiResp[T] = EitherT[Future, ApiError, T]

  val getAuthToken = EitherT(authApi.getAuthToken(username, password))

  val booking2F = for {
    token <- EitherT(authApi.getAuthToken(username, password))
    user <- EitherT(userApi.getUser(username, token))
    service <- EitherT(serviceApi.getService(serviceName, token))
    serviceAvailability <- EitherT(serviceApi.getServiceAvailability(service.id, from, to, token))
    booking <- if (serviceAvailability.available) {
      EitherT(bookingApi.bookService(service.id, user.id, token))
    } else EitherT(Future.successful[Either[ApiError, Booking]](Left(ServiceBookingError("Service unavailable"))))
  } yield booking


  val bookingF = for {
    token <- authApi.getAuthToken(username, password)
    user <- token match {
      case Right(t) => userApi.getUser(username, t)
      case Left(err) => Future.successful(Left(err))
    }
    service <- token match {
      case Right(t) => serviceApi.getService(serviceName, t)
      case Left(err) => Future.successful(Left(err))
    }
//    for comparision
//    isAvailable <- service match {
//      case Right(s) => token match {
//        case Right(t) => serviceApi.getServiceAvailability(s.id, from, to, t)
//        case Left(err) => Future.successful(Left(err))
//      }
//      case Left(err) => Future.successful(Left(err))
//    }

    isAvailable <- (service, token) match {
      case (Right(s), Right(t)) => serviceApi.getServiceAvailability(s.id, from, to, t)
      case (Left(err), _) => Future.successful(Left(err))
      case (_, Left(err)) => Future.successful(Left(err))
    }

    booking <- (isAvailable, token, user, service) match {
      case (Right(isAv), Right(t), Right(u), Right(s)) if isAv.available => bookingApi.bookService(s.id, u.id, t)
      case (Right(isAv), Right(t), Right(u), Right(s)) if !isAv.available => Future.successful(Left(ServiceBookingError("Service unavailable")))
      case (Left(err), _, _, _) => Future.successful(Left(err))
      case (_, Left(err), _, _) => Future.successful(Left(err))
      case (_, _, Left(err), _) => Future.successful(Left(err))
      case (_, _, _, Left(err)) => Future.successful(Left(err))
    }

  } yield booking

  println("bookingF")
  println(Await.result(bookingF, 1.second))
  println()

  println("booking2F")
  println(Await.result(booking2F.value, 1.second))
}

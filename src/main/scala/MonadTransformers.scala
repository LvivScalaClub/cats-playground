import MonadTransformers.ApiError
import MonadTransformers.Domain._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

object MonadTransformers extends App {

  sealed trait ApiError

  case class ResourceNotFound(msg: String) extends ApiError

  case class InvalidCredentials(msg: String) extends ApiError

  case class ApiUnavailable(msg: String) extends ApiError

  case class SerializationError(msg: String) extends ApiError

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

  for {
    token <- authApi.getAuthToken(username, password)
    user <- token match {
      case Right(t) => userApi.getUser(username, t)
      case Left(err) => Future.successful(Left(err))
    }
    service <- token match {
      case Right(t) => serviceApi.getService("test", t)
      case Left(err) => Future.successful(Left(err))
    }
  } yield ()
}

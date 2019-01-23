import MonadTransformers.Domain._

import scala.concurrent.Future

object MonadTransformers {
  sealed trait ApiError

  case class ResourceNotFound(msg: String) extends ApiError
  case class ApiUnavailable(msg: String) extends ApiError
  case class SerializationError(msg: String) extends ApiError

  object Domain {
    case class User(id: String, name: String)
    case class UserCredentials(id: String, userId: String, username: String, password: String)
    case class AuthToken(token: String, expiresAt: Long)
    case class Service(id: String, name: String)
    case class ServiceAvailability(id: String, available: Boolean)
    case class Booking(id: String, name: String, startTime: Long, endTime: Long)
  }

  trait AuthApi {
    def getAuthToken(username: String, password: String): Future[Either[ApiError, User]]
  }

  trait UserApi {
    def getUser(username: String): Future[Either[ApiError, User]]
    def getUserCredentials(userId: String): Future[Either[ApiError, UserCredentials]]
  }

  trait ServiceApi {
    def getService(serviceName: String): Future[Either[ApiError, Service]]
    def getServiceAvailability(serviceId: String, from: Long, to: Long): Future[Either[ApiError, ServiceAvailability]]
  }

  trait BookingApi {
    def bookService(serviceId: String): Future[Either[ApiError, Booking]]
  }


  def main(args: Array[String]): Unit = {
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

  }
}

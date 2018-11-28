
object ApplicativeApp extends App {
  // to access ValidatedNel and .toValidNel
  import cats.data._
  import cats.syntax.validated._
  import cats.syntax.apply._

  // you need the alias in order to have mapN accessible
  type ValidationResult[T] = ValidatedNel[String, T]

  val config: Map[String, Any] = Map(
    "email" -> "john@g.com",
    "age" -> 18
  )

  val invalidConfig: Map[String, Any] = Map(
    "email" -> "john@",
    "age" -> -1
  )

  case class AppConfig(email: String, age: Int)

  def validateEmailEither(email: Option[Any]): Either[String, String] = {
    email match {
      case Some(e: String) if isValidEmail(e) => Right(e)
      case Some(a: String) => Left("Invalid email")
      case Some(a) => Left("Email should be string")
      case None => Left("Email should be specified")
    }
  }

  def validateAgeEither(age: Option[Any]): Either[String, Int] = {
    age match {
      case Some(a: Int) if a >= 0 => Right(a)
      case Some(_: Int) => Left("Age couldn't be negative")
      case Some(_) => Left("Age should be integer")
      case None => Left("Age should be specified")
    }
  }

  def validateConfigEither(config: Map[String, Any]): Either[String, AppConfig] = {
    /*for {
      email <- validateEmail(config.get("email"))
      age <- validateAge(config.get("age"))
    } yield AppConfig(email, age)*/

    val validatedAge = validateAgeEither(config.get("age"))
    val validatedEmail = validateEmailEither(config.get("email"))

    val validated = List(validatedAge, validatedEmail)

    if (validated.forall(_.isRight)) {
      Right(AppConfig(
        validatedEmail.right.get,
        validatedAge.right.get
      ))
    } else {
      Left(validated.filter(_.isLeft).map(_.left.get).mkString("\n"))
    }
  }

  def validateEmail(email: Option[Any]): ValidationResult[String] = {
    email match {
      case Some(e: String) if isValidEmail(e) => e.validNel
      case Some(a: String) => "Invalid email".invalidNel
      case Some(a) => "Email should be string".invalidNel
      case None => "Email should be specified".invalidNel
    }
  }

  def isValidEmail(email: String): Boolean = {
    email.nonEmpty && email.contains('@') && !email.endsWith("@")
  }

  def validateAge(age: Option[Any]): ValidationResult[Int] = {
    age match {
      case Some(a: Int) if a >= 0 => Validated.validNel(a)
      case Some(_: Int) => "Age couldn't be negative".invalidNel
      case Some(_) => "Age should be integer".invalidNel
      case None => Validated.invalidNel("Age should be specified")
    }
  }

  def validateConfig(config: Map[String, Any]): ValidationResult[AppConfig] = {
    val validatedAge = validateAge(config.get("age"))
    val validatedEmail = validateEmail(config.get("email"))

    (validatedEmail, validatedAge).mapN(AppConfig)
  }


  println(validateAgeEither(Some("12")))
  println(validateAgeEither(Some(12)))

  println(validateAge(Some("12")))
  println(validateAge(Some(12)))

  println(validateConfig(config))
  println(validateConfig(invalidConfig))
}

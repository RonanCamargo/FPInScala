package ronancamargo.fp.either

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

case class Left[+E](error: E) extends Either[E, Nothing] {
  override def map[B](f: Nothing => B): Either[E, B] = Left(error)
  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = Left(error)
  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b
  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = Left(error)
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))
  override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = f(value)
  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = this
  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    b match {
      case Right(bValue) => Right(f(value, bValue))
      case Left(e) => Left(e)
    }
  }
}

object Try {
  def apply[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }
}

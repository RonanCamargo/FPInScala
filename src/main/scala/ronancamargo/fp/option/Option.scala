package ronancamargo.fp.option

import ronancamargo.fp.list.{List, Nil}

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
  def isEmpty: Boolean
}

case class Some[A](get: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Some(f(get))
  override def flatMap[B](f: A => Option[B]): Option[B] = f(get)
  override def getOrElse[B >: A](default: => B): B = get
  override def orElse[B >: A](ob: => Option[B]): Option[B] = this
  override def filter(f: A => Boolean): Option[A] = if(f(get)) this else None
  override def isEmpty: Boolean = false
}

case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = None
  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None
  override def getOrElse[B >: Nothing](default: => B): B = default
  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
  override def filter(f: Nothing => Boolean): Option[Nothing] = None
  override def isEmpty: Boolean = true
}

object Option {
  def apply[A](option: A): Option[A] = if (option != null) Some(option) else None

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (None, _) | (_, None) => None
      case (Some(x), Some(y)) => Option(f(x, y))
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    val isNone = List.exists(a)(_.isEmpty) || a.equals(Nil)

    if (isNone) None
    else Some(List.map(a) {
        case Some(get) => get
      })
  }
}
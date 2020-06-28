package ronancamargo.fp.list

sealed trait List[+A]

case object Nil extends List[Nothing]
case class NonEmpty[A] (head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if(as.isEmpty) Nil
    else NonEmpty(as.head, apply(as.tail: _*))
  }

  def fill[A](elem: A, n: Int): List[A] = {
    n match {
      case 0 => Nil
      case 1 => NonEmpty(elem, Nil)
      case _ => NonEmpty(elem, fill(elem, n - 1))
    }
  }

  def tail[A](list: List[A]): List[A] = {
    list match {
      case NonEmpty(_, tail) => tail
      case _ => list
    }
  }

  @scala.annotation.tailrec
  def drop[A](n: Int, list: List[A]): List[A] = {
    n match {
      case x if x > 1 => drop(x-1, tail(list))
      case 1 => tail(list)
      case 0 => Nil
    }
  }

  @scala.annotation.tailrec
  def uncurriedDropWhile[A](list: List[A], f: A => Boolean): List[A] = {
    list match {
      case NonEmpty(h, t) if f(h) => uncurriedDropWhile(t, f)
      case _ => list
    }
  }

  @scala.annotation.tailrec
  def dropWhile[A](list: List[A])(f: A => Boolean): List[A] = {
    list match {
      case NonEmpty(h, t) if f(h) => dropWhile(t)(f)
      case _ => list
    }
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = {
    l1 match {
      case Nil => l2
      case NonEmpty(h, t) => NonEmpty(h, append(t, l2))
    }
  }

}
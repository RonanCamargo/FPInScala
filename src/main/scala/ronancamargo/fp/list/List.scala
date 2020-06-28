package ronancamargo.fp.list

sealed trait List[+A]

case object Nil extends List[Nothing]
case class NonEmpty[A] (head: A, tail: List[A]) extends List[A]

object List {

  def apply[A] (as: A*): List[A] = {
    if(as.isEmpty) Nil
    else NonEmpty(as.head, apply(as.tail: _*))
  }
}
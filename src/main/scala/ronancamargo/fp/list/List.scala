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

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    l match {
      case Nil => Nil
      case NonEmpty(h, t) => NonEmpty(f(h), map(t)(f))
    }
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case NonEmpty(h, t) if f(h) => NonEmpty(h, filter(t)(f))
      case NonEmpty(_, t) => filter(t)(f)
    }
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    l match {
      case Nil => Nil
      case NonEmpty(h, t) => append(f(h), flatMap(t)(f))
    }
  }

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] = {
    l1 match {
      case Nil => Nil
      case NonEmpty(h, t) =>
        l2 match {
          case NonEmpty(h2, t2) => NonEmpty(f(h,h2), zipWith(t,t2)(f))
        }
    }
  }

  @scala.annotation.tailrec
  def foldLeft[A,B](l: List[A], seed: B)(f: (B, A) => B): B = {
    l match {
      case Nil => seed
      case NonEmpty(h, t) => foldLeft(t, f(seed,h))(f)
    }
  }

  def length[A](l: List[A]): Int = {
    foldLeft(l,0){(x,_) => x + 1}
  }

  def forall[A](l: List[A])(f: A => Boolean): Boolean = {
    foldLeft(l, true)((x,y) => x && f(y))
  }

  @scala.annotation.tailrec
  def exists[A](l: List[A])(f: A => Boolean): Boolean = {
    l match {
      case Nil => false
      case NonEmpty(h, _) if f(h) => true
      case NonEmpty(_, t) => exists(t)(f)
    }
  }

  //TODO
  def scanLeft[A,B](l: List[A], z: B)(f: (B,A) => B): List[B] = {
    Nil
  }
}
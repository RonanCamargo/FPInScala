package ronancamargo.fp.stream


sealed trait Stream[+A] {
  def toList: List[A]

  def take(n: Int): Stream[A]

  def drop(n: Int): Stream[A]

  def takeWhile(p: A => Boolean): Stream[A]

  def dropWhile(p: A => Boolean): Stream[A]
}

case object Empty extends Stream[Nothing] {
  override def toList: List[Nothing] = Nil

  override def take(n: Int): Stream[Nothing] = Empty

  override def drop(n: Int): Stream[Nothing] = Empty

  override def takeWhile(p: Nothing => Boolean): Stream[Nothing] = Empty

  override def dropWhile(p: Nothing => Boolean): Stream[Nothing] = Empty
}

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A] {

  override def toList: List[A] = {
    head() :: tail().toList
  }

  override def take(n: Int): Stream[A] = {
    if (n > 0) Cons(head, () => tail().take(n - 1))
    else Empty
  }

  override def drop(n: Int): Stream[A] = {
    if (n > 0) tail().drop(n - 1)
    else Cons(head, tail)
  }

  override def takeWhile(p: A => Boolean): Stream[A] = {
    lazy val h = head()
    if (p(h)) Cons(head, () => tail().takeWhile(p))
    else Empty
  }

  override def dropWhile(p: A => Boolean): Stream[A] = {
    lazy val eval = p(head())
    if (eval) tail().dropWhile(p)
    else Cons(head, tail)
  }
}


object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](elems: A*): Stream[A] = {
    if (elems.isEmpty) empty
    else cons(elems.head, apply(elems.tail: _*))
  }
}
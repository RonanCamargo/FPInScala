package ronancamargo.fp.stream

sealed trait Stream[+A] {
  def toList: List[A]

  def isEmpty: Boolean

  def take(n: Int): Stream[A]

  def drop(n: Int): Stream[A]

  def takeWhile(p: A => Boolean): Stream[A]

  def dropWhile(p: A => Boolean): Stream[A]

  def exists(p: A => Boolean): Boolean

  def forAll(p: A => Boolean): Boolean

  def foldRight[B](z: => B)(f: (A, => B) => B): B

  def foldLeft[B](z: => B)(f: (A, => B) => B): B

  def map[B](f: A => B): Stream[B]

  def flatMap[B](f: A => Stream[B]): Stream[B]

  def filter(p: => A => Boolean): Stream[A]

  def headOption: Option[A]

  def append[B >: A](other: => Stream[B]): Stream[B]

}

case object Empty extends Stream[Nothing] {
  override def toList: List[Nothing] = Nil
  override def take(n: Int): Stream[Nothing] = Empty
  override def drop(n: Int): Stream[Nothing] = Empty
  override def takeWhile(p: Nothing => Boolean): Stream[Nothing] = Empty
  override def dropWhile(p: Nothing => Boolean): Stream[Nothing] = Empty
  override def exists(p: Nothing => Boolean): Boolean = false
  override def forAll(p: Nothing => Boolean): Boolean = false
  override def foldRight[B](z: => B)(f: (Nothing, => B) => B): B = z
  override def foldLeft[B](z: => B)(f: (Nothing, => B) => B): B = z
  override def map[B](f: Nothing => B): Stream[B] = Empty
  override def flatMap[B](f: Nothing => Stream[B]): Stream[B] = Empty
  override def filter(p: => Nothing => Boolean): Stream[Nothing] = Empty
  override def headOption: Option[Nothing] = None

  override def append[B >: Nothing](other: => Stream[B]): Stream[B] = other

  override def isEmpty: Boolean = true
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
    lazy val eval = p(head())
    if (eval) Cons(head, () => tail().takeWhile(p))
    else Empty
  }

  override def dropWhile(p: A => Boolean): Stream[A] = {
    lazy val eval = p(head())
    if (eval) tail().dropWhile(p)
    else Cons(head, tail)
  }

  override def exists(p: A => Boolean): Boolean = p(head()) || tail().exists(p)

  override def forAll(p: A => Boolean): Boolean = p(head()) && tail().forAll(p)

  override def foldRight[B](z: => B)(f: (A, => B) => B): B = f(head(), tail().foldRight(z)(f))

  override def foldLeft[B](z: => B)(f: (A, => B) => B): B = tail().foldLeft(f(head(), z))(f)

  override def map[B](f: A => B): Stream[B] = Stream.cons(f(head()), tail().map(f))

  override def flatMap[B](f: A => Stream[B]): Stream[B] = ???

  override def filter(p: => A => Boolean): Stream[A] = {
    lazy val h = head()
    if(p(h)) Stream.cons(h, tail().filter(p))
    else tail().filter(p)
  }

  override def headOption: Option[A] = ???

  override def isEmpty: Boolean = false

  override def append[B >: A](other: => Stream[B]): Stream[B] = {
    Stream.cons(head(), tail().append(other))
  }
}

object Stream {

  def apply[A](elems: A*): Stream[A] = {
    if (elems.isEmpty) empty
    else cons(elems.head, apply(elems.tail: _*))
  }

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def repeat[A](elem: A, n: Int): Stream[A] = {
    if(n > 0) cons(elem, repeat(elem, n - 1))
    else empty
  }

}
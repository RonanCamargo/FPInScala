package ronancamargo.fp.helper

object FunctionHelper {

  def flip[A,B,C](f: (A,B) => C): (B,A) => C = {
    (b: B, a: A) => f(a,b)
  }

  def curry[A,B,C](f: (A,B) => C): A => B => C = {
    (a: A) => (b: B) => f(a,b)
  }

  def partialApply[A,B,C](f: A => B => C)(a: A): B => C = {
    (b: B) => f(a)(b)
  }
}

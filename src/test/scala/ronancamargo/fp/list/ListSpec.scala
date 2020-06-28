package ronancamargo.fp.list

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ronancamargo.fp.list.List._

class ListSpec extends AnyFlatSpec
  with Matchers{

  behavior of "List"

  it should "be Nil" in {
    List() should be(Nil)
  }

  it should "be NonEmpty" in {
    List(1,2,3) should be(NonEmpty(1, NonEmpty(2, NonEmpty(3, Nil))))
  }

  it should "fill with n elements" in {
    val filled = fill("a", 3)
    filled should be(List("a","a","a"))
  }

  it should "drop the first n elements" in {
    val dropped = drop(3, fill('A', 5))
    dropped should be(List('A','A'))
  }

  it should "drop while f result is true, uncurried" in {
    val dropped = uncurriedDropWhile(List(1,3,5,10,11), (x: Int) => x < 10)
    dropped should be(List(10,11))
  }

  it should "drop while f result is true" in {
    val dropped = dropWhile(List(1,3,5,10,11))(x => x < 10)
    dropped should be(List(10,11))
  }


  it should "append two lists" in {
    val appended = append(List(1,2), List(3,4,5))
    appended should be(List(1,2,3,4,5))
  }

}

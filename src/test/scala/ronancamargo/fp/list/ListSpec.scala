package ronancamargo.fp.list

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListSpec extends AnyFlatSpec
  with Matchers{

  behavior of "List"

  it should "be Nil" in {
    List() should be(Nil)
  }

  it should "be NonEmpty" in {
    List(1,2,3) should be(NonEmpty(1, NonEmpty(2, NonEmpty(3, Nil))))
  }

}

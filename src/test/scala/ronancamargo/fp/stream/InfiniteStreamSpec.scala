package ronancamargo.fp.stream

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InfiniteStreamSpec extends AnyFlatSpec
  with Matchers {

  val ones: Stream[Int] = Stream.cons(1, ones)
  val threeOnes = List(1, 1, 1)

  behavior of "Stream"

  it should "contain 1" in {
    ones.exists(_ == 1) should be(true)
  }

  it should "take 3 elements" in {
    ones.take(3).toList should be(threeOnes)
  }

}

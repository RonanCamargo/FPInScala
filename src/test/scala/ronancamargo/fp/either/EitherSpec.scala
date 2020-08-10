package ronancamargo.fp.either

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EitherSpec extends AnyFlatSpec
  with Matchers{

  behavior of "Either"

  it should "be a Right" in {
    Try(1) should be(Right(1))
  }

  "map" should "return a Right" in {
    val right = Right(5).map(_ * 2)
    right should be(Right(10))
  }

  it should "return a Left"

  "flatMap" should "return a Right" in {
    val right = Right(5).flatMap(x => Right(x + 5))
    right should be(Right(10))
  }

  it should "return a Left"

  "orElse" should "return a Right" in {

  }

  it should "return a Left"
}

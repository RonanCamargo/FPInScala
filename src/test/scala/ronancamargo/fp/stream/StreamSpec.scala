package ronancamargo.fp.stream

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StreamSpec extends AnyFlatSpec
  with Matchers {

  val shortStream: Stream[Int] = Stream(1, 2, 3)
  val longStream: Stream[Int] = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9)
  val shortList = List(1, 2, 3)


  behavior of "Stream"

  it should "be converted to Scala List" in {
    shortStream.toList should be(shortList)
  }

  behavior of "Takeable Stream"

  it should "take 3 elements" in {
    longStream.take(3).toList should be(shortStream.toList)
  }

  it should "take while numbers are less than or equals to 3" in {
    longStream.takeWhile(_ <= 3).toList should be(shortList)
  }

  behavior of "Droppable Stream"

  it should "drop 2 elements" in {
    shortStream.drop(2).toList should be(List(3))
  }

  it should "drop all elements" in {
    shortStream.drop(3).toList should be(Nil)
  }

  it should "drop while numbers are less than 9" in {
    longStream.dropWhile(_ < 9).toList should be(List(9))
  }

  it should "not match condition and drop all elements" in {
    longStream.dropWhile(_ < 20).toList should be(Nil)
  }

}

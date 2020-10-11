package ronancamargo.fp.stream

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StreamSpec extends AnyFlatSpec
  with Matchers {

  private val shortStream: Stream[Int] = Stream(1, 2, 3)
  private val longStream: Stream[Int] = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9)
  private val shortList = List(1, 2, 3)
  private val onesStream = Stream(1, 1, 1, 1)
  private val onesList = List(1, 1, 1, 1)

  behavior of "Stream"

  it should "be converted to Scala List" in {
    shortStream.toList shouldBe shortList
  }

  it should "contain 5" in {
    longStream.exists(_ == 5) shouldBe true
  }

  it should "not contain 20" in {
    longStream.exists(_ == 20) shouldBe false
  }

  it should "append another Stream" in {
    val ones = Stream.repeat(1, 2)
    ones.append(ones).toList shouldBe onesList
  }

  behavior of "Stream functions"

  it should "generate ones Stream" in {
    Stream.repeat(1, 4).toList shouldBe onesList
  }

  behavior of "Stream higher order functions"

  it should "sum 4 with foldRight" in {
    onesStream.foldRight(0)(_ + _) shouldBe 4
  }

  it should "sum 4 with foldLeft" in {
    onesStream.foldLeft(0)(_ + _) shouldBe 4
  }

  it should "map into Stream of strings" in {
    onesStream.map(_.toString).toList shouldBe List("1", "1", "1", "1")
  }

  it should "filter and result an empty Stream" in {
    onesStream.filter(_ != 1) shouldBe Empty
  }


  behavior of "Takeable Stream"

  it should "take 3 elements" in {
    longStream.take(3).toList shouldBe shortStream.toList
  }

  it should "take while numbers are less than or equals to 3" in {
    longStream.takeWhile(_ <= 3).toList shouldBe shortList
  }

  behavior of "Droppable Stream"

  it should "drop 2 elements" in {
    shortStream.drop(2).toList shouldBe List(3)
  }

  it should "drop all elements" in {
    shortStream.drop(3).toList shouldBe Nil
  }

  it should "drop while numbers are less than 9" in {
    longStream.dropWhile(_ < 9).toList shouldBe List(9)
  }

  it should "not match condition and drop all elements" in {
    longStream.dropWhile(_ < 20).toList shouldBe Nil
  }

}

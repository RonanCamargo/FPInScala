package ronancamargo.fp.option

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ronancamargo.fp.list._


class OptionSpec extends AnyFlatSpec
  with Matchers{

  behavior of "Option constructor"

  it should "create Some" in {
    Option(3) should be(Some(3))
  }

  it should "create None" in {
    Option(null) should be(None)
  }

  behavior of "Some methods"

  it should "map into another Some" in {
    Option(5).map(_ * 5) should be(Some(25))
  }

  it should "flatMap into another Some" in {
    Option(5).flatMap(o => Option(o * 5)) should be(Some(25))
  }

  it should "return the wrapped value" in {
    Option(5).getOrElse(15) should be(5)
  }

  it should "filter and return Some" in {
    Option(5).filter(_ == 5) should be(Some(5))
  }

  it should "filter and return None" in {
    Option(5).filter(_ > 10) should be(None)
  }

  it should "return itself" in {
    Option(5).orElse(Option(4)) should be(Option(5))
  }


  behavior of "None methods"

  val noneOption: Option[Int] = None

  it should "map into None" in {
    noneOption.map(_ > 3) should be(None)
  }

  it should "flatMap into None" in {
    noneOption.flatMap(x => Option(x + 5)) should be(None)
  }

  it should "return a default value" in {
    noneOption.getOrElse(5) should be(5)
  }

  it should "filter and return None" in {
    noneOption.filter(_ > 5) should be(None)
  }

  it should "return alternative" in {
    noneOption.orElse(Option(5)) should be(Some(5))
  }

  behavior of "Sequence function"

  it should "return None" in {
    Option.sequence(List(Some(1),None)) should be(None)
  }

  it should "turn a List of Option into a Option of a List" in {
    Option.sequence(List(Some(1),Some(2),Some(3))) should be(Option(List(1,2,3)))
  }



}

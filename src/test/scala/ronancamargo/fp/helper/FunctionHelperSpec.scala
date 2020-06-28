package ronancamargo.fp.helper

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ronancamargo.fp.helper.FunctionHelper._
import ronancamargo.fp.list.List
import ronancamargo.fp.list.List._


class FunctionHelperSpec extends AnyFlatSpec
  with Matchers{

  "Flip" should "swap function parameters" in {
    val list = List(1,2,3)
    val f = (x: Int) => x*10

    val mapFn: (List[Int],Int=>Int) => List[Int] = map(_)(_)
    val flippedMap = flip(mapFn)

    flippedMap(f,list) should be(mapFn(list,f))
  }

  "FunctionHelper" should "map and filter in a Haskell-like way" in {
    val list = List(1,2,3)
    val * = (x: Int) => (y: Int) => y * x
    val > = (x: Int) => (y: Int) => y > x

    val mapFn: (List[Int], Int => Int) => List[Int] = List.map(_)(_)
    val filterFn: (List[Int], Int => Boolean) => List[Int] = List.filter(_)(_)

    val flippedMap = flip(mapFn)
    val flippedFilter = flip(filterFn)

    val map = curry(flippedMap)
    val filter = curry(flippedFilter)

    /* Similar syntax to Haskell
    (map (* 10) . filter (> 2)) list
     */
    val result = (map(*(10)) compose filter(>(2)))(list)

    result should be(List(30))
  }

}

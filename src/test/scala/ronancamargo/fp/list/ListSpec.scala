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

  it should "map a list into another list type" in {
    val mapped = map(List(1,2,3))(x => x*10)
    mapped should be(List(10,20,30))
  }

  it should "filter a list" in {
    val filtered = filter(List(1,5,2,3,10))(x => x <=3)
    filtered should be(List(1,2,3))
  }

  it should "flat map a list" in {
    val flatMapped = flatMap(List(1,2,3))(x => List(x,x))
    flatMapped should be(List(1,1,2,2,3,3))
  }

  it should "filter using flatMap" in {
    val fm = flatMap(List(1,2,3)){x =>
      if(x != 2) List(x)
      else Nil
    }
    fm should be(List(1,3))
  }

  it should "zip two lists with a function" in {
    val zipped = zipWith(List("a","b","c"), List(2,3,2)){
      (x,y) => fill(x,y)
    }
    zipped should be(List(List("a","a"), List("b","b","b"), List("c","c")))
  }

  "foldLeft" should "calculate list size" in {
    val size = foldLeft(List(1,2,3),0)((x,_) => x + 1)
    size should be(3)
  }

  it should "implement exists and be true" in {
    val exists = (l: List[Int],f: Int => Boolean) => foldLeft(l, false){(x,y) => x || f(y)}
    exists(List(1,2,3), x => x == 2) should be(true)
  }

  "forall" should "be true" in {
    forall(List(1,2,3))(x => x < 5) should be(true)
  }

  "exists" should "be true" in {
    exists(List(1,2,3))(x => x == 2) should be(true)
  }

  it should "be false" in {
    exists(List(1,2,3))(x => x == 5) should be(false)
  }

  it should "be false when List is Nil" in {
    exists(Nil)(_: Any => true) should be(false)
  }

  "from" should "generate numbers from 0 to 5" in {
    from(0)(5) should be(List(0,1,2,3,4,5))
  }

  "scanLeft" should "generate the first n factorials" in {
    val to: Int => List[Int] = from(1)
    scanLeft(to(5), 1)(_ * _) should be(List(1,1,2,6,24,120))
  }

  it should "return List(seed) when list is Nil" in {
    scanLeft(List[Int](), 10)(_ * _) should be(List(10))
  }

}

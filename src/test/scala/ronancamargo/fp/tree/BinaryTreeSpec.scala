package ronancamargo.fp.tree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BinaryTreeSpec extends AnyFlatSpec
  with Matchers{

  behavior of "Binary Tree size"

  it should "be 3 elements" in {
    val tree = BinaryTree(Leaf(2), Leaf(1))
    BinaryTree.size(tree) should be(3)
  }

  it should "be 5 elements" in {
    val tree = BinaryTree(left = Leaf(2), right = SubTree(2,3))
    BinaryTree.size(tree) should be(5)
  }

  it should "be 7 elements" in {
    val tree = BinaryTree(left = SubTree(1,1), right = SubTree(2,2))
    BinaryTree.size(tree) should be(7)
  }

  behavior of "Binary Tree max int value"

  it should "be 5" in {
    val tree = BinaryTree(left = Leaf(2), right = SubTree(5,3))
    BinaryTree.max(tree) should be(5)
  }

  behavior of "Binary Tree depth"

  it should "be 0 when it is only a root element" in {
    val tree = Leaf(2)
    BinaryTree.depth(tree) should be(0)
  }

  it should "be 1" in {
    val tree = BinaryTree(Leaf(2), Leaf(1))
    BinaryTree.depth(tree) should be(1)
  }

  it should "be 2" in {
    val tree = BinaryTree(left = Leaf(2), right = SubTree(5,3))
    BinaryTree.depth(tree) should be(2)
  }

  it should "be 3" in {
    val tree = BinaryTree(left = Leaf(2), right = SubTree(Leaf(2), SubTree(2,2)))
    BinaryTree.depth(tree) should be(3)
  }

  behavior of "Binary Tree map"

  it should "convert an Int tree into a String one" in {
    val tree = BinaryTree(left = Leaf(2), right = SubTree(5,3))
    BinaryTree.map(tree)(_.toString) should be(BinaryTree(left = Leaf("2"), right = SubTree("5","3")))
  }

}

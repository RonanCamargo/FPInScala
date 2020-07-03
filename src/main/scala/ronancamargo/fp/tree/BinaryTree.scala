package ronancamargo.fp.tree

sealed trait BinaryTree[+A]

case class Leaf[A] (a: A) extends BinaryTree[A]
case class SubTree[A](left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

object SubTree{

  def apply[A](leftLeaf: A, rightLeaf: A): SubTree[A] = {
    SubTree(Leaf(leftLeaf),Leaf(rightLeaf))
  }
}

object BinaryTree {

  def apply[A](left: BinaryTree[A], right: BinaryTree[A]): BinaryTree[A] = {
    SubTree(left,right)
  }

  def apply[A](leaf: A): BinaryTree[A] = {
    Leaf(leaf)
  }

  def size[A](tree: BinaryTree[A]): Int = {
    def accumulator[B](tree: BinaryTree[B], count: Int): Int = {
      tree match {
        case Leaf(_) => count + 1
        case SubTree(left, right) => accumulator(left, count) + accumulator(right, count) + 1
      }
    }
    accumulator(tree,0)
  }

  def max(tree: BinaryTree[Int]): Int = {
    def maxElem(tree: BinaryTree[Int], partialMax: Int): Int = {
      tree match {
        case Leaf(value) => partialMax max value
        case SubTree(left, right) => maxElem(left, partialMax) max maxElem(right, partialMax)
      }
    }
    maxElem(tree, 0)
  }

  def depth[A](tree: BinaryTree[A]): Int = {
    def maxDepth(tree: BinaryTree[A], partialMax: Int): Int = {
      tree match {
        case Leaf(_) => partialMax
        case SubTree(left, right) => maxDepth(left, partialMax + 1) max maxDepth(right, partialMax + 1)
      }
    }
    maxDepth(tree, 0)
  }

  def map[A,B](tree: BinaryTree[A])(f: A => B): BinaryTree[B] = {
    tree match {
      case Leaf(value) => Leaf(f(value))
      case SubTree(left, right) =>
        SubTree(
          map(left)(f),
          map(right)(f)
        )
    }
  }

}
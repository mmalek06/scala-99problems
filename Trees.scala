package NinetyNineProblems

import scala.Predef._

object Trees extends App {

  sealed abstract class Tree[+T] {
    def addValue[U >: T](x: U)(implicit ev: U => Ordered[U]): Tree[U]
    def isMirrorOf[V](tree: Tree[V]): Boolean
    def isSymmetric: Boolean
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = s"T(${value.toString} ${left.toString} ${right.toString})"

    def addValue[U >: T](x: U)(implicit p: U => Ordered[U]): Tree[U] =
      if (x == value) this
      else if (x > value) Node(value, left, right.addValue(x))
      else Node(value, left.addValue(x), right)

    def isMirrorOf[V](tree: Tree[V]): Boolean = tree match {
      case t: Node[V] => left.isMirrorOf(t.right) && right.isMirrorOf(t.left)
      case _          => false
    }

    def isSymmetric: Boolean = left.isMirrorOf(right)
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  case object End extends Tree[Nothing] {
    override def toString = "."

    def addValue[U >: Nothing](x: U)(implicit p: U => Ordered[U]): Tree[U] = Node(x, End, End)
    def isMirrorOf[V](tree: Tree[V]): Boolean = tree == End
    def isSymmetric: Boolean = true
  }

  object Tree {
    def balancedAllPermutations[T](nodes: Int, value: T): List[Tree[T]] = nodes match {
      case n if n < 1 => List(End)
      case n if n % 2 == 1 =>
        val subtrees = balancedAllPermutations(n / 2, value)

        subtrees.flatMap(l => subtrees.map(r => Node(value, l, r)))
      case n if n % 2 == 0 =>
        val lesserSubtrees = balancedAllPermutations((n - 1) / 2, value)
        val greaterSubtrees = balancedAllPermutations((n - 1) / 2 + 1, value)

        lesserSubtrees.flatMap(l => greaterSubtrees.flatMap(g => List(Node(value, l, g), Node(value, g, l))))
    }

    def balancedSymmetricAllPermutations[T](nodes: Int, value: T): List[Tree[T]] =
      balancedAllPermutations(nodes, value).filter(_.isSymmetric)

    def balanced[T](nodes: Int, value: T): Tree[T] = nodes match {
      case n if n < 1 => End
      case n if n % 2 == 1 => Node(value, balanced(n / 2, value), balanced(n / 2, value))
      case n if n % 2 == 0 =>
        val lesserSubtree = balanced((n - 1) / 2, value)
        val greaterSubtree = balanced((n - 1) / 2 + 1, value)

        Node(value, lesserSubtree, greaterSubtree)
    }

    def balancedSymmetric[T](nodes: Int, value: T, swapSubtrees: Boolean = false): Tree[T] = nodes match {
      case n if n < 1 => End
      case n if n % 2 == 1 => Node(value, balancedSymmetric(n / 2, value), balancedSymmetric(n / 2, value, swapSubtrees = true))
      case n if n % 2 == 0 =>
        val lesserSubtree = balancedSymmetric((n - 1) / 2, value, swapSubtrees)
        val greaterSubtree = balancedSymmetric((n - 1) / 2 + 1, value, swapSubtrees)

        if (!swapSubtrees) Node(value, lesserSubtree, greaterSubtree)
        else Node(value, greaterSubtree, lesserSubtree)
    }

    def bst[T](l: List[T]): Tree[T] =
      if (l.isEmpty) End
      else {
        implicit val orderable: T => Ordered[T] = {
          case value: Int => intWrapper(value).asInstanceOf[Ordered[T]]
        }
        val tree = End.addValue(l.head)

        l.tail.foldLeft(tree) {
          (acc, curr) => acc.addValue(curr)
        }
      }
  }

  (5 to 1000).foreach(n => println(s"$n : ${Tree.balancedSymmetric(n, "x").isSymmetric}"))
}

package com.github.interaction.s99

object binarytree{
  sealed abstract class Tree[+T]{
    def isMirrorOf[V](tree: Tree[V]): Boolean
    def isSymmetric: Boolean
  }
  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

    override def isMirrorOf[V](tree: Tree[V]): Boolean = tree match {
      case t: Node[V] => left.isMirrorOf(t.right) && right.isMirrorOf(t.left)

      case _ => false
    }

    override def isSymmetric: Boolean = left.isMirrorOf(right)
  }

  case object End extends Tree[Nothing] {
    override def toString = "."

    override def isMirrorOf[V](tree: Tree[V]): Boolean = tree == End

    override def isSymmetric: Boolean = true
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)

    def isSymmetric(node: Node): Boolean = false

  }

  object Tree {
    // 程序的本质是好理解的, 但是这段代码有些难理解
    def cBalanced[T](nodes: Int, value: T): List[Tree[T]] = nodes match {
      case n if n < 1 => List(End)
      case n if n % 2 == 1 => {
        val subtrees = cBalanced(n / 2, value)
        //左右无所谓的情况, 左边和右侧的树的所有组合
        subtrees.flatMap(l => subtrees.map(r => Node(value, l, r)))
      }
      case n if n % 2 == 0 => {
        val lesserSubtrees = cBalanced((n - 1) / 2, value)
        val greaterSubtrees = cBalanced((n - 1) / 2 + 1, value)
        //左右有所谓的情况, 左边和右侧的树的所有组合
        lesserSubtrees.flatMap(l => greaterSubtrees.flatMap(g => List(Node(value, l, g), Node(value, g, l))))
      }
    }
  }

}

object test_bintary_tree{
  import binarytree._


  def main(args: Array[String]) {
    val tree = Node('a',
      Node('b', Node('d'), Node('e')),
      Node('c', End, Node('f', Node('g'), End)))

    println("tree: ", tree)
  }
}
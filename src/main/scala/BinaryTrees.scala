package com.github.interaction.s99

object binarytree{

  sealed abstract class Tree[+T]{
    def isMirrorOf[V](tree: Tree[V]): Boolean
    def isSymmetric: Boolean
    // view bound, equal to `def addValue[U >: T](x: U)(implicit cv: U => Ordered[U]):Tree[U]
    // U >:T <% Ordered[U] 的解释顺序是 U >:T  && <% Ordered[U]
    def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
    def leafCount:Int
    def leafList: List[T]
    def internalList: List[T]
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

    override def isMirrorOf[V](tree: Tree[V]): Boolean = tree match {
      case t: Node[V] => left.isMirrorOf(t.right) && right.isMirrorOf(t.left)

      case _ => false
    }

    override def isSymmetric: Boolean = left.isMirrorOf(right)


    override def addValue[U >: T <% Ordered[U]](x: U): Tree[U] =
      if (x < value){//逐个比较节点, 以此判断是在左节点还是在右节点加
        //这个地方的实现很巧妙
        Node(value, left.addValue(x), right)
      }else{
        Node(value, left, right.addValue(x))
      }

    override def leafCount: Int = {
      if (this.isLeaf) 1
      else left.leafCount + right.leafCount
    }

    override def leafList: List[T] = {
      if (this.isLeaf) List(value)
      else left.leafList ++ right.leafList
    }

    override def internalList: List[T] = this match {
      case Node(_, End, End) => Nil
      case Node(x, _, _) => x :: left.internalList ::: right.internalList
    }

    def atLevel(lvl: Int): List[T] = {
      // 这部分代码可以用面向对象的方式吧 End, Node 的 match 给分割开
      def cur[T](node: Tree[T], lvl: Int):List[T] = {
        assert(lvl >= 0)

        node match {
          case End => Nil
          case Node(x, left, right) =>
            if (lvl == 0){
              List(x)
            }else{
              cur(left, lvl - 1) ::: cur(right, lvl -1)
            }
        }

      }

      cur(this, lvl-1)
    }

    def isLeaf = this match {
      case Node(_, End, End) => true
      case _ => false
    }

    /*
    sealed abstract class Tree[+T] {
        def layoutBinaryTree: Tree[T] = layoutBinaryTreeInternal(1, 1)._1
        def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int)
      }

      case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
        def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int) = {
          val (leftTree, myX) = left.layoutBinaryTreeInternal(x, depth + 1)
          val (rightTree, nextX) = right.layoutBinaryTreeInternal(myX + 1, depth + 1) //这个地方正确吗 leaf Node 的两个 End 会因此加 1, 感觉是多加了1
          (PositionedNode(value, leftTree, rightTree, myX, depth), nextX)
        }
      }

      case class PositionedNode[+T](override val value: T, override val left: Tree[T], override val right: Tree[T], x: Int, y: Int) extends Node[T](value, left, right) {
        override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
      }

      case object End extends Tree[Nothing] {
        def layoutBinaryTreeInternal(x: Int, depth: Int) = (End, x)
      }

      答案要写的简洁多了 被虐了 :(
     */
    def layoutBinaryTree = {

      var x:Int = 0
      val mapping = scala.collection.mutable.Map[T, (T, Int, Int)]()

      Tree.inorderTravers(this)((node: Node[T], h)=>{
        x += 1
        mapping.update(node.value, (node.value, x, h))
      })

      def traverse(node: Node[T]): PositionedNode[T] = {
        val (v, x, y) = mapping.get(node.value).get
        val l:Option[PositionedNode[T]] = if (node.left != End) Some(traverse(node.left.asInstanceOf[Node[T]])) else None
        val r = if (node.right != End) Some(traverse(node.right.asInstanceOf[Node[T]])) else None

        PositionedNode(v, l, r, x, y)
      }

      traverse(this)
    }
  }

  case object End extends Tree[Nothing] {
    override def toString = "."

    override def isMirrorOf[V](tree: Tree[V]): Boolean = tree == End

    override def isSymmetric: Boolean = true

    // override def addValue[U >: Nothing <% Ordered[U]](x: U): Tree[U] = Node(x, End, End)
    // 注意这个地方的Nothing 必须干掉, 要不然语义会有问题
    override def addValue[U <% Ordered[U]](x: U): Tree[U] = Node(x, End, End)

    override def leafCount: Int = 0

    override def leafList: List[Nothing] = Nil

    override def internalList: List[Nothing] = Nil
  }

  case class PositionedNode[+T](value: T,
                                left: Option[PositionedNode[T]],
                                right: Option[PositionedNode[T]],
                                x: Int, y: Int){

    override def toString = {
      def fstr[T](node: PositionedNode[T]):String = {
        val left = node.left.flatMap((x)=> Some(fstr(x))).getOrElse(".")
        val right = node.right.flatMap((x)=> Some(fstr(x))).getOrElse(".")

        s"T[${node.x.toString}, ${node.y.toString}](${node.value.toString} ${left} ${right})"
      }
      fstr(this)
    }


  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
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

    def fromList[T <% Ordered[T]](ls: List[T]): Tree[T] =
      ls.foldLeft(End: Tree[T])((b, t)=> b.addValue(t))

    def symmetricBalancedTrees[T](nodes: Int, value: T):List[Tree[T]]=
      cBalanced(nodes, value).filter(_.isSymmetric)

    // return minimum nodes given a height x balanced binary tree
    def minHbalNodes(height: Int): Int = {
      if (height<=0) 0
      else Math.pow(2.toDouble, (height-1).toDouble).toInt
    }


    //return max height given a balanced binary tree contains x nodes
    def maxHbalHeight(nodesCount: Int): Int ={
      val log2 = {x:Double =>
        val logof2 = Math.log(2)
        Math.log(x)/logof2
      }

      // height = log(2, nodesCount + 1)
      if (nodesCount <= 0) 0
      else Math.ceil(log2(nodesCount+1)).toInt
    }


    def completeBinaryTree[T](nodes: Int, value: T): Tree[T] = {
      def generateTree(addr: Int): Tree[T] =
        if (addr > nodes) End
        // 虽然第一个 generateTree 会优先先执行完, 就算超出 nodes 数目 也由于 `if (addr > nodes) End` 的
        // 条件检查给过滤掉了, 所以这段代码还是能够正常工作
        else Node(value, generateTree(2 * addr), generateTree(2 * addr + 1))
      generateTree(1)
    }

    def inorderTravers[T](node: Node[T])(cb:(Node[T], Int)=>Unit):Unit = {
      /*
      fn f(node, h)
        if(left != End)
          f(left, h+1)
        cb(value, h)
        f(right, h+1)
       */

      def f[T](node: Node[T], h: Int, cb: (Node[T], Int)=>Unit) :Unit={
        if (node.left != End) f(node.left.asInstanceOf[Node[T]], h+1, cb)
        cb(node, h)
        if (node.right != End) f(node.right.asInstanceOf[Node[T]], h+1, cb)
      }
      f(node, 1, cb)
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

    println("Tree.fromList(List(3, 2, 5, 7, 1)): ", Tree.fromList(List(3, 2, 5, 7, 1)))

    println("Tree.symmetricBalancedTrees(5, 'x'): ", Tree.symmetricBalancedTrees(5, 'x'))

    println("Tree.minHbalNodes(3): ", Tree.minHbalNodes(3))

    //should be 2, 3
    println("Tree.maxHbalHeight(2), Tree.maxHbalHeight(4)", Tree.maxHbalHeight(2), Tree.maxHbalHeight(4))

    //todo: P60 (**) Construct height-balanced binary trees with a given number of nodes

    println("Node('b', Node('d'), Node('e')).leafCount :", Node('b', Node('d'), Node('e')).leafCount)
    println("Node('x', Node('x'), End).leafCount : ", Node('x', Node('x'), End).leafCount)


    println("Node('b', Node('d'), Node('e')).leafList :", Node('b', Node('d'), Node('e')).leafList)


    println("Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList :",
      Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList)

    println("Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2): ",
      Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2))

    val _node = Tree.fromList(List('n','k','m','c','a','h','g','e','u','p','s','q')).asInstanceOf[Node[Char]]
    Tree.inorderTravers(_node)((node, h)=> println(node.value, h))
    println("_node.layoutBinaryTree: ", _node.layoutBinaryTree)
    println(" Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree: ",
      Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree)

    //PositionNode tostring
    val _node2 = PositionedNode('a', Some(PositionedNode('b', None, None, 3, 4)), None, 1, 2)
    println("PositionedNode toString: ", _node2)
  }
}
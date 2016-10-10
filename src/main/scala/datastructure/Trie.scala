package com.github.interaction.s99

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/*
for more info about this structure, visit @  https://www.youtube.com/watch?v=RIUY7ieyH40
 */

sealed trait Trie[T] {
  def key: T
}

case class Node[T](key: T, childNodes: List[Trie[T]]) extends Trie[T]{

  def tranverse(f: T => Unit): Unit ={
    f(key)
    if (this.childNodes(0).isInstanceOf[Leaf[T]])
      f(childNodes(0).key)
    else{
      childNodes.foreach(e=>{
        e.asInstanceOf[Node[T]].tranverse(f)
      })
    }
  }
}

case class Leaf[T](key: T) extends Trie[T]{

}

object StringTrie {
  def group(datas: List[String], depth: Int = 0): mutable.Map[String, List[String]] ={
    val map = mutable.HashMap.empty[String, List[String]]
    for (e <- datas){
      require(depth < e.length)
      val key = e.charAt(depth).toString
      val ls = map.getOrElseUpdate(key, List.empty[String])
      map += key -> (e :: ls)
    }
    map
  }

  def build(key: String, datas: List[String], depth: Int = 0): Node[String] ={
    require(datas.length != 0)

    val groups = group(datas, depth)
    val ls = ListBuffer.empty[Node[String]]
    for ( (key, value) <- groups){
      if (value.length == 1){
        ls += Node(key, List(Leaf(value(0))))
      }else{
        ls += build(key, value, depth + 1)
      }
    }

    return Node(key, ls.toList)
  }


}


object TrieApp extends App{
  val ls = List("32400", "38718", "31240", "87770", "89313", "893#")
  val node = StringTrie.build("", ls, 0)
  node.tranverse(println)
}


package com.github.interaction.s99

import scala.util.control.Breaks._

object list {
  def add(x: Int, y: Int): Int = x + y

  def last[A](x: List[A]): A = x.last

  def lastN[A](x: List[A], idx: Int): A = x.reverse(idx - 1);

  //def isPalindrome[A](ls: List[A]): Boolean = ls == ls.reverse
  def isPalindrome[A](x: List[A]): Boolean = {
    val middle = x.length / 2
    var result = true
    //http://stackoverflow.com/questions/2742719/how-do-i-break-out-of-a-loop-in-scala
    breakable {
      for (a <- 0 until middle) {
        if (x(a) != x(x.length - 1 - a)) {
          result = false
          break
        }
      }
    }
    result
  }

  //List(List(1, 2), List(3, 4)).flatten
  //flatMap遍历所有的元素，你需要返回List[B]，这个函数之后会将你返回的List组成一个新的List
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case others => List(others)
  }

  /*

  def compressRecursive[A](ls: List[A]): List[A] = ls match {
    case Nil       => Nil
    case h :: tail => h :: compressRecursive(tail.dropWhile(_ == h))
  }

   */
  def compress(ls: List[Any]): List[Any] = {
    var pre = ls(0)
    var result = List(pre)
    for (i <- 1 until ls.length) {
      if (pre != ls(i)) {
        result = result :+ ls(i)
        pre = ls(i)
      }

    }

    result
  }

  /*

  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

   */
  def pack(ls: List[Any]): List[Any] = {
    if (ls.isEmpty) List(Nil)
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  /*
	def encode[A](ls: List[A]): List[(Int, A)] =
    	pack(ls) map { e => (e.length, e.head) }
   */
  def encode(ls: List[Any]): List[Any] = {
    val packed = pack(ls)
    packed flatMap {
      case ms: List[_] => List((ms.length, ms(0)))
      case others => Nil
    }
  }

  def encodeModified(ls: List[Any]): List[Any] = {
    val packed = pack(ls)
    packed flatMap {
      case ms: List[_] if ms.length > 1 => List((ms.length, ms(0)))
      // assert ns.length == 1
      case ns: List[_] => List(ns(0))
      case others => Nil
    }
  }

  def decode[A](ls: List[(Int, A)]): List[A] = ls flatMap { e =>
    List.tabulate(e._1) { e_1 =>
      e._2
    }
  }

  def duplicate(ls: List[Any]): List[Any] = ls flatMap { e =>
    List.tabulate(2) { p1 =>
      e
    }
  }

  def duplicateN(n: Int, ls: List[Any]): List[Any] = ls flatMap { e =>
    List.tabulate(n) { p1 =>
      e
    }
  }
  /*
	// Simple recursion.
	  def dropRecursive[A](n: Int, ls: List[A]): List[A] = {
	    def dropR(c: Int, curList: List[A]): List[A] = (c, curList) match {
	      case (_, Nil)       => Nil
	      case (1, _ :: tail) => dropR(n, tail)
	      case (_, h :: tail) => h :: dropR(c - 1, tail)
	    }
	    dropR(n, ls)
	  }
   */
  def drop(step: Int, ls: List[Any]): List[Any] = {
    //用 filter + map 的方式语义上比较正确
    // collect 的方式语义上有歧义但是比较便捷
    ls.zipWithIndex collect {
      case x if ((x._2 + 1) % step != 0) => x._1
    }
  }

  def split(offset: Int, ls: List[Any]): (List[Any], List[Any]) = {
    val nl = ls.zipWithIndex
    val st = nl.span { e =>
      if (e._2 < offset) true
      else false
    }
    val f = { x: (Any, Int) => x._1 }
    (st._1.map(f), st._2.map(f))
  }

  def slice(from: Int, until: Int, ls: List[Any]): List[Any] = {
    ls.slice(from, until)
  }

  def rotate(offset: Int, ls: List[Any]): List[Any] = {
    if (Math.abs(offset) > ls.length - 1) {
      throw new RuntimeException("invalid arguments")
    }

    val index = if (offset > 0) offset else ls.length - Math.abs(offset)
    ls.slice(index, ls.length) ++ ls.slice(0, index);
  }

  //idx starts from 1, not 0
  def removeAt[A](idx: Int, ls: List[A]): (List[A], A) = ls.splitAt(idx) match {
    case (Nil, _) if idx < 0 => throw new NoSuchElementException
    case (_, Nil) => throw new NoSuchElementException
    case (pre, head :: tails) => (pre ::: tails, head)
  }

  def insertAt[A](idx: Int, e: A, ls: List[A]): List[A] = ls.splitAt(idx) match {
    case (Nil, _) => throw new NoSuchElementException
    case (_, Nil) => throw new NoSuchElementException
    case (pre, aft) => pre ::: List(e) ::: aft
  }

  def range(left: Int, right: Int): List[Int] = {
    Nil ++ (left to right)
  }
  /*
  // It can be expensive to create a new Random instance every time, so let's
  // only do it once.
  def randomSelect[A](n: Int, ls: List[A]): List[A] = {
    def randomSelectR(n: Int, ls: List[A], r: util.Random): List[A] =
      if (n <= 0) Nil
      else {
        val (rest, e) = removeAt(r.nextInt(ls.length), ls)
        e :: randomSelectR(n - 1, rest, r)
      }
    randomSelectR(n, ls, new util.Random)
  }
   */
  def randomSelect[A](total: Int, ls: List[A]): List[A] = {
    def select(total: Int, ls: List[A], sampler: util.Random): List[A] = {
      if (total == 0) Nil
      else {
        val (nls, e) = removeAt(sampler.nextInt(ls.length), ls)
        e :: select(total - 1, nls, sampler)
      }
    }

    select(total, ls, new util.Random)
  }

  def lotto(count: Int, max: Int): List[Int] =
    randomSelect(count, List.range(1, max + 1))

  def randomPermute(ls: List[Any]): List[Any] =
    randomSelect(ls.length, ls)

}

object TestList {
  import list._

  def main(args: Array[String]): Unit = {
    println("Problem Set")
    //1
    println("1. last element in list is : " + last(List("a", "b", "c")))
    //2
    println("2. last but one element in the list is: " + lastN(List("A", "B", "C"), 1))
    //3
    println("3. 2th element in the list is : " + List(1, 1, 2)(2))
    //4
    println("4. number of elements in the list is: " + List(1, 2, 3).length)
    //5
    println("5. this is the way of reverse a list: " + List(1, 2, 3).reverse)
    //6
    println("6. is this array palindrome:" + isPalindrome(List(1, 2, 3, 2, 1)))
    //7
    println("7. flatten array should work: " + flatten(List(List(1, 2), 3, 4)))
    //8
    def result = compress(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'));
    println("8. compress list is: " + result)
    //9
    println("9. packed list is " + pack(List(1, 1, 2, 3, 3, 4, 5, 5)));
    //10
    println("10. encoded list is: " + encode(List(1, 1, 2, 3, 3, 4, 5, 5)));
    //11
    println("11. modified encoded list is: " + encodeModified(List(1, 1, 2, 3, 3, 4, 5, 5)))
    //12
    println("12. decoded list result is: " + decode(List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))))
    //14
    println("14. duplicated list result is: " + duplicate(List(1, 2, 3, 4)))
    //15
    println("15. duplicateN list result is: " + duplicateN(3, List(1, 2, 3, 4)))
    //16
    println("16. drop list result is: " + drop(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
    //17
    println("17. split list result is: " + split(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
    //18
    println("18. slice list result is: " + slice(3, 7, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
    //19
    println("19. rotate list result is: " + rotate(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
    println("19. rotate list result is: " + rotate(-2, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
    //20
    println("20. remove list at is: " + removeAt(1, List('a', 'b', 'c', 'd')))
    //21
    println("21. insert list at is: " + insertAt(1, 'n', List('a', 'b', 'c', 'd')))
    //22
    println("22. create a range of list: " + range(1, 10))
    //23
    println("23.  Extract a given number of randomly selected elements from a list: " + randomSelect(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
    //25
    println("24. Generate a random permutation of the elements of a list.: " + randomPermute(List('a', 'b', 'c', 'd', 'e', 'f')))
    //26

    //27

    //28


  }
}

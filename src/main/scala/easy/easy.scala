package easy

import scala.util.control.Breaks._

// http://aperiodic.net/phil/scala/s-99/

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

object arithmetic {
  implicit def IntMix(number: Int) = new {
    /*todo: how to use Stream class
     * object S99Int {
		  val primes = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime })
		}
     */
    def isPrime: Boolean = {
      //this is pretty naive implementation
      //for more efficient test please visit:
      // http://primes.utm.edu/prove/index.html
      // [Haskell implementation](http://article.gmane.org/gmane.comp.lang.haskell.cafe/19470)
      val end: Int = Math.ceil(Math.sqrt(number.toDouble)).toInt
      val list = List.range(2, end+1)
      (number > 1) && (list forall { number % _ != 0 })
    }
  }

  def gcd(x: Int, y: Int): Int = {
    if (y == 0) {
      x
    } else {
      gcd(y, x % y)
    }
  }
  
  implicit def IntCoprimeMixIn(number: Int) = new {
    //what is number coprime:
	  // 14 and 15 are coprime, being commonly divisible by only 1, but 14 and 21 are not,
    // because they are both divisible by 7.
    def isCoprimeTo(y: Int):Boolean = gcd(number, y) == 1
  }
  
  implicit def TotientMixIn(number: Int) = new {
    //获得 1..number 与 number 的互质的自然数长度
    def totient: Int = (1 to number) filter {number.isCoprimeTo(_)} length
  }
  
  
  
}
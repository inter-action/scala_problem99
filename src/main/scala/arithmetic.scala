package com.github.interaction.s99

import scala.util.control.Breaks._

// http://aperiodic.net/phil/scala/s-99/



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


object TestArithmetic {
  import arithmetic._

  def main(args: Array[String]): Unit = {
    println("hello world")

    /*
      P31 (**) Determine whether a given integer number is prime.
      scala> 7.isPrime
      res0: Boolean = true
     */
    println("7.isPrime: " + 7.isPrime)


    /*
    P32 (**) Determine the greatest common divisor of two positive integer numbers.
    Use Euclid's algorithm.
    scala> gcd(36, 63)
     */
    println("gcd(36, 63) is: " + gcd(36, 63))


    /*
    P33 (*) Determine whether two positive integer numbers are coprime.
    Two numbers are coprime if their greatest common divisor equals 1.
    scala> 35.isCoprimeTo(64)
    res0: Boolean = true
     */
    println("35.isCoprimeTo(64)" + 35.isCoprimeTo(64))


    /*
    P34 (**) Calculate Euler's totient function phi(m).
    Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m) that are coprime to m.
    scala> 10.totient
    res0: Int = 4P
     */
    println("10.totient is: " + 10.totient)




  }
}
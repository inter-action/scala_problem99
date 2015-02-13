import easy.list._
import easy.arithmetic._

//working with the problem set http://aperiodic.net/phil/scala/s-99/

object Set_List {
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

object Arithmetic {
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
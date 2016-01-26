package com.github.interaction.s99


object logic_and_code{
  object logic {
    def and(a: Boolean, b: Boolean) = a & b

    def or(a:Boolean, b:Boolean) = a | b

    def xor(a: Boolean, b:Boolean) = a ^ b

    def table2(f: (Boolean, Boolean) => Boolean): Unit = {
      val seq = Seq(true, false)

      for (
        a <- seq; b<-seq
      ){
        println(a, b, f(a, b))
      }
    }
  }


  object logic1 {
    //todo: get rid of ugly .b call, (extends Function0 will not get this job done)
    class BoolWrapper(val b: Boolean){


      def map(f: Boolean=> Boolean): BoolWrapper = BoolWrapper.map(this)(f)

      def flatMap(f: Boolean => BoolWrapper):BoolWrapper = BoolWrapper.flatMap(this)(f)

      def and(b: BoolWrapper): BoolWrapper = BoolWrapper.map2(this, b)((a, b)=> a & b)

      def or(b: BoolWrapper): BoolWrapper = BoolWrapper.map2(this, b)((a, b)=> a | b)

      def xor(b: BoolWrapper): BoolWrapper = BoolWrapper.map2(this, b)((a, b)=> a ^ b)

    }


    object BoolWrapper{
      def apply(b: Boolean) = new BoolWrapper(b)

      def map(a: BoolWrapper)(f: Boolean => Boolean):BoolWrapper = BoolWrapper(f(a.b))

      def flatMap(a: BoolWrapper)(f: Boolean => BoolWrapper) = f(a.b)

      def map2(a: BoolWrapper, b: BoolWrapper)(f: (Boolean, Boolean)=> Boolean):BoolWrapper =
        for(
          ba <- a;
          bb <- b
        ) yield f(ba, bb)

    }

    implicit def boolToWrapper(a: Boolean):BoolWrapper = BoolWrapper(a)

    def not(a: BoolWrapper) = a.map(!_)
  }


}

object TestLogicAndCode{
  import logic_and_code._


  def main(args: Array[String]) {
    //logic
    {
      import logic_and_code.logic._
      table2((a, b)=> and(a, or(a, b)))
    }

    // logic1
    {
      import logic_and_code.logic1._

      println("true and false is: ", (true and false).b)
      println("not(true) is", not(true).b)
      println("true and (true or not(false)) is ", (true and (true or not(false))).b)
      println("not(true and false) is: ", (not(true and false)).b)
    }
  }
}
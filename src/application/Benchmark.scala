package application

import scala.collection.mutable.ArrayBuffer
import scala.math._

/**
 * Created by qmha on 10/29/14.
 */
object Benchmark {
  def f1(particle:ArrayBuffer[Int]):Double = {
    0
  }

  def f2(particle:ArrayBuffer[Int]):Double = {
    0
  }

  def f3(particle:ArrayBuffer[Int]):Double = {
    0
  }

  def f4(particle:ArrayBuffer[Int]):Double = {
    0
  }

  def f5(particle:ArrayBuffer[Int]):Double = {
    0
  }

  def f6(particle:ArrayBuffer[Int]):Double = {
    0
  }

  def f7(particle:ArrayBuffer[Int]):Double = {
    0
  }

  def f8(particle:ArrayBuffer[Int]):Double = {
    0
  }

  def f9(particle:ArrayBuffer[Int]):Double = {
    0
  }

  def f10(particle:ArrayBuffer[Int]):Double = {
    0
  }

  def f11(particle:ArrayBuffer[Int]):Double = {
    0
  }

  def f12(particle:ArrayBuffer[Int]):Double = {
    0
  }

  def f13(particle:ArrayBuffer[Int]):Double = {
    0
  }

  def f14(particle:ArrayBuffer[Int]):Double = {
    0
  }

  def f15(particle:ArrayBuffer[Int]):Double = {
    val PI:Double = 4 * atan2(1,1)
    var output:Double = 0

    var i:Int = 0
    for (i <- 0 until Configuration.NUMBER_OF_INPUT) {
      var base_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var j:Int = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2(j) = particle(i * Configuration.ENCODE_BIT + j)
      }

      var x:Double = Misc.base_2_to_10(base_2) / pow(10, Configuration.DECIMAL)

      output += x * x - 10 * cos(2 * PI * x) + 10

      //println("Benchmark = " + output)
    }

    return output
  }


  def f16(particle:ArrayBuffer[Int]):Double = {
    0
  }

  def f17(particle:ArrayBuffer[Int]):Double = {
    0
  }

  def f18(particle:ArrayBuffer[Int]):Double = {
    0
  }
}

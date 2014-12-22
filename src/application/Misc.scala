package application

import scala.collection.mutable.ArrayBuffer
import scala.math._
import scala.util.Random

/**
 * Created by qmha on 10/28/14.
 */
object Misc {
  def evaluation_benchmark = {

  }

  def copy_chromosome(original:ArrayBuffer[Int]):ArrayBuffer[Int] = {
    var to_be_transfered:ArrayBuffer[Int] = ArrayBuffer.fill((Configuration.NUMBER_OF_INPUT * (3 * Configuration.WEIGHT_BIT + 1)))(-1)
    var k:Int = 0

    for (k <- 0 until (Configuration.NUMBER_OF_INPUT * (3 * Configuration.WEIGHT_BIT + 1))) {
      //original(k) = to_be_transfered(k)
      to_be_transfered(k) = original(k)
    }

    to_be_transfered
  }

  def in_the_range(chromosome:ArrayBuffer[Int]):Boolean = {
    var i:Int = 0

    for (i <- 0 until Configuration.NUMBER_OF_INPUT) {
      var base_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(-1)
      var j:Int = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2(j) = chromosome(i * Configuration.ENCODE_BIT + j)
      }

      var x:Double = base_2_to_10(base_2) / (pow(10, Configuration.DECIMAL))

      if (x > Configuration.X_MAX || x < Configuration.X_MIN) {
        return false
      }
    }

    return true
  }

  def base_2_to_10(base_2:ArrayBuffer[Int]):Int = {
    var w:Int = 0
    var i:Int = 0

    for (i <- 1 until Configuration.ENCODE_BIT) {
      if (base_2(i) != 0 && base_2(i) != 1) {
        throw new Exception ("Error: input string is not base 2!")
      }

      w = w + base_2(i) * (pow(2, (Configuration.ENCODE_BIT - i - 1))).toInt
    }

    if (base_2(0) == 1) {
      return w
    }
    else if (base_2(0) == 0) {
      return (0 - w)
    }
    else {
      if (base_2(i) != 0 && base_2(i) != 1)
        throw new Exception("Error: input string is not base 2!")
    }

    w
  }

  def my_sigmoid(t:Double):Double = {
    1 / (1 + exp(-t))
  }

  def cmp_chromosome(c1:ArrayBuffer[Int], c2:ArrayBuffer[Int]):Boolean = {
    var k:Int = 0
    for (k <- 0 until (Configuration.NUMBER_OF_INPUT * (3 * Configuration.WEIGHT_BIT + 1))) {
      if (c2(k) != c1(k))
        return false
    }

    true
  }

  def get_random():Double = {
    Random.nextDouble() * Random.nextDouble()
  }
}

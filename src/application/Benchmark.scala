package application

import scala.collection.mutable.ArrayBuffer
import scala.math._
import scala.util.Random

/**
 * Created by qmha on 10/29/14.
 */
object Benchmark {
  def f1(particle:ArrayBuffer[Int]):Double = {
    var i:Int = 0
    var j:Int = 0
    var fx:Double = 0

    for (i <- 0 until Configuration.NUMBER_OF_INPUT) {
      var base_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var x:Double = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2(j) = particle(i * Configuration.ENCODE_BIT + j)
      }

      x = Misc.base_2_to_10(base_2) / pow(10, Configuration.DECIMAL)

      fx += x * x
    }

    fx
  }

  def f2(particle:ArrayBuffer[Int]):Double = {
    var i:Int = 0
    var j:Int = 0
    var fx:Double = 0

    for (i <- 0 until (Configuration.NUMBER_OF_INPUT - 1)) {
      var base_2_1:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var base_2_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var x1:Double = 0
      var x2:Double = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2_1(j) = particle(i * Configuration.ENCODE_BIT + j)
        base_2_1(j) = particle((i + 1) * Configuration.ENCODE_BIT + j)
      }

      x1 = Misc.base_2_to_10(base_2_1) / pow(10, Configuration.DECIMAL)
      x2 = Misc.base_2_to_10(base_2_2) / pow(10, Configuration.DECIMAL)

      fx += 100 * pow((x2 - x1 * x1), 2) + pow((x1 - 1), 2)
    }

    fx
  }

  def f3(particle:ArrayBuffer[Int]):Double = {
    var i:Int = 0
    var j:Int = 0
    var fx:Double = 0

    for (i <- 0 until Configuration.NUMBER_OF_INPUT) {
      var base_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var x:Double = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2(j) = particle(i * Configuration.ENCODE_BIT + j)
      }

      x = Misc.base_2_to_10(base_2) / pow(10, Configuration.DECIMAL)

      fx += pow((abs(x + 0.5)),2)
    }

    fx
  }

  def f4(particle:ArrayBuffer[Int]):Double = {
    var i:Int = 0
    var j:Int = 0
    var fx:Double = 0

    for (i <- 0 until Configuration.NUMBER_OF_INPUT) {
      var base_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var x:Double = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2(j) = particle(i * Configuration.ENCODE_BIT + j)
      }

      x = Misc.base_2_to_10(base_2) / pow(10, Configuration.DECIMAL)

      fx += (i * pow(x, 4))
    }

    (fx + Random.nextDouble())
  }

  def f5(particle:ArrayBuffer[Int]):Double = {
    var i:Int = 0
    var j:Int = 0
    var fx:Double = -999999

    for (i <- 0 until Configuration.NUMBER_OF_INPUT) {
      var base_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var x:Double = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2(j) = particle(i * Configuration.ENCODE_BIT + j)
      }

      x = Misc.base_2_to_10(base_2) / pow(10, Configuration.DECIMAL)

      if (abs(x) > fx)
        fx = abs(x)
    }

    fx
  }

  def f6(particle:ArrayBuffer[Int]):Double = {
    var i:Int = 0
    var j:Int = 0
    var fx1:Double = 0
    var fx2:Double = 1

    for (i <- 0 until Configuration.NUMBER_OF_INPUT) {
      var base_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var x:Double = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2(j) = particle(i * Configuration.ENCODE_BIT + j)
      }

      x = Misc.base_2_to_10(base_2) / pow(10, Configuration.DECIMAL)

      fx1 += abs(x)
      fx2 = fx2 * abs(x)
    }

    (fx1 + fx2)
  }

  // Easom function
  def f7(particle:ArrayBuffer[Int]):Double = {
    var i:Int = 0
    var j:Int = 0
    var fx:Double = 0

    for (i <- 0 until 1) {
      var base_2_1:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var base_2_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var x_1:Double = 0
      var x_2:Double = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2_1(j) = particle(i * Configuration.ENCODE_BIT + j)
        base_2_2(j) = particle((i + 1) * Configuration.ENCODE_BIT + j)
      }

      x_1 = Misc.base_2_to_10(base_2_1) / pow(10, Configuration.DECIMAL)
      x_2 = Misc.base_2_to_10(base_2_2) / pow(10, Configuration.DECIMAL)

      fx = -1 * cos(x_1) * cos(x_2) * exp(-(pow(x_1 - Pi,2) + pow(x_2 - Pi,2)))
    }

    fx
  }

  def f8(particle:ArrayBuffer[Int]):Double = {
    var i:Int = 0
    var j:Int = 0
    var fx:Double = 0

    for (i <- 0 until Configuration.NUMBER_OF_INPUT) {
      var base_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var x:Double = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2(j) = particle(i * Configuration.ENCODE_BIT + j)
      }

      x = Misc.base_2_to_10(base_2) / pow(10, Configuration.DECIMAL)

      fx += x * x
    }

    fx
  }

  def f9(particle:ArrayBuffer[Int]):Double = {
    var i:Int = 0
    var j:Int = 0
    var fx:Double = 0

    for (i <- 0 until Configuration.NUMBER_OF_INPUT) {
      var base_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var x:Double = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2(j) = particle(i * Configuration.ENCODE_BIT + j)
      }

      x = Misc.base_2_to_10(base_2) / pow(10, Configuration.DECIMAL)

      fx += x * x
    }

    fx
  }

  def f10(particle:ArrayBuffer[Int]):Double = {
    var i:Int = 0
    var j:Int = 0
    var fx:Double = 0

    for (i <- 0 until Configuration.NUMBER_OF_INPUT) {
      var base_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var x:Double = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2(j) = particle(i * Configuration.ENCODE_BIT + j)
      }

      x = Misc.base_2_to_10(base_2) / pow(10, Configuration.DECIMAL)

      fx += x * x
    }

    fx
  }

  def f11(particle:ArrayBuffer[Int]):Double = {
    var i:Int = 0
    var j:Int = 0
    var fx:Double = 0

    for (i <- 0 until Configuration.NUMBER_OF_INPUT) {
      var base_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var x:Double = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2(j) = particle(i * Configuration.ENCODE_BIT + j)
      }

      x = Misc.base_2_to_10(base_2) / pow(10, Configuration.DECIMAL)

      fx += x * x
    }

    fx
  }

  def f12(particle:ArrayBuffer[Int]):Double = {
    var i:Int = 0
    var j:Int = 0
    var fx:Double = 0

    for (i <- 0 until Configuration.NUMBER_OF_INPUT) {
      var base_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var x:Double = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2(j) = particle(i * Configuration.ENCODE_BIT + j)
      }

      x = Misc.base_2_to_10(base_2) / pow(10, Configuration.DECIMAL)

      fx += x * x
    }

    fx
  }

  def f13(particle:ArrayBuffer[Int]):Double = {
    var i:Int = 0
    var j:Int = 0
    var fx:Double = 0

    for (i <- 0 until Configuration.NUMBER_OF_INPUT) {
      var base_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var x:Double = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2(j) = particle(i * Configuration.ENCODE_BIT + j)
      }

      x = Misc.base_2_to_10(base_2) / pow(10, Configuration.DECIMAL)

      fx += x * x
    }

    fx
  }

  def f14(particle:ArrayBuffer[Int]):Double = {
    var i:Int = 0
    var j:Int = 0
    var fx:Double = 0
    var u:Double = 0

    for (i <- 0 until Configuration.NUMBER_OF_INPUT) {
      var base_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var base_2_1:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)

      var x:Double = 0
      var x1:Double = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2(j) = particle(i * Configuration.ENCODE_BIT + j)
        if (i < 29)
          base_2_1(j) = particle((i + 1) * Configuration.ENCODE_BIT + j)
      }

      x = Misc.base_2_to_10(base_2) / pow(10, Configuration.DECIMAL)

      if (i == 0) {
        fx += sin(3 * Pi * x) * sin(3 * Pi * x)
      }
      else if (i == 29) {
        fx += pow((x - 1), 2) * (1 + (sin(2 * Pi *x) * sin(2 * Pi *x)))
      }
      else {
        x1 = Misc.base_2_to_10(base_2_1) / pow(10, Configuration.DECIMAL)
        fx += pow((x - 1), 2) * (1 + (sin(3 * Pi * x1) * sin(3 * Pi * x1)))
      }

      u += this.f14_u(x, 5, 100, 4)
    }

    (fx * 0.1 + u)
  }

  def f14_u(x:Double, a:Int, k:Int, m:Int):Double = {
    if (x > a) {
      return (k * pow((x - a), m))
    }
    else if (x <= a && x >= -a) {
      return 0
    }
    else {
      return (k * pow((-x - a),m))
    }
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

    }

    return output
  }


  def f16(particle:ArrayBuffer[Int]):Double = {
    var i:Int = 0
    var j:Int = 0
    var fx1:Double = 0
    var fx2:Double = 1

    for (i <- 0 until Configuration.NUMBER_OF_INPUT) {
      var base_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var x:Double = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2(j) = particle(i * Configuration.ENCODE_BIT + j)
      }

      x = Misc.base_2_to_10(base_2) / pow(10, Configuration.DECIMAL)

      fx1 += x * x
      fx2 = fx2 * (cos(x / sqrt(i)))
    }

    ((1/4000) * fx1 - fx2) + 1
  }

  def f17(particle:ArrayBuffer[Int]):Double = {
    var i:Int = 0
    var j:Int = 0
    var fx1:Double = 0
    var fx2:Double = 0

    for (i <- 0 until Configuration.NUMBER_OF_INPUT) {
      var base_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var x:Double = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2(j) = particle(i * Configuration.ENCODE_BIT + j)
      }

      x = Misc.base_2_to_10(base_2) / pow(10, Configuration.DECIMAL)

      fx1 += x * x
      fx2 += cos(2 * Pi * x)
    }

    (-20 * exp(-0.2 * sqrt((1/30) * fx1)) - exp((1/30) * fx2) + 20 + exp(1))
  }

  def f18(particle:ArrayBuffer[Int]):Double = {
    var i:Int = 0
    var j:Int = 0
    var fx:Double = 0

    for (i <- 0 until Configuration.NUMBER_OF_INPUT) {
      var base_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var x:Double = 0
      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2(j) = particle(i * Configuration.ENCODE_BIT + j)
      }

      x = Misc.base_2_to_10(base_2) / pow(10, Configuration.DECIMAL)

      fx += x * sin(sqrt(abs(x)))
    }

    Configuration.NUMBER_OF_INPUT * 418.9829 - fx
  }
}

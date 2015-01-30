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

      x = Misc.base_2_to_10(base_2) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2)))

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
        base_2_2(j) = particle((i + 1) * Configuration.ENCODE_BIT + j)
      }

      x1 = Misc.base_2_to_10(base_2_1) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2_1)))
      x2 = Misc.base_2_to_10(base_2_2) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2_2)))

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

      x = Misc.base_2_to_10(base_2) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2)))

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

      x = Misc.base_2_to_10(base_2) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2)))

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

      x = Misc.base_2_to_10(base_2) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2)))

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

      x = Misc.base_2_to_10(base_2) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2)))

      fx1 += abs(x)
      fx2 = fx2 * abs(x)
    }

    (fx1 + fx2)
  }

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

      x_1 = Misc.base_2_to_10(base_2_1) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2_1)))
      x_2 = Misc.base_2_to_10(base_2_2) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2_2)))

      fx = -cos(x_1) * cos(x_2) * exp(-(pow(x_1 - Pi,2) + pow(x_2 - Pi,2)))
    }

    fx
  }

  // Easom function
  def f8(particle:ArrayBuffer[Int]):Double = {
    // pre-defined matrix for easom function
    val a:Array[Array[Int]] = Array(
      Array(-32, -16, 0, 16, 32, -32, -16, 0, 16, 32, -32, -16, 0, 16, 32, -32, -16, 0, 16, 32, -32, -16, 0, 16, 32),
      Array(-32, -32, -32, -32, -32, -16, -16, -16, -16, -16, 0, 0, 0, 0, 0, 16, 16, 16, 16, 16, 32, 32, 32, 32, 32)
    )

    var i:Int = 0
    var j:Int = 0
    var k:Int = 0
    var fx:Double = 0

    for (j <- 0 until 25) {
      var fi:Double = 0
      for (i <- 0 until 2) {
        var base_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
        var x:Double = 0
        for (k <- 0 until Configuration.ENCODE_BIT) {
          base_2(k) = particle(i * Configuration.ENCODE_BIT + k)
        }

        x = Misc.base_2_to_10(base_2) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2)))

        fi += pow(x - a(i)(j),6)
      }

      fx += (1/(j + fi))
    }

    pow(((1/500) + fx), -1)
  }

  // Kowalik function
  def f9(particle:ArrayBuffer[Int]):Double = {
    var a:Array[Double] = Array(0.1957, 0.1947, 0.1735, 0.1600, 0.0844, 0.0627, 0.0456, 0.0342, 0.0323, 0.0235, 0.0246)
    // b^(-1)
    var b:Array[Double] = Array(0.25, 0.5, 1, 2, 4, 6, 8, 10, 12, 14, 16)

    var i:Int = 0
    var j:Int = 0
    var fx:Double = 0

    for (i <- 0 until 11) {
      var base_2_1:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var base_2_2:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var base_2_3:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)
      var base_2_4:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.ENCODE_BIT)(0)

      var x_1:Double = 0
      var x_2:Double = 0
      var x_3:Double = 0
      var x_4:Double = 0

      for (j <- 0 until Configuration.ENCODE_BIT) {
        base_2_1(j) = particle(i * Configuration.ENCODE_BIT + j)
        base_2_2(j) = particle((i + 1) * Configuration.ENCODE_BIT + j)
        base_2_3(j) = particle((i + 2) * Configuration.ENCODE_BIT + j)
        base_2_4(j) = particle((i + 3) * Configuration.ENCODE_BIT + j)
      }

      x_1 = Misc.base_2_to_10(base_2_1) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2_1)))
      x_2 = Misc.base_2_to_10(base_2_2) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2_2)))
      x_3 = Misc.base_2_to_10(base_2_3) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2_3)))
      x_4 = Misc.base_2_to_10(base_2_4) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2_4)))

      fx += pow(a(i) - (x_1 * (pow(pow(b(i), -1),2) + pow(b(i),-1) * x_2)) / (pow(pow(b(i), 2),-1) + b(i) * x_3 + x_4), 2)
    }

    fx
  }

  def f10(particle:ArrayBuffer[Int]):Double = {
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

      x_1 = Misc.base_2_to_10(base_2_1) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2_1)))
      x_2 = Misc.base_2_to_10(base_2_2) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2_2)))

      fx = -(sin(x_1) * sin(x_2) / (x_1 * x_2))
    }

    fx
  }

  def f11(particle:ArrayBuffer[Int]):Double = {
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

      x_1 = Misc.base_2_to_10(base_2_1) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2_1)))
      x_2 = Misc.base_2_to_10(base_2_2) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2_2)))

      fx = (4 * pow(x_1, 2) - 2.1 * pow(x_1, 2) + (1/3) * pow(x_1, 4) + x_1 * x_2 - 4 * pow(x_2, 2) + 4 * pow(x_2, 4))
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

      x = Misc.base_2_to_10(base_2) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2)))

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

      x = Misc.base_2_to_10(base_2) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2)))

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

      x = Misc.base_2_to_10(base_2) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2)))

      if (i == 0) {
        fx += sin(3 * Pi * x) * sin(3 * Pi * x)
      }
      else if (i == 29) {
        fx += pow((x - 1), 2) * (1 + (sin(2 * Pi *x) * sin(2 * Pi *x)))
      }
      else {
        x1 = Misc.base_2_to_10(base_2_1) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2_1)))
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

      var x:Double = Misc.base_2_to_10(base_2) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2)))

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

      x = Misc.base_2_to_10(base_2) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2)))

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

      x = Misc.base_2_to_10(base_2) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2)))

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

      x = Misc.base_2_to_10(base_2) / pow(10, Misc.getDecimalPoint(Misc.base_2_to_10(base_2)))

      fx += x * sin(sqrt(abs(x)))
    }

    Configuration.NUMBER_OF_INPUT * 418.9829 - fx
  }
}

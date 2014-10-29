package algorithms

import scala.collection.mutable.ArrayBuffer
import application.Configuration
import application.Misc
import scala.math._
import scala.util.Random

/**
 * Created by qmha on 10/28/14.
 */
class BinaryHPSOWM {
  def initialization_hpsowm = {
    var i:Int = 0
    for (i <- 0 until Configuration.POP_SIZE) {
      var k:Int = 0
      for (k <- 0 until (Configuration.NUMBER_OF_INPUT * (3 * Configuration.WEIGHT_BIT + 1))) {
        this.VELOCITY(i)(k) = 0
        this.P_BEST_CHROMOSOME(i)(k) = 0
        this.POPULATION2(i)(k) = 0
      }

      this.P_BEST_CHROMOSOME(i) = Misc.copy_chromosome(this.POPULATION(i))
      this.P_BEST_OBJECTIVE(i) = this.OBJECTIVE(i)
    }

    this.G_BEST_CHROMOSOME = this.POPULATION(0)
    this.G_BEST_OBJECTIVE = this.OBJECTIVE(0)
  }

  def hpsowm_save_best = {
    var i:Int = 0
    for (i <- 0 until Configuration.POP_SIZE) {
      if (this.OBJECTIVE(i) <= this.P_BEST_OBJECTIVE(i)) {
        this.P_BEST_CHROMOSOME(i) = Misc.copy_chromosome(this.POPULATION(i))
        this.P_BEST_OBJECTIVE(i) = this.OBJECTIVE(i)
      }

      if (this.OBJECTIVE(i) <= this.G_BEST_OBJECTIVE) {
        this.G_BEST_CHROMOSOME = Misc.copy_chromosome(this.POPULATION(i))
        this.G_BEST_OBJECTIVE = this.OBJECTIVE(i)
      }
    }
  }

  def hpsowm_velocity(iot:Double) = {
    var FI:Double = Configuration.FI_1 + Configuration.FI_2
    var KK:Double = Configuration.KKKK * 2 / abs(2 - FI - sqrt (FI * FI - 4 * FI))
    var WW:Double = Configuration.WW_MAX - (Configuration.WW_MAX - Configuration.WW_MIN) * iot

    var i:Int = 0
    for (i <- 0 until Configuration.POP_SIZE) {
      var ii:Int = i + this.OC

      if (ii >= Configuration.POP_SIZE)
        ii = ii - Configuration.POP_SIZE

      var k:Int = 0
      for (k <- 0 until (Configuration.NUMBER_OF_INPUT * (3 * Configuration.WEIGHT_BIT + 1))) {
        this.VELOCITY(i)(k) = KK * (
          WW * this.VELOCITY(i)(k)
          + Configuration.FI_1 * Random.nextDouble() * (this.P_BEST_CHROMOSOME(i)(k) - this.POPULATION(i)(k))
          + Configuration.FI_2 * Random.nextDouble() * (this.G_BEST_CHROMOSOME(k) - this.POPULATION(i)(k))
        )

        if (this.VELOCITY(i)(k) > Configuration.V_MAX) {
          this.VELOCITY(i)(k) = Configuration.V_MAX
        }
        else if (this.VELOCITY(i)(k) < 0 - Configuration.V_MAX) {
          this.VELOCITY(i)(k) = 0 - Configuration.V_MAX
        }
      }
    }

    this.OC += 1
    if (this.OC == Configuration.POP_SIZE)
      this.OC = 0
  }

  def hpsowm_update = {
    var i:Int = 0
    for (i <- 0 until Configuration.POP_SIZE) {
      var flag:Int = 0
      this.POPULATION2(i) = Misc.copy_chromosome(this.POPULATION(i))
      this.OBJECTIVE2(i) = this.OBJECTIVE(i)

      var k:Int = 0
      for (k <- 0 until (Configuration.NUMBER_OF_INPUT * (3 * Configuration.WEIGHT_BIT + 1))) {
        var old_value:Int = this.POPULATION(i)(k)

        if (Random.nextDouble() < Misc.my_sigmoid(this.VELOCITY(i)(k))) {
          this.POPULATION(i)(k) = 1
        }
        else {
          this.POPULATION(i)(k) = 0
        }

        if (this.POPULATION(i)(k) != old_value)
          flag = 1
      }

      if (flag == 1) {
        this.OBJECTIVE(i) = 10000000000
      }

      if (!Misc.in_the_range(this.POPULATION(i)) || Misc.cmp_chromosome(this.POPULATION(i), this.G_BEST_CHROMOSOME)) {
        this.POPULATION(i) = Misc.copy_chromosome(this.POPULATION2(i))
        this.OBJECTIVE(i) = this.OBJECTIVE2(i)
      }
    }
  }

  def hpsowm_wm(iot:Double) = {
    var a:Double = exp( (0 - log (Configuration.GG)) * ( pow (( 1 - iot), Configuration.XI_WM)) + log (Configuration.GG) )
    var fi:Double = Random.nextDouble() * 5 * a - 2.5 * a
    var rho:Double = Configuration.GAMA * (1 / sqrt(a)) * exp(0 - (fi / a) * (fi / a) / 2) * cos(5 * fi / a)

    var i:Int = 0
    for (i <- 0 until Configuration.POP_SIZE) {
      this.POPULATION2(i) = Misc.copy_chromosome(this.POPULATION(i))
      this.OBJECTIVE2(i) = this.OBJECTIVE(i)

      var flag:Int = 0

      // Comparable in perl: next if rand() < P_M
      if (Random.nextDouble() <= Configuration.P_M) {
        var k:Int = 0
        for (k <- 0 until (Configuration.NUMBER_OF_INPUT * (3 * Configuration.WEIGHT_BIT + 1))) {
          var old_value:Int = this.POPULATION(i)(k)

          if (rho > 0) {
            if (Random.nextDouble() < Misc.my_sigmoid(rho * (old_value - Configuration.PARA_MIN))) {
              this.POPULATION(i)(k) = 1
            }
            else {
              this.POPULATION(i)(k) = 0
            }
          }

          if (this.POPULATION(i)(k) != old_value) {
            flag = 1
          }

          if (flag == 1) {
            this.OBJECTIVE(i) = 10000000000
          }

          if (!Misc.in_the_range(this.POPULATION(i))) {
            this.POPULATION(i) = Misc.copy_chromosome(this.POPULATION2(i))
            this.OBJECTIVE(i) = this.OBJECTIVE2(i)
          }
        }
      }
    }
  }

  // Properties
  val POPULATION:ArrayBuffer[ArrayBuffer[Int]] = new ArrayBuffer[ArrayBuffer[Int]]()
  val POPULATION2:ArrayBuffer[ArrayBuffer[Int]] = new ArrayBuffer[ArrayBuffer[Int]]()
  val OBJECTIVE:ArrayBuffer[Double] = new ArrayBuffer[Double]()
  val OBJECTIVE2:ArrayBuffer[Double] = new ArrayBuffer[Double]()

  val VELOCITY:ArrayBuffer[ArrayBuffer[Double]] = new ArrayBuffer[ArrayBuffer[Double]]()
  var P_BEST_CHROMOSOME:ArrayBuffer[ArrayBuffer[Int]] = new ArrayBuffer[ArrayBuffer[Int]]()
  var P_BEST_OBJECTIVE:ArrayBuffer[Double] = new ArrayBuffer[Double]()
  var G_BEST_CHROMOSOME:ArrayBuffer[Int] = new ArrayBuffer[Int]()
  var G_BEST_OBJECTIVE:Double = 100000000
  var OC:Int = 0
  val save_best_object = 10000
}

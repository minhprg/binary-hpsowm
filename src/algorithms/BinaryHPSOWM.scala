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

  }

  def hpsowm_wm = {

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

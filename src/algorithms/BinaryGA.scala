package algorithms

import application.Configuration
import application.Misc
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.util.control.Breaks._

/**
 * Created by qmha on 10/28/14.
 */
class BinaryGA {
  def initialization_ga = {
    var i = 0
    var k = 0

    for (i <- 0 until Configuration.POP_SIZE) {
      for (k <- 0 until (Configuration.NUMBER_OF_INPUT  * (3 * Configuration.WEIGHT_BIT + 1))) {
        this.POPULATION2(i) += 0
      }
    }
  }

  def selection = {
    // Sorting using simple bubble sort
    var a:Double = 0
    var i:Int = 0
    var j:Int = 0

    for (i <- 0 until (Configuration.POP_SIZE - 1)) {
      for (j <- (i + 1) until Configuration.POP_SIZE) {
        if (this.OBJECTIVE(i) >= this.OBJECTIVE(j)) {
          var temp:ArrayBuffer[Int] = new ArrayBuffer[Int]()

          temp = Misc.copy_chromosome(this.POPULATION(i))
          this.POPULATION(i) = Misc.copy_chromosome(this.POPULATION(j))
          this.POPULATION(j) = temp

          a = this.OBJECTIVE(i)
          this.OBJECTIVE(i) = this.OBJECTIVE(j)
          this.OBJECTIVE(j) = a
        }
      }
    }

    // Save the best chromosome
    if (this.BEST_OBJECTIVE >= this.OBJECTIVE(0)) {
      this.BEST_CHROMOSOME = Misc.copy_chromosome(this.POPULATION(0))
      this.BEST_OBJECTIVE = this.OBJECTIVE(0)
    }

    var q:ArrayBuffer[Double] = new ArrayBuffer[Double]()
    q(0) = 1

    var r:Double = 0
    var t:Double = 0.999
    for (i <- 1 until Configuration.POP_SIZE) {
      t = t * t
      q(i) = q(i - 1) + t
    }

    for (i <- 0 until Configuration.POP_SIZE) {
      r = Random.nextDouble() % q(Configuration.POP_SIZE - 1)
      var j:Int = 0
      for (j <- 0 until Configuration.POP_SIZE) {
        if (r <= q(j)) {
          this.POPULATION2(i) = Misc.copy_chromosome(this.POPULATION(j))
          this.OBJECTIVE2(i) = this.OBJECTIVE(j)
          break
        }
      }
    }

    for (i <- 0 until Configuration.POP_SIZE) {
      this.POPULATION(i) = Misc.copy_chromosome(this.POPULATION2(i))
      this.OBJECTIVE(i) = this.OBJECTIVE2(i)
    }
  }

  def crossover = {
    var point_r:Int = 0
    var point_c:Int = 0
    var bit:Int = 0

    var i:Int = 0
    for (i <- 0 until Configuration.POP_SIZE) {
      var j:Int = 0
      for (j <- (i + 1) until Configuration.POP_SIZE) {
        // comparable to perl : next if (rand() > P_CROSSOVER)
        if (Random.nextDouble() <= Configuration.P_CROSSOVER) {
          this.POPULATION2(i) = this.POPULATION(i)
          this.OBJECTIVE2(i) = this.OBJECTIVE(i)

          this.POPULATION2(j) = this.POPULATION(j)
          this.OBJECTIVE2(j) = this.OBJECTIVE2(j)

          // crossover point
          point_c = Random.nextInt(Configuration.NUMBER_OF_INPUT * (3 * Configuration.WEIGHT_BIT.toInt + 1))

          // cross it!
          var l:Int = 0
          for (l <- point_c until (Configuration.NUMBER_OF_INPUT * (3 * Configuration.WEIGHT_BIT + 1))) {
            // swap the bit
            bit = this.POPULATION(i)(l)
            this.POPULATION(i)(l) = this.POPULATION(j)(l)
            this.POPULATION(j)(l) = bit
          }

          // crossover point
          point_c = Random.nextInt(Configuration.NUMBER_OF_INPUT * (3 * Configuration.WEIGHT_BIT.toInt + 1))

          // cross it!
          for (l <- point_c until (Configuration.NUMBER_OF_INPUT * (3 * Configuration.WEIGHT_BIT + 1))) {
            // swap the bit
            bit = this.POPULATION(i)(l)
            this.POPULATION(i)(l) = this.POPULATION(j)(l)
            this.POPULATION(j)(l) = bit
          }

          this.OBJECTIVE(i) = 10000000000
          this.OBJECTIVE(j) = 10000000000

          if (!Misc.in_the_range(this.POPULATION(i)) || !Misc.in_the_range(this.POPULATION(j))) {
            this.POPULATION(i) = this.POPULATION2(i)
            this.OBJECTIVE(i) = this.OBJECTIVE2(i)

            this.POPULATION(j) = this.POPULATION2(j)
            this.OBJECTIVE(j) = this.OBJECTIVE2(j)
          }
        }
      }
    }
  }

  def mutation = {
    var i:Int = 0

    for (i <- 0 until Configuration.POP_SIZE) {
      this.POPULATION2(i) = this.POPULATION(i)
      this.OBJECTIVE2(i) = this.OBJECTIVE(i)

      var l:Int = 0
      for (l <- 0 until (Configuration.NUMBER_OF_INPUT * (3 * Configuration.WEIGHT_BIT + 1))) {
        // comparable to Perl: next if (rand() > P_MUTATION)
        if (Random.nextDouble() <= Configuration.P_MUTATION) {
          if (this.POPULATION(i)(l) == 1) {
            this.POPULATION(i)(l) = 0
          }
          else {
            this.POPULATION(i)(l) = 1
          }

          this.OBJECTIVE(i) = 10000000000
        }
      }

      if (!Misc.in_the_range(this.POPULATION(i))) {
        this.POPULATION(i) = this.POPULATION2(i)
        this.OBJECTIVE(i) = this.OBJECTIVE2(i)
      }
    }
  }

  // Properties
  val POPULATION:ArrayBuffer[ArrayBuffer[Int]] = new ArrayBuffer[ArrayBuffer[Int]]()
  val POPULATION2:ArrayBuffer[ArrayBuffer[Int]] = new ArrayBuffer[ArrayBuffer[Int]]()
  val OBJECTIVE:ArrayBuffer[Double] = new ArrayBuffer[Double]()
  val OBJECTIVE2:ArrayBuffer[Double] = new ArrayBuffer[Double]()
  var BEST_CHROMOSOME:ArrayBuffer[Int] = new ArrayBuffer[Int]() // store the best result
  var BEST_OBJECTIVE:Double = 10000000000
  val save_best_obj = -1
}

package algorithms

import application.{Benchmark, Configuration, Misc}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * Created by qmha on 12/23/14.
 */
class BinaryGA2 {

  def initialization_ga = {
    var i:Int = 0
    var k:Int = 0

    for (i <- 0 until Configuration.POP_SIZE) {
      for (k <- 0 until (Configuration.NUMBER_OF_INPUT * Configuration.ENCODE_BIT)) {
        this.POPULATION2(i)(k) = 0
      }
    }
  }

  def selection = {
    // sort
    var a:Double = 0
    var i:Int = 0
    var j:Int = 0

    for (i <- 0 until (Configuration.POP_SIZE - 1)) {
      for (j <- (i + 1) until Configuration.POP_SIZE) {
        if (this.OBJECTIVE(i) >= this.OBJECTIVE(j)) {
          var temp:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.NUMBER_OF_INPUT * Configuration.ENCODE_BIT)(1)
          temp = Misc.copy_chromosome(this.POPULATION(i))
          this.POPULATION(i) = Misc.copy_chromosome(this.POPULATION(j))
          this.POPULATION(j) = Misc.copy_chromosome(temp)

          a = this.OBJECTIVE(i)
          this.OBJECTIVE(i) = this.OBJECTIVE(j)
          this.OBJECTIVE(j) = a
        }
      }
    }

    // save the best chromosome
    if (this.BEST_OBJECTIVE >= this.OBJECTIVE(0)) {
      this.BEST_CHROMOSOME = Misc.copy_chromosome(this.POPULATION(0))
      this.BEST_OBJECTIVE = this.OBJECTIVE(0)
    }

    // New population
    var q:ArrayBuffer[Double] = ArrayBuffer.fill(Configuration.POP_SIZE)(0)
    q(0) = 1

    var r:Double = 0
    var t:Double = 0.999
    for (i <- 1 until Configuration.POP_SIZE) {
      t = t * t
      q(i) = q(i - 1) + t
    }

    for (i <- 0 until Configuration.POP_SIZE) {
      // ???? perl is random (0,...)
      r = q(Configuration.POP_SIZE - 1) * Random.nextDouble()
      var j:Int = 0

      var is_break:Boolean = false
      for (j <- 0 until Configuration.POP_SIZE) {
        if (is_break == false && r <= q(j)) {
          this.POPULATION2(i) = Misc.copy_chromosome(this.POPULATION(j))
          this.OBJECTIVE2(i) = this.OBJECTIVE(j)
          is_break = true
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
          this.POPULATION2(i) = Misc.copy_chromosome(this.POPULATION(i))
          this.OBJECTIVE2(i) = this.OBJECTIVE(i)

          this.POPULATION2(j) = Misc.copy_chromosome(this.POPULATION(j))
          this.OBJECTIVE2(j) = this.OBJECTIVE2(j)

          // crossover point
          point_c = Random.nextInt(Configuration.NUMBER_OF_INPUT * Configuration.ENCODE_BIT)

          // cross it!
          var l:Int = 0
          for (l <- point_c until (Configuration.NUMBER_OF_INPUT * Configuration.ENCODE_BIT)) {
            // swap the bit
            bit = this.POPULATION(i)(l)
            this.POPULATION(i)(l) = this.POPULATION(j)(l)
            this.POPULATION(j)(l) = bit
          }

          // crossover point
          point_c = Random.nextInt(Configuration.NUMBER_OF_INPUT * Configuration.ENCODE_BIT)

          // cross it!
          for (l <- point_c until (Configuration.NUMBER_OF_INPUT * Configuration.ENCODE_BIT)) {
            // swap the bit
            bit = this.POPULATION(i)(l)
            this.POPULATION(i)(l) = this.POPULATION(j)(l)
            this.POPULATION(j)(l) = bit
          }

          this.OBJECTIVE(i) = 1000000000
          this.OBJECTIVE(j) = 1000000000

          if (!Misc.in_the_range(this.POPULATION(i)) || !Misc.in_the_range(this.POPULATION(j))) {
            this.POPULATION(i) = Misc.copy_chromosome(this.POPULATION2(i))
            this.OBJECTIVE(i) = this.OBJECTIVE2(i)

            this.POPULATION(j) = Misc.copy_chromosome(this.POPULATION2(j))
            this.OBJECTIVE(j) = this.OBJECTIVE2(j)
          }
        }
      }
    }
  }

  def mutation = {
    var i:Int = 0

    for (i <- 0 until Configuration.POP_SIZE) {
      this.POPULATION2(i) = Misc.copy_chromosome(this.POPULATION(i))
      this.OBJECTIVE2(i) = this.OBJECTIVE(i)

      var l:Int = 0
      for (l <- 0 until (Configuration.NUMBER_OF_INPUT * Configuration.ENCODE_BIT)) {
        // comparable to Perl: next if (rand() > P_MUTATION)
        if (Random.nextDouble() <= Configuration.P_MUTATION) {
          if (this.POPULATION(i)(l) == 1) {
            this.POPULATION(i)(l) = 0
          }
          else {
            this.POPULATION(i)(l) = 1
          }

          this.OBJECTIVE(i) = 1000000000
        }
      }

      if (!Misc.in_the_range(this.POPULATION(i))) {
        this.POPULATION(i) = Misc.copy_chromosome(this.POPULATION2(i))
        this.OBJECTIVE(i) = this.OBJECTIVE2(i)
      }
    }
  }

  def restore_population(filepath:String) = {
    val source = scala.io.Source.fromFile(filepath)
    val lines = source.getLines()

    var i:Int = 0
    lines.foreach(line => {
      if (line.charAt(0) != '[') {
        // Objective value
        this.OBJECTIVE(i) = line.toInt
      }
      else {
        // A chromosome need to be processed
        var newline:String = line.substring(1, line.length - 1)
        var ft = newline.split(',')
        ft = ft.map(f => f.trim)

        var count:Int = 0
        for (k <- 0 until Configuration.NUMBER_OF_INPUT) {
          var l:Int = 0
          for (l <- 0 until (3 * Configuration.WEIGHT_BIT)) {
            this.POPULATION(i)(k * (3 * Configuration.WEIGHT_BIT + 1) + l) = ft(count).toInt
            count = count + 1
          }

          this.POPULATION(i)(k * (3 * Configuration.WEIGHT_BIT + 1) + (3 * Configuration.WEIGHT_BIT)) = ft(count).toInt
          count = count + 1
        }

        i = i + 1
      }
    })
  }

  def evaluation_benchmark = {
    var output:Double = 0
    var i:Int = 0
    for (i <- 0 until Configuration.POP_SIZE) {
      // only process if it is new chromosome. Comparable to perl: next if objective != 1000000000
      if (this.OBJECTIVE(i) == 1000000000) {
        if (Configuration.BF == 1)
          output = Benchmark.f1(this.POPULATION(i))
        else if (Configuration.BF == 6)
          output = Benchmark.f6(this.POPULATION(i))
        else if (Configuration.BF == 12)
          output = Benchmark.f12(this.POPULATION(i))
        else if (Configuration.BF == 14)
          output = Benchmark.f14(this.POPULATION(i))
        else
          output = Benchmark.f15((this.POPULATION(i)))

        this.OBJECTIVE(i) = output
      }
    }
  }

  // Properties
  val POPULATION:ArrayBuffer[ArrayBuffer[Int]] =
    ArrayBuffer.fill(Configuration.POP_SIZE)(ArrayBuffer.fill(Configuration.NUMBER_OF_INPUT * Configuration.ENCODE_BIT)(1))
  val POPULATION2:ArrayBuffer[ArrayBuffer[Int]] =
    ArrayBuffer.fill(Configuration.POP_SIZE)(ArrayBuffer.fill(Configuration.NUMBER_OF_INPUT * Configuration.ENCODE_BIT)(1))
  val OBJECTIVE:ArrayBuffer[Double] =
    ArrayBuffer.fill(Configuration.POP_SIZE)(1000000000)
  val OBJECTIVE2:ArrayBuffer[Double] =
    ArrayBuffer.fill(Configuration.POP_SIZE)(1000000000)
  // store the best result
  var BEST_CHROMOSOME:ArrayBuffer[Int] = ArrayBuffer.fill(Configuration.NUMBER_OF_INPUT * Configuration.ENCODE_BIT)(1)
  var BEST_OBJECTIVE:Double = 1000000000
  val save_best_obj = -1
}

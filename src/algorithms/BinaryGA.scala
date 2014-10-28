package algorithms

import application.Configuration

import scala.collection.mutable.ArrayBuffer

/**
 * Created by qmha on 10/28/14.
 */
class BinaryGA {
  def initialization_ga = {
    var i = 0
    var k = 0

    for (i <- 0 to Configuration.POP_SIZE) {
      for (k <- 0 to (Configuration.NUMBER_OF_INPUT  * (3 * Configuration.WEIGHT_BIT + 1))) {
        this.POPULATION2(i) += 0
      }
    }
  }

  def selection = {
    // Sort

    // Save the best chromosome
  }

  def crossover = {

  }

  def mutation = {

  }

  // Properties
  val POPULATION:ArrayBuffer[ArrayBuffer[Int]] = new ArrayBuffer[ArrayBuffer[Int]]()
  val POPULATION2:ArrayBuffer[ArrayBuffer[Int]] = new ArrayBuffer[ArrayBuffer[Int]]()
  val OBJECTIVE:ArrayBuffer[ArrayBuffer[Int]] = new ArrayBuffer[ArrayBuffer[Int]]()
  val OBJECTIVE2:ArrayBuffer[ArrayBuffer[Int]] = new ArrayBuffer[ArrayBuffer[Int]]()
  val save_best_obj = -1
}

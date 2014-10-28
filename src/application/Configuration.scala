package application

/**
 * Created by qmha on 10/28/14.
 */
object Configuration {
  // Benchmark function
  val BF = 15

  val NUMBER_OF_INPUT = 30
  val NUMBER_OF_STAGE = 0.5

  val ENCODE_BIT = 10
  var WEIGHT_BIT:Double = (ENCODE_BIT - 1) / 3
  val DECIMAL = 2 // the number of figures after the floating point

  val X_MAX = 10.24
  val X_MIN = -5.12
  val OPTIMAL = 0 // optimal solution

  // Settings of evolution
  val POP_SIZE = 50

  // Settings of GA
  val P_MUTATION = 0.001
  val P_CROSSOVER = 0.001

  // Settings of HPSOWM
  val FIT_1 = 3.0001
  val FIT_2 = 1.0001
  val WW_MIN = 0.2
  val WW_MAX = 0.5
  val P_M = 0.001
  val XI_WM = 1
  val GG = 1000
  val PARA_MAX = 1
  val PARA_MIN = 0
  val V_MAX = 5
  val KKKK = 4 // ???
  val GAMA = 1

  // Running time
  val MAX_TIME = 60 * 60 * 24 * 7 / 200

  // Settings of ROC evalution
  val THRESHOLD = 0
}
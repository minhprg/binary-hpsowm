package application

import algorithms.BinaryGA
import algorithms.BinaryGA2
import scala.util.control.Breaks._

/**
 * Created by qmha on 10/28/14.
 */
object TrainBinaryGA {
  def main(args:Array[String]):Unit = {
    // Arguments checking
    if (args.length == 4) {
      Configuration.BF = args(0).toInt
      Configuration.OPTIMAL = args(1).toDouble
      Configuration.X_MIN = args(2).toDouble
      Configuration.X_MAX = args(3).toDouble
    }

    // Start
    var start_time = System.currentTimeMillis / 1000
    var ga_agent:BinaryGA2 = new BinaryGA2

    ga_agent.restore_population(Configuration.FILEPATH)
    ga_agent.initialization_ga

    var iter:Int = 0
    var is_break:Boolean = false
    var last_time:Long = 0

    while (is_break == false) {
      iter = iter + 1

      ga_agent.evaluation_benchmark

      ga_agent.selection
      ga_agent.mutation
      ga_agent.crossover

      ga_agent.POPULATION(0) = Misc.copy_chromosome(ga_agent.BEST_CHROMOSOME)
      ga_agent.OBJECTIVE(0) = ga_agent.BEST_OBJECTIVE

      var current_time = (System.currentTimeMillis / 1000) - start_time

      // only print out if it is the next second
      //if (current_time != last_time)
      println(current_time + " " + iter + " " + ga_agent.BEST_OBJECTIVE)

      if (current_time > Configuration.MAX_TIME && is_break == false) {
        is_break = true
      }

      if (ga_agent.BEST_OBJECTIVE <= Configuration.OPTIMAL && is_break == false) {
        is_break = true
      }

      // update the last time
      last_time = current_time
    }

  }
}

package application

import algorithms.BinaryGA
import scala.util.control.Breaks._

/**
 * Created by qmha on 10/28/14.
 */
object TrainBinaryGA {
  def main(args:Array[String]):Unit = {
    var start_time = System.currentTimeMillis / 1000
    var ga_agent:BinaryGA = new BinaryGA

    ga_agent.restore_population(Configuration.FILEPATH)
    ga_agent.initialization_ga

    var iter:Int = 0
    var is_break:Boolean = false

    while (is_break == false) {
      iter = iter + 1

      ga_agent.evaluation_benchmark

      ga_agent.selection
      ga_agent.mutation
      ga_agent.crossover

      ga_agent.POPULATION(0) = Misc.copy_chromosome(ga_agent.BEST_CHROMOSOME)
      ga_agent.OBJECTIVE(0) = ga_agent.BEST_OBJECTIVE

      var current_time = (System.currentTimeMillis / 1000) - start_time

      println(current_time + " " + iter + " " + ga_agent.BEST_OBJECTIVE)

      if (current_time > Configuration.MAX_TIME && is_break == false) {
        is_break = true
      }

      if (ga_agent.BEST_OBJECTIVE <= Configuration.OPTIMAL && is_break == false) {
        is_break = true
      }
    }

  }
}

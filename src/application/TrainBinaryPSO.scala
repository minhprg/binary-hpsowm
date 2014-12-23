package application

import algorithms.BinaryPSO

/**
 * Created by qmha on 10/28/14.
 */
object TrainBinaryPSO {
  def main(args:Array[String]):Unit = {
    // Arguments checking
    if (args.length == 2) {
      Configuration.BF = args(0).toInt
      Configuration.OPTIMAL = args(1).toDouble
    }

    // Start
    var start_time = System.currentTimeMillis / 1000
    var bpso_agent = new BinaryPSO

    bpso_agent.restore_population(Configuration.FILEPATH)
    bpso_agent.initialization_bpso()

    var iter:Int = 0
    var is_break:Boolean = false

    while (is_break == false) {
      iter = iter + 1

      bpso_agent.evaluation_benchmark
      bpso_agent.bpso_save_best()

      // >????? NEED CONFIRM
      var current_time:Int = (System.currentTimeMillis() / 1000 - start_time).toInt
      var iot:Double = current_time / Configuration.MAX_TIME

      bpso_agent.bpso_velocity(iot)
      bpso_agent.bpso_update()

      println(current_time + " " + iter + " " + bpso_agent.G_BEST_OBJECTIVE)

      if (current_time > Configuration.MAX_TIME && is_break == false) {
        is_break = true
      }

      if (bpso_agent.G_BEST_OBJECTIVE <= Configuration.OPTIMAL && is_break == false) {
        is_break = true
      }
    }
  }
}

package application

import algorithms.BinaryHPSOWM

/**
 * Created by qmha on 10/28/14.
 */
object TrainBinaryHPSOWM {
  def main(args:Array[String]):Unit = {
    var start_time = System.currentTimeMillis / 1000
    var bhpsowm_agent = new BinaryHPSOWM

    bhpsowm_agent.restore_population(Configuration.FILEPATH)
    bhpsowm_agent.initialization_hpsowm

    var iter:Int = 0
    var is_break:Boolean = false

    while (is_break == false) {
      iter = iter + 1

      bhpsowm_agent.evaluation_benchmark
      bhpsowm_agent.hpsowm_save_best

      // >????? NEED CONFIRM
      var current_time:Int = (System.currentTimeMillis() / 1000 - start_time).toInt
      var iot:Double = current_time / Configuration.MAX_TIME

      bhpsowm_agent.hpsowm_velocity(iot)
      bhpsowm_agent.hpsowm_update
      bhpsowm_agent.hpsowm_wm(iot)

      println(current_time + " " + iter + " " + bhpsowm_agent.G_BEST_OBJECTIVE)

      if (current_time > Configuration.MAX_TIME && is_break == false) {
        is_break = true
      }

      if (bhpsowm_agent.G_BEST_OBJECTIVE <= Configuration.OPTIMAL && is_break == false) {
        is_break = true
      }
    }
  }
}

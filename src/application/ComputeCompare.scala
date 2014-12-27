package application

import scala.collection.mutable.ArrayBuffer
import scala.math._

/**
 * Created by qmha on 10/30/14.
 */
object ComputeCompare {
  def main(args:Array[String]):Unit = {
    if (args.length == 2) {
      Configuration.BF = args(0).toInt
      Configuration.OPTIMAL = args(1).toDouble
    }

    Configuration.FILE_RESULT_PREFIX = "/Users/qmha/projects/bhpsowm/src/cmp_result/" + Configuration.BF + "/"

    var start:Int = 0
    var run:Int = 50
    var time_max = Configuration.MAX_TIME
    var iter_max = 1000000
    var obj_min:Double = Configuration.OPTIMAL

    // Arguments processing
    if (args.length == 2) {
      if (args(0).toInt == 1) {
        time_max = args(1).toInt
      }
      else if (args(0) == 2) {
        iter_max = args(1).toInt
      }
      else {
        obj_min = (args(1).toDouble / 20)
      }
    }

    // Time variables
    var time_ga:ArrayBuffer[Int] = new ArrayBuffer[Int]()
    var iter_ga:ArrayBuffer[Int] = new ArrayBuffer[Int]()
    var obj_ga:ArrayBuffer[Double] = new ArrayBuffer[Double]()

    var time_bpso:ArrayBuffer[Int] = new ArrayBuffer[Int]()
    var iter_bpso:ArrayBuffer[Int] = new ArrayBuffer[Int]()
    var obj_bpso:ArrayBuffer[Double] = new ArrayBuffer[Double]()

    var time_hpsowm:ArrayBuffer[Int] = new ArrayBuffer[Int]()
    var iter_hpsowm:ArrayBuffer[Int] = new ArrayBuffer[Int]()
    var obj_hpsowm:ArrayBuffer[Double] = new ArrayBuffer[Double]()

    var i:Int = 0
    var is_last:Boolean = false

    // GA
    for (i <- start until (start + run)) {
      // Open file
      val source = scala.io.Source.fromFile(Configuration.FILE_RESULT_PREFIX + "ga_result_" + i)
      val lines = source.getLines()

      var time_save:Int = 0
      var iter_save:Int = 0
      var obj_save:Double = 0

      // Get each line
      lines.foreach(line => {
        var splitter = line.split(" ")
        var time:Int = splitter(0).toInt
        var iter:Int = splitter(1).toInt
        var obj:Double = Misc.parseDouble(splitter(2)).toString.toDouble

        if (time <= time_max && iter <= iter_max && obj >= obj_min) {
          time_save = time
          iter_save = iter
          obj_save = obj
        }
      })

      time_ga += time_save
      iter_ga += iter_save
      obj_ga += obj_save
    }


    // BPSO
    for (i <- start until (start + run)) {
      // Open file
      val source = scala.io.Source.fromFile(Configuration.FILE_RESULT_PREFIX + "bpso_result_" + i)
      val lines = source.getLines()

      var time_save:Int = 0
      var iter_save:Int = 0
      var obj_save:Double = 0

      // Get each line
      lines.foreach(line => {
        var splitter = line.split(" ")
        var time:Int = splitter(0).toInt
        var iter:Int = splitter(1).toInt
        var obj:Double = Misc.parseDouble(splitter(2)).toString.toDouble

        if (time <= time_max && iter <= iter_max && obj >= obj_min) {
          time_save = time
          iter_save = iter
          obj_save = obj
        }
      })

      time_bpso += time_save
      iter_bpso += iter_save
      obj_bpso += obj_save
    }



    // HPSOWM
    for (i <- start until (start + run)) {
      // Open file
      val source = scala.io.Source.fromFile(Configuration.FILE_RESULT_PREFIX + "hpsowm_result_" + i)
      val lines = source.getLines()

      var time_save:Int = 0
      var iter_save:Int = 0
      var obj_save:Double = 0

      // Get each line
      lines.foreach(line => {
        var splitter = line.split(" ")
        var time:Int = splitter(0).toInt
        var iter:Int = splitter(1).toInt
        var obj:Double = Misc.parseDouble(splitter(2)).toString.toDouble

        if (time <= time_max && iter <= iter_max && obj >= obj_min) {
          time_save = time
          iter_save = iter
          obj_save = obj
        }
      })

      time_hpsowm += time_save
      iter_hpsowm += iter_save
      obj_hpsowm += obj_save
    }


    // Collect the stat
    var ga_time:ArrayBuffer[Double] = this.compute_stat(time_ga)
    var ga_obj:ArrayBuffer[Double] = this.compute_stat(obj_ga)

    var bpso_time:ArrayBuffer[Double] = this.compute_stat(time_bpso)
    var bpso_obj:ArrayBuffer[Double] = this.compute_stat(obj_bpso)

    var hpsowm_time:ArrayBuffer[Double] = this.compute_stat(time_hpsowm)
    var hpsowm_obj:ArrayBuffer[Double] = this.compute_stat(obj_hpsowm)

    print("GA (time = " + time_max + ") \t time_mean:" + ga_time(0) + "\t time_best: " + ga_time(1) + " \t time_dev: " + ga_time(3))
    println()
    print("BPSO (time = " + time_max + ") \t time_mean:" + bpso_time(0) + "\t time_best: " + bpso_time(1) + " \t time_dev: " + bpso_time(3))
    println()
    print("HPSOWM (time = " + time_max + ") \t time_mean:" + hpsowm_time(0) + "\t time_best: " + hpsowm_time(1) + " \t time_dev: " + hpsowm_time(3))
    println()
    println("==============================================================================================================")
    println()
    print("GA (obj = " + obj_min + ") \t obj_mean:" + ga_obj(0) + "\t obj_best: " + ga_obj(2) + " \t obj_dev: " + ga_obj(3))
    println()
    print("BPSO (obj = " + obj_min + ") \t obj_mean:" + bpso_obj(0) + "\t obj_best: " + bpso_obj(2) + " \t obj_dev: " + bpso_obj(3))
    println()
    print("HPSOWM (obj = " + obj_min + ") \t obj_mean:" + hpsowm_obj(0) + "\t obj_best: " + hpsowm_obj(2) + " \t obj_dev: " + hpsowm_obj(3))
    println()
  }

  def compute_stat(data:ArrayBuffer[_]):ArrayBuffer[Double] = {
    var total:Double = 0
    var max:Double = -10000000
    var min:Double = 1000000000
    var dev:Double = 0
    var count:Int = 0

    data.foreach(v => {
      count = count + 1
      total = total + Misc.parseDouble(v.toString).toString.toDouble
      if (Misc.parseDouble(v.toString).toString.toDouble > max)
        max = Misc.parseDouble(v.toString).toString.toDouble
      if (Misc.parseDouble(v.toString).toString.toDouble < min)
        min = Misc.parseDouble(v.toString).toString.toDouble
    })

    var mean:Double = total / count

    data.foreach(v => {
      dev += (v.toString.toDouble - mean) * (v.toString.toDouble - mean)
    })

    dev = sqrt(dev / count)

    var result:ArrayBuffer[Double] = new ArrayBuffer[Double]()
    /*
    result += ((mean * 10000 + 0.555555) / 10000)//.toInt
    result += ((max * 10000 + 0.555555) / 10000)//.toInt
    result += ((min * 10000 + 0.555555) / 10000)//.toInt
    result += ((dev * 10000 + 0.555555) / 10000)//.toInt
    */
    result += ((mean * 10000) / 10000)//.toInt
    result += ((max * 10000) / 10000)//.toInt
    result += ((min * 10000) / 10000)//.toInt
    result += ((dev * 10000) / 10000)//.toInt

    return result
  }
}

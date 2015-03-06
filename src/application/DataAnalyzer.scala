package application

import scala.collection.mutable.ArrayBuffer

/**
 * @author qmha
 */
object DataAnalyzer extends App {
  var prefix:String = "/Users/qmha/projects/bhpsowm/src/analysis_result/"
  var folder:String = "/Users/qmha/projects/bhpsowm/src/cmp_result/"

  // run it!
  run

  /**
   * Methods
   */
  def run = {
    println("Running....")
    for (i <- 1 to 1) {
      // process
      setProcessing(i)

      // test
      /*
      var counter:Int = 0
      for (i <- 1 until timeGA.length) {
        println(timeGA(i) + " " + iterationGA(i) + " " + objectiveGA(i))
        if (timeGA(i) != 0)
          counter += 1
      }
      println("Counter = " + counter)
      */

      // write to file

    }

    println("Done!")
  }

  // Each set (3 algorithms) processing
  def setProcessing(set:Int) = {
    var tmp:Tuple3[ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Double]] = null

    tmp = algoProcessing("ga_result_", set, 0)
    timeGA = tmp._1
    iterationGA = tmp._2
    objectiveGA = tmp._3

    tmp = algoProcessing("bpso_result_", set, 1)
    timePSO = tmp._1
    iterationPSO = tmp._2
    objectivePSO = tmp._3

    tmp = algoProcessing("hpsowm_result_", set, 2)
    timeHPSOWM = tmp._1
    iterationHPSOWM = tmp._2
    objectiveHPSOWM = tmp._3
  }

  // Each algorithm processing
  def algoProcessing(prefix: String, set:Int, algo:Int):(ArrayBuffer[Int], ArrayBuffer[Int], ArrayBuffer[Double]) = {
    var timeAverage:ArrayBuffer[Int] = ArrayBuffer.fill(20000)(0)
    var iterationAverage:ArrayBuffer[Int] = ArrayBuffer.fill(20000)(0)
    var objectiveAverage:ArrayBuffer[Double] = ArrayBuffer.fill(20000)(0.0)

    for (i <- 0 until 50) {
      var i:Int = 0
      // Each file
      import scala.io.Source
      for(line <- Source.fromFile(this.folder + set + "/" + prefix + i).getLines()) {
        // each line
        val elements = line.trim.split("\\s+") // 0 - time, 1 - iteration, 2 - value

        timeAverage(i) += Integer.parseInt(elements(0))
        iterationAverage(i) += Integer.parseInt(elements(1))
        objectiveAverage(i) += Misc.parseDouble(elements(2)).toString.toDouble

        i += 1
      }
    }

    return (timeAverage.map(_ / 50), iterationAverage.map(_ / 50), objectiveAverage.map(_ / 50))
  }

  // set 1 -> 18
  // algo: 0 - ga, 1 - bpso, 2- bhpsowm
  def readFile(set:Int, algo:Int) = {

  }

  def writeFile(set:Int, algo:Int) = {

  }

  /**
   * Attributes
   */

  var timeGA:ArrayBuffer[Int] = new ArrayBuffer[Int]()
  var iterationGA:ArrayBuffer[Int] = new ArrayBuffer[Int]()
  var objectiveGA:ArrayBuffer[Double] = new ArrayBuffer[Double]()
  var timePSO:ArrayBuffer[Int] = new ArrayBuffer[Int]()
  var iterationPSO:ArrayBuffer[Int] = new ArrayBuffer[Int]()
  var objectivePSO:ArrayBuffer[Double] = new ArrayBuffer[Double]()
  var timeHPSOWM:ArrayBuffer[Int] = new ArrayBuffer[Int]()
  var iterationHPSOWM:ArrayBuffer[Int] = new ArrayBuffer[Int]()
  var objectiveHPSOWM:ArrayBuffer[Double] = new ArrayBuffer[Double]()
}

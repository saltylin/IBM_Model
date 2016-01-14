import java.io.File
import java.io.PrintWriter

import scala.io.Source

import salty._

object IBMModel {
  def main(args: Array[String]) {
    val chFile = "../resource/test.ch.txt"
    val enFile = "../resource/test.en.txt"
    val chSen = Source.fromFile(chFile).getLines.toArray[String]
    val enSen = Source.fromFile(enFile).getLines.toArray[String]
    val model = new Model(chSen, enSen)
    println("Training...")
    model.training
    val result = model.test
    val resultFile = "../resource/result.txt"
    val writer = new PrintWriter(new File(resultFile))
    result.foreach(x => {
        writer.print("0-" + x(0))
        for (i <- 1 until x.length) {
          writer.print(" " + i + "-" + x(i))
        }
        writer.println()
      }
    )
    writer.close
    val (precision, recall, fMeasure) = validate(result)
    println(f"Precision: $precision%#.4f\nRecall: $recall%#.4f\nF-Measure: $fMeasure%#.4f\n")
  }

  def validate(result: Array[Array[Int]]) = {
    val alignFile = "../resource/test.align.txt"
    var realTotal = 0
    var testTotal = 0
    var rightTotal = 0
    val realAlignStr = Source.fromFile(alignFile).getLines.toArray[String]
    for (k <- 0 until result.length) {
      val realAlign = realAlignStr(k).split(" ")
      realTotal += realAlign.length
      testTotal += result(k).length
      realAlign.foreach(x => {
          val indexStr = x.split("-")
          val chIndex = indexStr(0).toInt
          val enIndex = indexStr(1).toInt
          if (enIndex == result(k)(chIndex)) {
            rightTotal += 1
          }
        }
      )
    }
    val precision = rightTotal.toDouble / testTotal
    val recall = rightTotal.toDouble / realTotal
    val fMeasure = 2.0 / (1 / precision + 1 / recall)
    (precision, recall, fMeasure)
  }
}

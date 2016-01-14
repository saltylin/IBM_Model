package salty {
  import scala.collection.mutable.HashMap
  import scala.collection.mutable.Map
  import scala.collection.mutable.Set
  class Transfer {
    private var enMap = new HashMap[String, Map[String, Double]]
    def this(chSet: Set[String], enSet: Set[String]) {
      this()
      val chSetSize = chSet.size
      val initPr = 1.0 / chSetSize
      for (enWord <- enSet) {
        val prMap = new HashMap[String, Double]
        for (chWord <- chSet) {
          prMap(chWord) = initPr
        }
        enMap(enWord) = prMap
      }
    }

    def getPr(chWord: String, enWord: String) = {
      enMap(enWord)(chWord)
    }

    def setPr(chWord: String, enWord: String, pr: Double) {
      enMap(enWord)(chWord) = pr
    }
  }

  class Distortion(val maxL: Int, val maxM: Int) {
    private var distPr = Dim4Array.generate(maxL, maxM)

    def getPr(i: Int, j: Int, l: Int, m: Int) = {
      distPr(m)(l)(j)(i)
    }

    def setPr(i: Int, j: Int, l: Int, m: Int, pr: Double) {
      distPr(m)(l)(j)(i) = pr
    }
  }

  object Dim4Array {
    def generate(maxL:Int, maxM: Int) = {
      val result = new Array[Array[Array[Array[Double]]]](maxM)
      for (i <- 0 until maxM) {
        result(i) = new Array[Array[Array[Double]]](maxL)
        for (j <- 0 until maxL) {
          result(i)(j) = new Array[Array[Double]](i + 1)
          for (k <- 0 to i) {
            result(i)(j)(k) = new Array[Double](j + 1)
            for (v <- 0 to j) {
              result(i)(j)(k)(v) = 1.0 / (1 + j)
            }
          }
        }
      }
      result
    }
  }

}

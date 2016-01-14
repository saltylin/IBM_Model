package salty {
  import scala.collection.mutable.Map
  import scala.collection.mutable.HashMap
  import scala.collection.mutable.ArrayBuffer
  import scala.collection.mutable.Set
  import scala.collection.mutable.HashSet
  class Model(val chSen: Array[String], val enSen: Array[String]) {
    val totalNum = chSen.length
    val chAllWord = chSen.map(x => x.split(" "))
    val enAllWord = enSen.map(x => x.split(" "))
    private var chWordSet = new HashSet[String]
    private var enWordSet = new HashSet[String]
    private var maxM = 0
    private var maxL = 0
    init
    var T = new Transfer(chWordSet, enWordSet)
    var D = new Distortion(maxL, maxM)

    def init {
      for (t <- chAllWord) {
        if (t.length > maxM) {
          maxM = t.length
        }
        for (w <- t) {
          chWordSet += w
        }
      }
      for (t <- enAllWord) {
        if (t.length > maxL) {
          maxL = t.length
        }
        for (w <- t) {
          enWordSet += w
        }
      }
    }

    def training {
      val a = generateA
      val tcount = generateTcount
      val scount = generateScount
      val acount = generateAcount
      val bcount = generateBcount
      for (iter <- 1 to 100) {
        /**
         * Calculate a
         */
        for (k <- 0 until totalNum) {
          val l = enAllWord(k).length
          val m = chAllWord(k).length
          for (j <- 0 until m) {
            var tmpSum = 0.0
            for (i <- 0 until l) {
              tmpSum += D.getPr(i, j, l - 1, m - 1) * T.getPr(chAllWord(k)(j), enAllWord(k)(i))
            }
            for (i <- 0 until l) {
              a(k)(j)(i) = D.getPr(i, j, l - 1, m - 1) * T.getPr(chAllWord(k)(j), enAllWord(k)(i)) / tmpSum
            }
          }
        }
        /**
         * Let tcount and scount to be zeros
         */
        for (enWord <- enWordSet) {
          scount(enWord) = 0.0
          for (chWord <- chWordSet) {
            tcount(enWord)(chWord) = 0.0
          }
        }
        /**
         * Calculate tcount and scount
         */
        for (k <- 0 until totalNum) {
          for (j <- 0 until chAllWord(k).length) {
            for (i <- 0 until enAllWord(k).length) {
              tcount(enAllWord(k)(i))(chAllWord(k)(j)) += a(k)(j)(i)
              scount(enAllWord(k)(i)) += a(k)(j)(i)
            }
          }
        }
        /**
         * Let acount and bcount to be zeros
         */
        for (m <- 0 until maxM) {
          for (l <- 0 until maxL) {
            for (j <- 0 to m) {
              bcount(m)(l)(j) = 0.0
              for (i <- 0 to l) {
                acount(m)(l)(j)(i) = 0.0
              }
            }
          }
        }
        /**
         * Calculate acount and bcount
         */
        for (k <- 0 until totalNum) {
          val m = chAllWord(k).length
          val l = enAllWord(k).length
          for (j <- 0 until m) {
            bcount(m - 1)(l - 1)(j) += 1
            for (i <- 0 until l) {
              acount(m - 1)(l - 1)(j)(i) += a(k)(j)(i)
            }
          }
        }
        /**
         * Update T
         */
        val smoothCoeT = 0.002
        val smoothCoeD = 0.005
        val chWordNum = chWordSet.size
        for (enWord <- enWordSet) {
          val y = scount(enWord) + chWordNum * smoothCoeT
          for (chWord <- chWordSet) {
            val x = tcount(enWord)(chWord) + smoothCoeT
            T.setPr(chWord, enWord, x / y)
          }
        }
        /**
         * Update D
         */
        for (m <- 0 until maxM) {
          for (l <- 0 until maxL) {
            for (j <- 0 to m) {
              val y = bcount(m)(l)(j) + (1 + l) * smoothCoeD
              for (i <- 0 to l) {
                val x = acount(m)(l)(j)(i) + smoothCoeD
                D.setPr(i, j, l, m, x / y)
              }
            }
          }
        }
      }
    }

    def test = {
      val result = new Array[Array[Int]](totalNum)
      for (k <- 0 until totalNum) {
        val chWords = chAllWord(k)
        val enWords = enAllWord(k)
        result(k) = new Array[Int](chWords.length)
        for (j <- 0 until chWords.length) {
          result(k)(j) = align(j, chWords, enWords)
        }
      }
      result
    }

    def align(j: Int, chWords: Array[String], enWords: Array[String]) = {
      val l = enWords.length
      val m = chWords.length
      var maxPr = 0.0
      var maxA = -1
      for (i <- 0 until l) {
        val tmpPr = D.getPr(i, j, l - 1, m - 1) * T.getPr(chWords(j), enWords(i))
        if (tmpPr > maxPr) {
          maxPr = tmpPr
          maxA = i
        }
      }
      maxA
    }

    def generateA = {
      val a = new Array[Array[Array[Double]]](totalNum)
      for (i <- 0 until totalNum) {
        a(i) = new Array[Array[Double]](chAllWord(i).length)
        for (j <- 0 until chAllWord(i).length) {
          a(i)(j) = new Array[Double](1 + enAllWord(i).length)
        }
      }
      a
    }

    def generateTcount = {
      val tcount = new HashMap[String, Map[String, Double]]
      for (enWord <- enWordSet) {
        val tmpMap = new HashMap[String, Double]
        for (chWord <- chWordSet) {
          tmpMap(chWord) = 1.0
        }
        tcount(enWord) = tmpMap
      }
      tcount
    }

    def generateScount = {
      val scount = new HashMap[String, Double]
      for (enWord <- enWordSet) {
        scount(enWord) = 1.0
      }
      scount
    }

    def generateAcount = {
      Dim4Array.generate(maxL, maxM)
    }

    def generateBcount = {
      val bcount = new Array[Array[Array[Double]]](maxM)
      for (i <- 0 until maxM) {
        bcount(i) = new Array[Array[Double]](maxL)
        for (j <- 0 until maxL) {
          bcount(i)(j) = new Array[Double](1 + i)
        }
      }
      bcount
    }
  }
}

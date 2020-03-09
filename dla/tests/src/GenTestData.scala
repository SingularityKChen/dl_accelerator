package dla.tests

import dla.cluster.GNMFCS2Config
import dla.pe.{MCRENFConfig, PESizeConfig, SPadSizeConfig}
import org.scalatest._

import scala.math.pow
import scala.util.Random
class GenOnePETestDataTest extends FlatSpec {
  val genHp = new GenOnePETestData
  private val inActList: List[List[Int]] = genHp.inActList
  private val weightList: List[List[Int]] = genHp.weightList
  private val inInActAdrRand = genHp.inInActAdrRand
  private val inInActCountRand = genHp.inInActCountRand
  private val inInActDataRand = genHp.inInActDataRand
  private val inWeightAdrRand = genHp.inWeightAdrRand
  private val inWeightCountRand = genHp.inWeightCountRand
  private val inWeightDataRand = genHp.inWeightDataRand
  private val outPSumRand: List[Int] = genHp.outPSumRand
  println("inInActAdrRand    = " + inInActAdrRand)
  println("inInActCountRand  = " + inInActCountRand)
  println("inInActDataRand   = " + inInActDataRand)
  println("inWeightAdrRand   = " + inWeightAdrRand)
  println("inWeightCountRand = " + inWeightCountRand)
  println("inWeightDataRand  = " + inWeightDataRand)
  println("outPSumRand       = " + outPSumRand)
  println("inActList         = " + inActList)
  println("weightList        = " + weightList)
  val oneStreamTest = new GenOneStreamData
  println("inActStream = " + oneStreamTest.inActStream)
  println("pSumStream  = " + oneStreamTest.outPSumStream)
}

class GenFunc extends PESizeConfig with SPadSizeConfig with MCRENFConfig with GNMFCS2Config {
  protected val pSumMax: Int = pow(2, psDataWidth).toInt
  protected val inActAdrMax: Int = pow(2, inActAdrWidth).toInt
  protected val weightAdrMax: Int = pow(2, weightAdrWidth).toInt
  protected val scsDataMax: Int = pow(2, cscDataWidth).toInt
  protected val weightStreamNum: Int = G2*M2*C2*S2
  protected val inActStreamNum: Int = G2*N2*F2*C2*S2
  protected val pSumStreamNum: Int = G2*N2*M2*F2
  protected val pSumOneSPadNum: Int = M0*E*N0*F0
  protected def genSparse(rows: Int, cols: Int, max: Int, ratio: Double): List[List[Int]] = {
    require(ratio <= 1 && ratio >= 0, "the range of ratio should be (0, 1)")
    var resultList: List[List[Int]] = Nil
    for (_ <- 0 until rows) {
      var temRowList: List[Int] = Nil
      while (temRowList.length < cols) {
        // use random to get sparse matrix
        val randomNum = if ((new Random).nextInt(10000) >= ratio*10000) (new Random).nextInt(max - 1) + 1 else 0
        temRowList = temRowList:::List(randomNum)
      }
      resultList = resultList:::List(temRowList)
    }
    resultList
  }
  protected def goldenCovResult(kernel: List[List[Int]], inAct: List[List[Int]]): List[List[Int]] = {
    // to compute convolution golden results
    val inActRowNum = inAct.length
    val inActColNum = inAct.head.length
    val weightRowNum = kernel.length
    val weightColNum = kernel.head.length
    require(weightRowNum <= inActRowNum && weightColNum <= inActColNum, "kernel's size should less than inAct\n" +
      s"$weightColNum < $inActColNum ?\n" +
      s"$weightRowNum < $inActRowNum")
    val pSumColNum = inActColNum - weightColNum + 1
    val pSumRowNum = inActRowNum - weightRowNum + 1
    var resultList: List[List[Int]] = Nil
    for (pCol <- 0 until pSumColNum) {
      var temRowList: List[Int] = Nil
      for (pRow <- 0 until pSumRowNum) {
        var temResult: Int = 0
        for (wCol <- 0 until weightColNum) {
          for (wRow <- 0 until weightRowNum) {
            temResult = temResult + kernel(wRow)(wCol)*inAct(wRow + pRow)(wCol + pCol)
          }
        }
        temRowList = temRowList:::List(temResult)
      }
      resultList = resultList:::List(temRowList)
    }
    resultList
  }
  protected def goldenResult(listA: List[List[Int]], listB: List[List[Int]]): List[List[Int]] = {
    // to compute multiply golden results
    require(listB.length == listA.head.length, "listA's column number should equal to listB's row number")
    val rowNum = listA.length
    val colNum = listB.head.length
    val addTimes = listB.length
    var resultList: List[List[Int]] = Nil
    for (i <- 0 until rowNum) {
      var temRowList: List[Int] = Nil
      for (j <- 0 until colNum) {
        var temResult: Int = 0
        for (k <- 0 until addTimes) {
          temResult = temResult + listA(i)(k) * listB(k)(j)
        }
        temRowList = temRowList:::List(temResult)
      }
      resultList = resultList:::List(temRowList)
    }
    resultList
  }
  protected def goldenFlatResult(listA: List[List[Int]], listB: List[List[Int]]): List[Int] = {
    // translate from every row to every column read
    val tempResult: List[List[Int]] = goldenResult(listA, listB)
    var resultList: List[Int] = Nil
    for (j <- tempResult.head.indices) {
      val currentCol = tempResult.map(x => x(j))
      resultList = resultList:::currentCol
    }
    resultList
  }
  protected def genAdrCountData(listA: List[List[Int]], inActOrWeight: Boolean): Seq[List[Int]] = {
    val zeroCode: Int = if (inActOrWeight) inActZeroColumnCode else weightZeroColumnCode
    var adrList: List[Int] = Nil
    var countList: List[Int] = Nil
    var dataList: List[Int] = Nil
    for (j <- listA.head.indices) { // for column
      val currentCol = listA.map(x => x(j))
      if (currentCol.max == 0){ // if zero column
        adrList = adrList:::List(zeroCode)
        //println(s"meet a zero column at column $j")
      } else {
        if (j != 0) {
          adrList = adrList:::List(dataList.length)
        }
        for (i <- currentCol.indices) { // for row
          if (currentCol(i) != 0){
            countList = countList:::List(i)
            dataList = dataList:::List(currentCol(i))
          }
        }
      }
    }
    // add zero signal to their end
    adrList = adrList:::List(0)
    countList = countList:::List(0)
    dataList = dataList:::List(0)
    Seq(adrList, countList, dataList)
  }
}

class GenOnePETestData extends GenFunc {
  val (inActSeq, weightSeq, outPSumRand, inActWeightList) = genAll()
  val inActList: List[List[Int]] = inActWeightList.head
  val weightList: List[List[Int]] = inActWeightList.last
  val inInActAdrRand: List[Int] = inActSeq.head
  val inInActCountRand: List[Int] = inActSeq(1)
  val inInActDataRand: List[Int] = inActSeq(2)
  val inWeightAdrRand: List[Int] = weightSeq.head
  val inWeightCountRand: List[Int] = weightSeq(1)
  val inWeightDataRand: List[Int] = weightSeq(2)
  private def genAll(): (Seq[List[Int]], Seq[List[Int]], List[Int], Seq[List[List[Int]]]) = {
    var error = true
    var inActList: List[List[Int]] = Nil
    var weightList: List[List[Int]] = Nil
    var inActSeq: Seq[List[Int]] = Nil
    var weightSeq: Seq[List[Int]] = Nil
    var outPSumRand: List[Int] = Nil
    while (error) {
      inActList = genSparse(cols = F0*N0*E, rows = R*C0, max = scsDataMax, ratio =  0.845)
      weightList = genSparse(cols = R*C0, rows = M0, max = scsDataMax, ratio =  0.6)
      inActSeq = genAdrCountData(inActList, inActOrWeight = true)
      weightSeq = genAdrCountData(weightList, inActOrWeight = false)
      outPSumRand = goldenFlatResult(weightList, inActList)
      error = checkConstrain(inActSeq, weightSeq, outPSumRand)
    }
    (inActSeq, weightSeq, outPSumRand, Seq(inActList, weightList))
  }
  private def checkConstrain(theInActSeq: Seq[List[Int]], theWeightSeq: Seq[List[Int]], thePSumList: List[Int]): Boolean = {
    var error = false
    if (
      theInActSeq.head.head == inActZeroColumnCode || // TODO: remove this requirement
      theWeightSeq.head.head == weightZeroColumnCode || // TODO: remove this requirement
      theInActSeq.head.length > inActAdrSPadSize ||
      theInActSeq.last.length > inActDataSPadSize ||
      theWeightSeq.head.length > weightAdrSPadSize ||
      theWeightSeq.last.length > weightDataSPadSize ||
      thePSumList.length > pSumDataSPadSize ||
      theInActSeq.head.max > inActAdrMax ||
      theWeightSeq.head.max > weightAdrMax ||
      thePSumList.max > pSumMax
    ) {
      error = true
    }
    if (error) {
      println("[Waring] Ops!!! those are not what you need!!!")
    } else {
      println("[Info] Congratulate!!! you've got what you want!!!")
    }
    error
  }
}

class GenOneStreamData extends GenFunc {
  require(M2 <= N2*F2, s"M2 should less than N2*F2, $M2 <= ${N2*F2} ?")
  private val oneStream = Seq.fill(inActStreamNum) {new GenOnePETestData}
  val inActStream: Seq[List[List[Int]]] = oneStream.map(x => x.inActList)
  val inActAdrStream: Seq[List[Int]] = oneStream.map(x => x.inInActAdrRand)
  val inActCountStream: Seq[List[Int]] = oneStream.map(x => x.inInActCountRand)
  val inActDataStream: Seq[List[Int]] = oneStream.map(x => x.inInActDataRand)
  val weightStream: Seq[List[List[Int]]] = oneStream.take(weightStreamNum).map(x => x.weightList)
  val weightAdrStream: Seq[List[Int]] = oneStream.take(weightStreamNum).map(x => x.inWeightAdrRand)
  val weightCountStream: Seq[List[Int]] = oneStream.take(weightStreamNum).map(x => x.inWeightCountRand)
  val weightDataStream: Seq[List[Int]] = oneStream.take(weightStreamNum).map(x => x.inWeightDataRand)
  val outPSumStream: List[List[Int]] = goldenFlatStreamResult()
  private def goldenFlatStreamResult(): List[List[Int]] = {
    var outPSumStream: List[List[Int]] = Nil
    for (g2 <- 0 until G2) {
      for (n2 <- 0 until N2) {
        for (m2 <- 0 until M2) {
          for (f2 <- 0 until F2) {
            var currentPSum: List[Int] = Nil
            var currentTempPSum: List[List[Int]] = Nil
            for (c2 <- 0 until C2) {
              for (s2 <- 0 until S2) {
                val goldFlatPSum = goldenFlatResult(weightStream(g2*M2*C2*S2 + m2*C2*S2 + c2*S2 + s2),
                  inActStream(g2*N2*F2*C2*S2 + n2*F2*C2*S2 + f2*C2*S2 + c2*S2 + s2))
                currentTempPSum = currentTempPSum:::List(goldFlatPSum)
              }
            }
            for (i <- 0 until pSumOneSPadNum) {
              var oneCurrentPSum: Int = 0
              oneCurrentPSum = currentTempPSum.map(x => x(i)).sum
              currentPSum = currentPSum:::List(oneCurrentPSum)
            }
            outPSumStream = outPSumStream:::List(currentPSum) // add this to the end of the Seq
          }
        }
      }
    }
    require(outPSumStream.length == pSumStreamNum, s"the PSum stream should have $pSumStreamNum pSum Lists, but ${outPSumStream.length}.")
    outPSumStream.foreach(x => require(x.max <= pSumMax, s"each pSum should smaller than max, but ${x.max} is greater than $pSumMax"))
    outPSumStream
  }
}

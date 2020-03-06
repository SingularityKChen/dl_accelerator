package dla.tests

import dla.pe.{MCRENFConfig, PESizeConfig, SPadSizeConfig}
import org.scalatest._

import scala.math.pow
import scala.util.Random
class GenOnePETestDataTest extends FlatSpec {
  val genHp = new GenOnePETestData
  val theData: List[List[Int]] = genHp.genSparse(3,5,20,0)
  val anotherData: List[List[Int]] = genHp.genSparse(5, 3, 15, 0)
  val resultData: List[List[Int]] = genHp.goldenResult(theData, anotherData)
  println("ListA = " + theData)
  println("ListB = " + anotherData)
  println("GoRe  = " + resultData)
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
}

class GenOnePETestData extends PESizeConfig with SPadSizeConfig with MCRENFConfig {
  private val pSumMax = pow(2, psDataWidth).toInt
  private val inActAdrMax = pow(2, inActAdrWidth).toInt
  private val weightAdrMax = pow(2, weightAdrWidth).toInt
  private val scsDataMax = pow(2, cscDataWidth).toInt
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
 def genSparse(rows: Int, cols: Int, max: Int, ratio: Double): List[List[Int]] = {
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
  def goldenCovResult(kernel: List[List[Int]], inAct: List[List[Int]]): List[List[Int]] = {
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
  def goldenResult(listA: List[List[Int]], listB: List[List[Int]]): List[List[Int]] = {
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
  def goldenFlatResult(listA: List[List[Int]], listB: List[List[Int]]): List[Int] = {
    // translate from every row to every column read
    val tempResult: List[List[Int]] = goldenResult(listA, listB)
    var resultList: List[Int] = Nil
    for (j <- tempResult.head.indices) {
      val currentCol = tempResult.map(x => x(j))
      resultList = resultList:::currentCol
    }
    resultList
  }
  def genAdrCountData(listA: List[List[Int]], inActOrWeight: Boolean): Seq[List[Int]] = {
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

class GenOneStreamData(val stream: Int) {

}

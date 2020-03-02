package dla.tests

import dla.pe.{PESizeConfig, SPadSizeConfig}
import org.scalatest._

import scala.math.pow
import scala.util.Random
class GenOnePETestDataTest extends FlatSpec {
  val oneTest = new GenOnePETestData
  val theData: List[List[Int]] = oneTest.genSparse(3,5,20,0)
  val anotherData: List[List[Int]] = oneTest.genSparse(5, 3, 15, 0)
  val resultData: List[List[Int]] = oneTest.goldenResult(theData, anotherData)
  println("ListA = " + theData)
  println("ListB = " + anotherData)
  println("GoRe  = " + resultData)
  val (adr, count, data): (List[Int], List[Int], List[Int]) = oneTest.genAdrCountData(theData, inActOrWeight = false)
  println("adr   = " + adr)
  println("count = " + count)
  println("data  = " + data)
  println(oneTest.goldenFlatResult(theData, anotherData))
}

class GenOnePETestData extends PESizeConfig with SPadSizeConfig {
  private val pSumMax = pow(2, psDataWidth).toInt
  private val inActAdrMax = pow(2, inActAdrWidth).toInt
  private val weightAdrMax = pow(2, weightAdrWidth).toInt
  private val scsDataMax = pow(2, cscDataWidth).toInt
  var inActList: List[List[Int]] = genSparse(8, 6, max = scsDataMax, 0.845)
  var weightList: List[List[Int]] = genSparse(4, 8, max = scsDataMax, 0.6)
  var (inInActAdrRand, inInActCountRand, inInActDataRand) = genAdrCountData(inActList, inActOrWeight = true)
  var (inWeightAdrRand, inWeightCountRand, inWeightDataRand) = genAdrCountData(weightList, inActOrWeight = false)
  var outPSumRand: List[Int] = goldenFlatResult(weightList, inActList)
  /*private def checkConstrain(): Unit = {
    var error = false
    if (
      inInActAdrRand.head == inActZeroColumnCode || // TODO: remove this requirement
      inWeightAdrRand.head == weightZeroColumnCode || // TODO: remove this requirement
      inInActAdrRand.length > inActAdrSPadSize ||
      inInActCountRand.length > inActDataSPadSize ||
      inWeightAdrRand.length > weightAdrSPadSize ||
      inWeightCountRand.length > weightDataSPadSize ||
      outPSumRand.length > pSumDataSPadSize ||
      inInActAdrRand.max > inActAdrMax ||
      inWeightAdrRand.max > weightAdrMax ||
      outPSumRand.max > pSumMax
    ) {
      error = true
    }
    if (error) {
      inActList = genSparse(8, 6, max = scsDataMax, 0.845)
      weightList = genSparse(4, 8, max = scsDataMax, 0.6)
      (inInActAdrRand, inInActCountRand, inInActDataRand) = genAdrCountData(inActList, inActOrWeight = true)
      (inWeightAdrRand, inWeightCountRand, inWeightDataRand) = genAdrCountData(weightList, inActOrWeight = false)
      outPSumRand = goldenFlatResult(weightList, inActList)
      println("meet one error")
      checkConstrain()
    }
  }
  */
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
  def genAdrCountData(listA: List[List[Int]], inActOrWeight: Boolean): (List[Int], List[Int], List[Int]) = {
    val zeroCode: Int = if (inActOrWeight) inActZeroColumnCode else weightZeroColumnCode
    var adrList: List[Int] = Nil
    var countList: List[Int] = Nil
    var dataList: List[Int] = Nil
    for (j <- listA.head.indices) { // for column
      val currentCol = listA.map(x => x(j))
      println(currentCol)
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
    (adrList, countList, dataList)
  }
}

class GenOneStreamData(val stream: Int) {

}

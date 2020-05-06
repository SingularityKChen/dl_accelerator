package dla.tests

import dla.cluster.{ClusterSRAMConfig, GNMFCS1Config, GNMFCS2Config}
import dla.eyerissWrapper.EyerissTopConfig
import dla.pe.{MCRENFConfig, PESizeConfig, SPadSizeConfig}
import org.scalatest._

import scala.math.{max, pow}
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
  println("inActAdrStream  = " + oneStreamTest.inActAdrStream.flatten)
  println("weightAdrStream = " + oneStreamTest.weightAdrStream.flatten)
  println("pSumStream      = " + oneStreamTest.outPSumStream)
}

class GenFunc(inActSparseRatio: Double = 0.845, weightSparseRatio: Double = 0.6, p: EyerissModelParam = new EyerissModelParam)
  extends PESizeConfig with SPadSizeConfig with MCRENFConfig with GNMFCS1Config with GNMFCS2Config
  with ClusterSRAMConfig with EyerissTopConfig {
  protected val pSumMax: Int = pow(2, psDataWidth).toInt
  protected val inActAdrMax: Int = pow(2, inActAdrWidth).toInt - 1 // zeroCode
  protected val weightAdrMax: Int = pow(2, weightAdrWidth).toInt - 1 // zeroCode
  protected val cscDataMax: Int = pow(2, cscDataWidth).toInt
  private val needRequirement: Boolean = false
  object dataSequencer {
    /** original data, complete matrix */
    object dram {
      val inAct: Seq[Seq[List[List[Int]]]] = Seq.fill(p.nnShape.inAct.number, p.nnShape.inAct.channel) {
        genSparse(rows = p.nnShape.inAct.height, cols = p.nnShape.inAct.width, max = cscDataMax, ratio = inActSparseRatio)
      }
      val weight: Seq[Seq[List[List[Int]]]] = Seq.fill(p.nnShape.weight.number, p.nnShape.weight.channel) {
        genSparse(rows = p.nnShape.weight.height, cols = p.nnShape.weight.width, max = cscDataMax, ratio = weightSparseRatio)
      }
      //val pSum
    }
    /** the first dimension is the index of NoC level's parameters,
      * the second dimension is the index of GLB level's parameters,
      * the third dimension is the index of SPad level's matrix width,
      * the fourth dimension is the index of SPad level's matrix height */
    object glb {
      val (inAct, weight) = dramToRSDataFlow(dram.inAct, dram.weight)
      object separatedSPadCSCData {
        val inActSeq: Seq[Seq[Seq[List[Int]]]] = inAct.map(x => x.map(y => genAdrCountData(y, inActOrWeight = true)))
        val weightSeq: Seq[Seq[Seq[List[Int]]]] = weight.map(x => x.map(y => genAdrCountData(y, inActOrWeight = false)))
        /** the first dimension is the index of NoC level's parameters,
          * the second dimension is the index of GLB level's parameters */
        val inActAdr: Seq[Seq[List[Int]]] = inActSeq.map(x => x.map({ y =>
          y.head
        }))
        val inActData: Seq[Seq[Seq[Int]]] = inActSeq.map(x => x.map(y => y.last)).zip(inActSeq.map(x => x.map(y => y(1))))
          .map({ case (seq, seq1) =>
            seq.zip(seq1).map { case (data, count) =>
              combineDataAndCount(data, count)
            }})
        val weightAdr: Seq[Seq[List[Int]]] = weightSeq.map(x => x.map({ y =>
          y.head
        }))
        val weightData: Seq[Seq[Seq[Int]]] = weightSeq.map(x => x.map(y => y.last)).zip(weightSeq.map(x => x.map(y => y(1))))
          .map({ case (seq, seq1) =>
            seq.zip(seq1).map { case (data, count) =>
              combineDataAndCount(data, count)
            }})
        /*println(s"one stream of inActAdr = ${inActAdr.head}")
        println(s"the length of inActAdr = ${inActAdr.head.head.length}")
        println(s"one stream of inActData = ${inActData.head}")
        println(s"the length of one inActData = ${inActData.head.head.length}")
        println(s"one stream of weightAdr = ${weightAdr.head}")
        println(s"the length of weightAdr = ${weightAdr.head.head.length}")
        println(s"one stream of weightData = ${weightData.head}")
        println(s"the length of weightData = ${weightData.head.head.length}")*/
        private val inActAdrMaxLength = inActAdr.map(x => x.map(y => y.length).max).max
        private val inActDataMaxLength = inActData.map(x => x.map(y => y.length).max).max
        private val weightAdrMaxLength = weightAdr.map(x => x.map(y => y.length).max).max
        private val weightDataMaxLength = weightData.map(x => x.map(y => y.length).max).max
        if (needRequirement) {
          require(inActAdrMaxLength <= inActAdrSPadSize, s"inActAdrSPadSize needs at least $inActAdrMaxLength")
          require(inActDataMaxLength <= inActDataSPadSize, s"inActDataSPadSize needs at least $inActDataMaxLength")
          require(weightAdrMaxLength <= weightAdrSPadSize, s"weightAdrSPadSize needs at least $weightAdrMaxLength")
          require(weightDataMaxLength <= weightDataSPadSize, s"weightDataSPadSize needs at least $weightDataMaxLength")
        }
      }
      object cscData {
        /** the first dimension is the index of NoC level's parameters,
          * the second dimension is one stream of inAct address vector with 0 as an end. */
        val inActAdr: Seq[List[Int]] = separatedSPadCSCData.inActAdr.map(x => x.flatten.toList ::: List(0))
        val inActData: Seq[List[Int]] = separatedSPadCSCData.inActData.map(x => x.flatten.toList ::: List(0))
        val weightAdr: Seq[List[Int]] = separatedSPadCSCData.weightAdr.map(x => x.flatten.toList ::: List(0))
        val weightData: Seq[List[Int]] = separatedSPadCSCData.weightData.map(x => x.flatten.toList ::: List(0))
        if (needRequirement) {
          require(inActAdr.flatten.max <= inActAdrMax, s"${inActAdr.flatten.max} < $inActAdrMax?\n$inActAdr\n\n\n$inActData")
          require(weightAdr.flatten.max <= weightAdrMax, s"${weightAdr.flatten.max} < $weightAdrMax?")
          require(inActAdr.map(x => x.length).max < inActAdrSRAMSize,
            s"inActAdrSRAMSize needs at least ${inActAdr.map(x => x.length).max}")
          require(inActData.map(x => x.length).max < inActDataSRAMSize,
            s"inActDataSRAMSize needs at least ${inActData.map(x => x.length).max}")
        }
        private def checkConstrain(theInActSeq: Seq[List[Int]], theWeightSeq: Seq[List[Int]]): Boolean = {
          var error = false
          if (
            theInActSeq.head.head == inActZeroColumnCode || // TODO: remove this requirement
              theWeightSeq.head.head == weightZeroColumnCode //|| // TODO: remove this requirement
              /* can meet the following constrain now
              theInActSeq.head.length > inActAdrSPadSize ||
              theInActSeq.last.length > inActDataSPadSize ||
              theWeightSeq.head.length > weightAdrSPadSize ||
              theWeightSeq.last.length > weightDataSPadSize*/
              //|| thePSumList.max > pSumMax
          ) {
            error = true
          }
          if (error) {
            print("x")
            //println("[Waring] Ops!!! those are not what you need!!!")
          } else {
            print(".")
            //println("[Info] Congratulate!!! you've got what you want!!!")
          }
          error
        }
      }
    }
    /** GLB level, for one Group Cluster */
    object oneStream {
      /** the first dimension is the index of GLB levels' parameters,
        * the second dimension is the index of SPad level's matrix width,
        * the third dimension is the index of SPad level's matrix height */
      val inAct: Seq[List[List[Int]]] = glb.inAct.head
      val weight: Seq[List[List[Int]]] = glb.weight.head
      object cscData {
        /** the combination of address, count and data */
        val inActAdr: List[Int] = glb.cscData.inActAdr.head
        val inActData: List[Int] = glb.cscData.inActData.head
        val weightAdr: List[Int] = glb.cscData.weightAdr.head
        val weightData: List[Int] = glb.cscData.weightData.head
      }
    }
    /** SPad level, for one PE */
    object oneSPad {
      val inAct: List[List[Int]] = oneStream.inAct.head
      val weight: List[List[Int]] = oneStream.weight.head
      object cscData {
        /** .head: inActAdr; .head: first NoC; .head: first SPad*/
        val inActAdr: List[Int] = glb.separatedSPadCSCData.inActAdr.head.head
        val inActData: Seq[Int] = glb.separatedSPadCSCData.inActData.head.head
        val weightAdr: List[Int] = glb.separatedSPadCSCData.weightAdr.head.head
        val weightData: Seq[Int] = glb.separatedSPadCSCData.weightData.head.head
      }
    }
    def dramToRSDataFlow(inActMem: Seq[Seq[List[List[Int]]]], weightMem: Seq[Seq[List[List[Int]]]]):
    (Seq[Seq[List[List[Int]]]], Seq[Seq[List[List[Int]]]]) = {
      val inActArray: Array[Array[Array[Array[Int]]]] =
        Array.fill(inActParNum, inActStreamNum, inActMatrixHeight, inActMatrixWidth) {0}
      val weightArray: Array[Array[Array[Array[Int]]]] =
        Array.fill(weightParNum, weightStreamNum, weightMatrixHeight, weightMatrixWidth) {0}
      for (g1 <- 0 until p.G1) {
        for (n1 <- 0 until p.N1) {
          for (m1 <- 0 until p.M1) {
            for (f1 <- 0 until p.F1) {
              for (c1 <- 0 until p.C1) {
                for (s1 <- 0 until p.S1) {
                  val inActNoCIdx = g1*p.N1*p.C1*(p.F1+p.S1) + n1*p.C1*(p.F1+p.S1) + c1*(p.F1+p.S1) + (f1+s1)
                  val weightNoCIdx = g1*p.M1*p.C1*p.S1 + m1*p.C1*p.S1 + c1*p.S1 + s1
                  for (g2 <- 0 until p.G2) {
                    for (n2 <- 0 until p.N2) {
                      for (m2 <- 0 until p.M2) {
                        for (f2 <- 0 until p.F2) {
                          for (c2 <- 0 until p.C2) {
                            for (s2 <- 0 until p.S2) {
                              val inActGLBIdx = g2*p.N2*p.C2*(p.F2 + p.S2) + n2*p.C2*(p.F2+p.S2) + c2*(p.F2+p.S2) + f2+s2
                              val weightGLBIdx = g2*p.M2*p.C2*p.S2 + m2*p.C2*p.S2 + c2*p.S2 + s2
                              val weightWidth = s2*p.S1 + s1
                              for (f0 <- 0 until p.F0) {
                                val inActWidth = s2*p.S1 + f2*p.F1*p.F0 + f1*p.F0 + f0
                                for (n0 <- 0 until p.N0) {
                                  val inActNumber = n2*p.N1*p.N0 +n1*p.N0 + n0
                                  for (e <- 0 until p.E) {
                                    val inActWidthIdx = f0*p.N0*p.E + n0*p.E + e
                                    for (r <- 0 until p.R) {
                                      val inActHeight = e + r
                                      val weightHeight = r
                                      for (c0 <- 0 until p.C0) {
                                        val channel = g2*p.G1*p.C2*p.C1*p.C0 + g1*p.C2*p.C1*p.C0 + c2*p.C1*p.C0 + c1*p.C0 + c0
                                        val inActHeightIdx = r*p.C0 + c0
                                        val weightWidthIdx = r*p.C0 + c0
                                        inActArray(inActNoCIdx)(inActGLBIdx)(inActHeightIdx)(inActWidthIdx) =
                                          inActMem(inActNumber)(channel)(inActHeight)(inActWidth)
                                        for (m0 <- 0 until p.M0) {
                                          val weightNumber = m2*p.M1*p.M0 + m1*p.M0 + m0
                                          val weightHeightIdx = m0
                                          weightArray(weightNoCIdx)(weightGLBIdx)(weightHeightIdx)(weightWidthIdx) =
                                            weightMem(weightNumber)(channel)(weightHeight)(weightWidth)
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      (inActArray.map(x => x.map(y => y.map(z => z.toList).toList).toSeq).toSeq,
        weightArray.map(x => x.map(y => y.map(z => z.toList).toList).toSeq).toSeq)
    }
  }
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
  def genAdrCountData(listA: List[List[Int]], inActOrWeight: Boolean): Seq[List[Int]] = {
    val zeroCode: Int = if (inActOrWeight) inActZeroColumnCode else weightZeroColumnCode
    val max: Int = if (inActOrWeight) inActAdrMax else weightAdrMax
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
    if (adrList.max > max) {
      println(s"${adrList.max} <= $max?\n$adrList\n\n$dataList\n")
    }
    Seq(adrList, countList, dataList)
  }
  protected def toBinary(i: Int, digits: Int = 8): String =
    s"%${digits}s".format(i.toBinaryString).replaceAllLiterally(" ", "0")
  def combineDataAndCount(theData: Seq[Int], theCount: Seq[Int]): Seq[Int] = {
    // input data and count, and combine them together
    val theDataWithCount: Seq[(Int, Int)] = theData zip theCount
    val theDataCountBinary: Seq[String] = theDataWithCount.map{case (x: Int, y: Int) =>
      toBinary(x, cscDataWidth) + toBinary(y, cscCountWidth)}
    val theDataCountDec: Seq[Int] = theDataCountBinary.map(x => BigInt(x, 2).toInt)
    theDataCountDec
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
      inActList = genSparse(rows = inActMatrixHeight, cols = inActMatrixWidth, max = cscDataMax, ratio =  0.845)
      weightList = genSparse(rows = weightMatrixHeight, cols = weightMatrixWidth, max = cscDataMax, ratio =  0.6)
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
      print("x")
      //println("[Waring] Ops!!! those are not what you need!!!")
    } else {
      print(".")
      //println("[Info] Congratulate!!! you've got what you want!!!")
    }
    error
  }
}

class GenOneStreamData extends GenFunc {
  require(M2 <= N2*F2, s"M2 should less than N2*F2, $M2 <= ${N2*F2} ?")
  private val inActGLBNum = inActStreamNum * inActParNum
  private val weightGLBNum = weightStreamNum * weightParNum
  private val pSumGLBNum = pSumStreamNum * pSumParNum
  private val peNum = peRowNum * peColNum * cgRowNum * cgColNum
  private val oneStream = Seq.fill(max(inActGLBNum, weightGLBNum)){new GenOnePETestData}
  print("\n")
  val inActStreamMemOrder: Seq[List[List[Int]]] = oneStream.take(inActGLBNum).map(x => x.inActList)
  println(s"the length of inActStream = ${inActStreamMemOrder.flatten.flatten.length}")
  val inActAdrStreamMemOrder: Seq[List[Int]] = oneStream.take(inActGLBNum).map(x => x.inInActAdrRand)
  println(s"the length of inActAdrStreamTmp = ${inActAdrStreamMemOrder.flatten.length}")
  val inActCountStreamMemOrder: Seq[List[Int]] = oneStream.take(inActGLBNum).map(x => x.inInActCountRand)
  val inActDataStreamMemOrder: Seq[List[Int]] = oneStream.take(inActGLBNum).map(x => x.inInActDataRand)
  println(s"the length of inActDataStreamTmp = ${inActDataStreamMemOrder.flatten.length}")
  val weightStreamMemOrder: Seq[List[List[Int]]] = oneStream.take(weightGLBNum).map(x => x.weightList)
  println(s"the length of weightStream = ${weightStreamMemOrder.flatten.flatten.length}")
  val weightAdrStreamMemOrder: Seq[List[Int]] = oneStream.take(weightGLBNum).map(x => x.inWeightAdrRand)
  println(s"the length of weightAdrStreamTmp = ${weightAdrStreamMemOrder.flatten.length}")
  val weightCountStreamMemOrder: Seq[List[Int]] = oneStream.take(weightGLBNum).map(x => x.inWeightCountRand)
  val weightDataStreamMemOrder: Seq[List[Int]] = oneStream.take(weightGLBNum).map(x => x.inWeightDataRand)
  println(s"the length of weightDataStreamTmp = ${weightDataStreamMemOrder.flatten.length}")
  val outPSumStreamMemOrder: List[List[Int]] = goldenFlatStreamResult()
  println(s"the length of outPSumStreamTmp = ${outPSumStreamMemOrder.flatten.length}")
  private val (rightOrInAct, rightInActAdr, rightInActData,
  rightOrWeight, rightWeightAdr, rightWeightData, rightPSumData) = getThingsReady
  /** [[inActStreamGLBOrder]] has four dimensions,
    * the first dimension is the NoC level index value,
    * the second one is the GLB level index value,
    * the third and four-th are height and width*/
  val inActStreamGLBOrder: Seq[Seq[List[List[Int]]]] = rightOrInAct
  val inActAdrStream: Seq[List[Int]] = rightInActAdr
  println(s"the length of inActAdrStream = ${inActAdrStream.flatten.length}")
  val inActDataStream: Seq[List[Int]] = rightInActData // has combined data and count
  /** [[weightStreamGLBOrder]] has four dimensions,
    * the first dimension is the NoC level index value,
    * the second one is the GLB level index value,
    * the third and four-th are height and width*/
  val weightStreamGLBOrder: Seq[Seq[List[List[Int]]]] = rightOrWeight
  val weightAdrStream: Seq[List[Int]] = rightWeightAdr
  val weightDataStream: Seq[List[Int]] = rightWeightData // has combined data and count
  val outPSumStream: List[List[Int]] = rightPSumData.toList
  private def getThingsReady: (Seq[Seq[List[List[Int]]]], Seq[List[Int]], Seq[List[Int]], Seq[Seq[List[List[Int]]]],
    Seq[List[Int]], Seq[List[Int]], Seq[List[Int]]) = {
    require(S1 == 3 && F1 == 4, "you need to correct this data generation function to fit more situations") // TODO: remove
    var inActOrStream: List[List[List[List[Int]]]] = Nil
    var inActAdr: List[List[Int]] = Nil
    var inActData: List[List[Int]] = Nil // include count
    var weightOrStream: List[List[List[List[Int]]]] = Nil
    var weightAdr: List[List[Int]] = Nil
    var weightData: List[List[Int]] = Nil // include count
    var pSumData: List[List[Int]] = Nil
    for (g1 <- 0 until G1) {
      for (n1 <- 0 until N1) {
        for (m1 <- 0 until M1) {
          for (f1 <- 0 until F1) {
            for (c1 <- 0 until C1) {
              for (s1 <- 0 until S1) {
                var inActOrTmp: List[List[List[Int]]] = Nil
                var inActAdrTmp: List[List[Int]] = Nil
                var inActDataTmp: List[List[Int]] = Nil // include count
                var weightOrTmp: List[List[List[Int]]] = Nil
                var weightAdrTmp: List[List[Int]] = Nil
                var weightDataTmp: List[List[Int]] = Nil // include count
                var pSumDataTmp: List[List[Int]] = Nil
                var inActIdxTest: List[Int] = Nil
                for (i <- 0 until inActStreamNum) {
                  val inActIdx = i*inActParNum + g1*N1*C1*(F1 + S1) + n1*C1*(F1 + S1) + c1*(F1 + S1) + (f1 + s1)
                  inActIdxTest = inActIdxTest:::List(inActIdx)
                  inActOrTmp = inActOrTmp:::List(inActStreamMemOrder(inActIdx))
                  inActAdrTmp = inActAdrTmp:::List(inActAdrStreamMemOrder(inActIdx))
                  inActDataTmp = inActDataTmp:::List(combineDataAndCount(inActDataStreamMemOrder(inActIdx),
                    inActCountStreamMemOrder(inActIdx)).toList)
                }
                for (i <- 0 until weightStreamNum) {
                  val weightIdx = i*weightParNum + g1*M1*C1*S1 + m1*C1*S1 + c1*S1 + s1
                  weightOrTmp = weightOrTmp:::List(weightStreamMemOrder(weightIdx))
                  weightAdrTmp = weightAdrTmp:::List(weightAdrStreamMemOrder(weightIdx))
                  weightDataTmp = weightDataTmp:::List(combineDataAndCount(weightDataStreamMemOrder(weightIdx),
                    weightCountStreamMemOrder(weightIdx)).toList)
                }
                for (i <- 0 until pSumStreamNum) {
                  val pSumIdx = i*pSumParNum + g1*N1*M1*F1 + n1*M1*F1 + m1*F1 + f1
                  pSumDataTmp = pSumDataTmp:::List(outPSumStreamMemOrder(pSumIdx))
                }
                require(inActAdrTmp.flatten.length <= inActAdrSRAMSize, s"current inActAdr should fit in one inActSRAM, " +
                  s"but ${inActAdrTmp.flatten.length} > $inActAdrSRAMSize")
                require(inActDataTmp.flatten.length <= inActDataSRAMSize, s"current inActData should fit in one inActDataSRAM, " +
                  s"but ${inActDataTmp.flatten.length} > $inActDataSRAMSize")
                require(pSumDataTmp.flatten.length <= pSumSRAMSize, s"current pSumData should fit in one pSumSRAM, " +
                  s"but ${pSumDataTmp.flatten.length} > $pSumSRAMSize")
                inActOrStream = inActOrStream:::List(inActOrTmp)
                inActAdr = inActAdr:::List(inActAdrTmp.flatten:::List(0))
                inActData = inActData:::List(inActDataTmp.flatten:::List(0))
                weightOrStream = weightOrStream:::List(weightOrTmp)
                weightAdr = weightAdr:::List(weightAdrTmp.flatten:::List(0))
                weightData = weightData:::List(weightDataTmp.flatten:::List(0))
                pSumData = pSumData:::List(pSumDataTmp.flatten)
              }
            }
          }
        }
      }
    }
    (inActOrStream, inActAdr, inActData, weightOrStream, weightAdr, weightData, pSumData)
  }
  private def goldenFlatStreamResult(): List[List[Int]] = {
    var outPSumStream: List[List[Int]] = Nil
    for (g2 <- 0 until G2) {
      for (n2 <- 0 until N2) {
        for (m2 <- 0 until M2) {
          for (f2 <- 0 until F2) {
            var currentStreamTempPSum: List[List[List[Int]]] = Nil
            for (c2 <- 0 until C2) {
              for (s2 <- 0 until S2) {
                var oneStreamPSum: List[List[Int]] = Nil
                for (g1 <- 0 until G1) {
                  for (n1 <- 0 until N1) {
                    for (m1 <- 0 until M1) {
                      for (f1 <- 0 until F1) {
                        var currentParPSum: List[Int] = Nil
                        var currentParTempPSum: List[List[Int]] = Nil
                        for (c1 <- 0 until C1) {
                          for (s1 <- 0 until S1) {
                            val weightIdx = (g2*M2*C2*S2 + m2*C2*S2 + c2*S2 + s2)*weightParNum +
                              g1*M1*C1*S1 + m1*C1*S1 + c1*S1 + s1
                            val inActIdx = (g2*N2*C2*(F2 + S2) + n2*C2*(F2 + S2) + c2*(F2 + S2) + (f2 + s2))*inActParNum +
                              g1*N1*C1*(F1 + S1) + n1*C1*(F1 + S1) + c1*(F1 + S1) + (f1 + s1)
                            val goldFlatPSum = goldenFlatResult(weightStreamMemOrder(weightIdx),
                              inActStreamMemOrder(inActIdx))
                            currentParTempPSum = currentParTempPSum:::List(goldFlatPSum)
                          }
                        }
                        for (i <- 0 until pSumOneSPadNum) {
                          var oneCurrentPSum: Int = 0
                          oneCurrentPSum = currentParTempPSum.map(x => x(i)).sum
                          currentParPSum = currentParPSum:::List(oneCurrentPSum)
                        }
                        oneStreamPSum = oneStreamPSum:::List(currentParPSum) // add this to the end of the Seq
                      }
                    }
                  }
                }
                currentStreamTempPSum = currentStreamTempPSum:::List(oneStreamPSum)
              }
            }
            for (par <- 0 until pSumParNum) {
              var oneParSum: List[Int] = Nil
              for (i <- 0 until pSumOneSPadNum) {
                var oneCurrentPSum: Int = 0
                oneCurrentPSum = currentStreamTempPSum.map(x => x(par)).map(y => y(i)).sum
                oneParSum = oneParSum:::List(oneCurrentPSum)
              }
              outPSumStream = outPSumStream:::List(oneParSum)
            }
          }
        }
      }
    }
    require(outPSumStream.length == pSumGLBNum, s"the PSum stream should have $pSumGLBNum pSum Lists, " +
      s"but ${outPSumStream.length}.")
    outPSumStream.foreach(x => require(x.max <= pSumMax, s"each pSum should smaller than max, " +
      s"but ${x.max} is greater than $pSumMax"))
    outPSumStream
  }
}

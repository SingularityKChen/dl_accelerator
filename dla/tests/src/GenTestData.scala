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

class GenFunc(inActSparseRatio: Double = 0.845, weightSparseRatio: Double = 0.6)
  extends PESizeConfig with SPadSizeConfig with MCRENFConfig with GNMFCS1Config with GNMFCS2Config
  with ClusterSRAMConfig with EyerissTopConfig {
  object nnShape {
    object inAct {
      val number: Int = N2*N1*N0
      val channel: Int = G2*G1*C2*C1*C0 //TODO: check G
      /** although the width of inAct in RS+ data flow is (S2 + F2)*(S1 + F1)*F0,
        * which is much greater than this width. It's caused by the overlap.*/
      val width: Int = S2*S1 + F2*F1*F0
      val height: Int = R + E
      //require(height == width, s"inAct's height doesn't equal to width, $height == $width ?")
    }
    object weight {
      val number: Int = M2*M1*M0
      val channel: Int = G2*G1*C2*C1*C0
      val width: Int = S2*S1
      val height: Int = R
      //require(height == width, s"weight's height doesn't equal to width, $height == $width ?")
    }
    object pSum {
      require(pSumDataSPadSize > pSumOneSPadNum,
        s"pSumSPad can not contains all the pSum, $pSumDataSPadSize > $pSumOneSPadNum?")
      val number: Int = N2*N1*N0
      val channel: Int = G2*G1*M2*M1*M0
      val width: Int = F2*F1*F0
      val height: Int = E
      //require(height == width, s"pSum's height doesn't equal to width, $height == $width ?")
    }
    require(inAct.number == pSum.number)
    require(inAct.channel == weight.channel)
    require(weight.number == pSum.channel)
  }
  protected val pSumMax: Int = pow(2, psDataWidth).toInt
  protected val inActAdrMax: Int = pow(2, inActAdrWidth).toInt - 1 // zeroCode
  protected val weightAdrMax: Int = pow(2, weightAdrWidth).toInt - 1 // zeroCode
  protected val cscDataMax: Int = pow(2, cscDataWidth).toInt
  object dataSequencer {
    /** original data, complete matrix */
    object dram {
      val inAct: Seq[Seq[List[List[Int]]]] = Seq.fill(nnShape.inAct.number, nnShape.inAct.channel) {
        genSparse(nnShape.inAct.width, nnShape.inAct.height, max = cscDataMax, ratio = inActSparseRatio)
      }
      val weight: Seq[Seq[List[List[Int]]]] = Seq.fill(nnShape.weight.number, nnShape.weight.channel) {
        genSparse(nnShape.weight.width, nnShape.weight.height, max = cscDataMax, ratio = weightSparseRatio)
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
          require(y.head.length <= weightAdrSPadSize, s"weightAdrSPadSize needs at least ${y.head.length}")
          y.head
        }))
        val weightData: Seq[Seq[Seq[Int]]] = weightSeq.map(x => x.map(y => y.last)).zip(weightSeq.map(x => x.map(y => y(1))))
          .map({ case (seq, seq1) =>
            seq.zip(seq1).map { case (data, count) =>
              combineDataAndCount(data, count)
            }})
        private val inActAdrMaxLength = inActAdr.map(x => x.map(y => y.length).max).max
        private val inActDataMaxLength = inActData.map(x => x.map(y => y.length).max).max
        private val weightAdrMaxLength = weightAdr.map(x => x.map(y => y.length).max).max
        private val weightDataMaxLength = weightData.map(x => x.map(y => y.length).max).max
        require(inActAdrMaxLength <= inActAdrSPadSize, s"inActAdrSPadSize needs at least $inActAdrMaxLength")
        require(inActDataMaxLength <= inActDataSPadSize, s"inActDataSPadSize needs at least $inActDataMaxLength")
        require(weightAdrMaxLength <= weightAdrSPadSize, s"weightAdrSPadSize needs at least $weightAdrMaxLength")
        require(weightDataMaxLength <= weightDataSPadSize, s"weightDataSPadSize needs at least $weightDataMaxLength")
      }
      object cscData {
        /** the first dimension is the index of NoC level's parameters,
          * the second dimension is one stream of inAct address vector with 0 as an end. */
        val inActAdr: Seq[List[Int]] = separatedSPadCSCData.inActAdr.map(x => x.flatten.toList ::: List(0))
        val inActData: Seq[List[Int]] = separatedSPadCSCData.inActData.map(x => x.flatten.toList ::: List(0))
        val weightAdr: Seq[List[Int]] = separatedSPadCSCData.weightAdr.map(x => x.flatten.toList ::: List(0))
        val weightData: Seq[List[Int]] = separatedSPadCSCData.weightData.map(x => x.flatten.toList ::: List(0))
        require(inActAdr.flatten.max <= inActAdrMax, s"${inActAdr.flatten.max} < $inActAdrMax?\n$inActAdr\n\n\n$inActData")
        require(weightAdr.flatten.max <= weightAdrMax, s"${weightAdr.flatten.max} < $weightAdrMax?")
        require(inActAdr.map(x => x.length).max < inActAdrSRAMSize,
          s"inActAdrSRAMSize needs at least ${inActAdr.map(x => x.length).max}")
        require(inActData.map(x => x.length).max < inActDataSRAMSize,
          s"inActDataSRAMSize needs at least ${inActData.map(x => x.length).max}")
        private def checkConstrain(theInActSeq: Seq[List[Int]], theWeightSeq: Seq[List[Int]]): Boolean = {
          var error = false
          if (
            theInActSeq.head.head == inActZeroColumnCode || // TODO: remove this requirement
              theWeightSeq.head.head == weightZeroColumnCode || // TODO: remove this requirement
              theInActSeq.head.length > inActAdrSPadSize ||
              theInActSeq.last.length > inActDataSPadSize ||
              theWeightSeq.head.length > weightAdrSPadSize ||
              theWeightSeq.last.length > weightDataSPadSize
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
        Array.fill(inActParNum, inActStreamNum, inActMatrixWidth, inActMatrixHeight) {0}
      val weightArray: Array[Array[Array[Array[Int]]]] =
        Array.fill(weightParNum, weightStreamNum, weightMatrixWidth, weightMatrixHeight) {0}
      for (g1 <- 0 until G1) {
        for (n1 <- 0 until N1) {
          for (m1 <- 0 until M1) {
            for (f1 <- 0 until F1) {
              for (c1 <- 0 until C1) {
                for (s1 <- 0 until S1) {
                  val inActNoCIdx = g1*N1*C1*(F1+S1) + n1*C1*(F1+S1) + c1*(F1+S1) + (f1+s1)
                  val weightNoCIdx = g1*M1*C1*S1 + m1*C1*S1 + c1*S1 + s1
                  for (g2 <- 0 until G2) {
                    for (n2 <- 0 until N2) {
                      for (m2 <- 0 until M2) {
                        for (f2 <- 0 until F2) {
                          for (c2 <- 0 until C2) {
                            for (s2 <- 0 until S2) {
                              val inActGLBIdx = g2*N2*C2*(F2 + S2) + n2*C2*(F2+S2) + c2*(F2+S2) + f2+s2
                              val weightGLBIdx = g2*M2*C2*S2 + m2*C2*S2 + c2*S2 + s2
                              val weightWidth = s2*S1 + s1
                              for (f0 <- 0 until F0) {
                                val inActWidth = s2*S1 + f2*F1*F0 + f1*F0 + f0
                                for (n0 <- 0 until N0) {
                                  val inActNumber = n2*N1*N0 +n1*N0 + n0
                                  for (e <- 0 until E) {
                                    val inActWidthIdx = f0*N0*E + n0*E + e
                                    for (r <- 0 until R) {
                                      val inActHeight = e + r
                                      val weightHeight = r
                                      for (c0 <- 0 until C0) {
                                        val channel = g2*G1*C2*C1*C0 + g1*C2*C1*C0 + c2*C1*C0 + c1*C0 + c0
                                        val inActHeightIdx = r*C0 + c0
                                        val weightWidthIdx = r*C0 + c0
                                        inActArray(inActNoCIdx)(inActGLBIdx)(inActWidthIdx)(inActHeightIdx) =
                                          inActMem(inActNumber)(channel)(inActWidth)(inActHeight)
                                        for (m0 <- 0 until M0) {
                                          val weightNumber = m2*M1*M0 + m1*M0 + m0
                                          val weightHeightIdx = m0
                                          weightArray(weightNoCIdx)(weightGLBIdx)(weightWidthIdx)(weightHeightIdx) =
                                            weightMem(weightNumber)(channel)(weightWidth)(weightHeight)
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
      inActList = genSparse(cols = inActMatrixWidth, rows = inActMatrixHeight, max = cscDataMax, ratio =  0.845)
      weightList = genSparse(cols = weightMatrixWidth, rows = weightMatrixHeight, max = cscDataMax, ratio =  0.6)
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
  val inActStreamTmp: Seq[List[List[Int]]] = oneStream.take(inActGLBNum).map(x => x.inActList)
  println(s"the length of inActStream = ${inActStreamTmp.flatten.flatten.length}")
  val inActAdrStreamTmp: Seq[List[Int]] = oneStream.take(inActGLBNum).map(x => x.inInActAdrRand)
  println(s"the length of inActAdrStreamTmp = ${inActAdrStreamTmp.flatten.length}")
  val inActCountStreamTmp: Seq[List[Int]] = oneStream.take(inActGLBNum).map(x => x.inInActCountRand)
  val inActDataStreamTmp: Seq[List[Int]] = oneStream.take(inActGLBNum).map(x => x.inInActDataRand)
  println(s"the length of inActDataStreamTmp = ${inActDataStreamTmp.flatten.length}")
  val weightStreamTmp: Seq[List[List[Int]]] = oneStream.take(weightGLBNum).map(x => x.weightList)
  println(s"the length of weightStream = ${weightStreamTmp.flatten.flatten.length}")
  val weightAdrStreamTmp: Seq[List[Int]] = oneStream.take(weightGLBNum).map(x => x.inWeightAdrRand)
  println(s"the length of weightAdrStreamTmp = ${weightAdrStreamTmp.flatten.length}")
  val weightCountStreamTmp: Seq[List[Int]] = oneStream.take(weightGLBNum).map(x => x.inWeightCountRand)
  val weightDataStreamTmp: Seq[List[Int]] = oneStream.take(weightGLBNum).map(x => x.inWeightDataRand)
  println(s"the length of weightDataStreamTmp = ${weightDataStreamTmp.flatten.length}")
  val outPSumStreamTmp: List[List[Int]] = goldenFlatStreamResult()
  println(s"the length of outPSumStreamTmp = ${outPSumStreamTmp.flatten.length}")
  private val (rightOrInAct, rightInActAdr, rightInActData,
  rightOrWeight, rightWeightAdr, rightWeightData, rightPSumData) = getThingsReady
  val inActStream: Seq[List[List[Int]]] = rightOrInAct
  val inActAdrStream: Seq[List[Int]] = rightInActAdr
  println(s"the length of inActAdrStream = ${inActAdrStream.flatten.length}")
  val inActDataStream: Seq[List[Int]] = rightInActData // has combined data and count
  val weightStream: Seq[List[List[Int]]] = rightOrWeight
  val weightAdrStream: Seq[List[Int]] = rightWeightAdr
  val weightDataStream: Seq[List[Int]] = rightWeightData // has combined data and count
  val outPSumStream: List[List[Int]] = rightPSumData.toList
  private def getThingsReady: (Seq[List[List[Int]]], Seq[List[Int]], Seq[List[Int]], Seq[List[List[Int]]],
    Seq[List[Int]], Seq[List[Int]], Seq[List[Int]]) = {
    require(S1 == 3 && F1 == 4, "you need to correct this data generation function to fit more situations") // TODO: remove
    var inActOrStream: List[List[List[Int]]] = Nil
    var inActAdr: List[List[Int]] = Nil
    var inActData: List[List[Int]] = Nil // include count
    var weightOrStream: List[List[List[Int]]] = Nil
    var weightAdr: List[List[Int]] = Nil
    var weightData: List[List[Int]] = Nil // include count
    var pSumData: List[List[Int]] = Nil
    for (g1 <- 0 until G1) {
      for (n1 <- 0 until N1) {
        for (m1 <- 0 until M1) {
          for (f1 <- 0 until F1) {
            for (c1 <- 0 until C1) {
              for (s1 <- 0 until S1) {
                var inActAdrTmp: List[List[Int]] = Nil
                var inActDataTmp: List[List[Int]] = Nil // include count
                var weightAdrTmp: List[List[Int]] = Nil
                var weightDataTmp: List[List[Int]] = Nil // include count
                var pSumDataTmp: List[List[Int]] = Nil
                var inActIdxTest: List[Int] = Nil
                for (i <- 0 until inActStreamNum) {
                  val inActIdx = i*inActParNum + g1*N1*C1*(F1 + S1) + n1*C1*(F1 + S1) + c1*(F1 + S1) + (f1 + s1)
                  inActIdxTest = inActIdxTest:::List(inActIdx)
                  inActOrStream = inActOrStream:::List(inActStreamTmp(inActIdx))
                  inActAdrTmp = inActAdrTmp:::List(inActAdrStreamTmp(inActIdx))
                  inActDataTmp = inActDataTmp:::List(combineDataAndCount(inActDataStreamTmp(inActIdx),
                    inActCountStreamTmp(inActIdx)).toList)
                }
                for (i <- 0 until weightStreamNum) {
                  val weightIdx = i*weightParNum + g1*M1*C1*S1 + m1*C1*S1 + c1*S1 + s1
                  weightOrStream = weightOrStream:::List(weightStreamTmp(weightIdx))
                  weightAdrTmp = weightAdrTmp:::List(weightAdrStreamTmp(weightIdx))
                  weightDataTmp = weightDataTmp:::List(combineDataAndCount(weightDataStreamTmp(weightIdx),
                    weightCountStreamTmp(weightIdx)).toList)
                }
                for (i <- 0 until pSumStreamNum) {
                  val pSumIdx = i*pSumParNum + g1*N1*M1*F1 + n1*M1*F1 + m1*F1 + f1
                  pSumDataTmp = pSumDataTmp:::List(outPSumStreamTmp(pSumIdx))
                }
                require(inActAdrTmp.flatten.length <= inActAdrSRAMSize, s"current inActAdr should fit in one inActSRAM, " +
                  s"but ${inActAdrTmp.flatten.length} > $inActAdrSRAMSize")
                require(inActDataTmp.flatten.length <= inActDataSRAMSize, s"current inActData should fit in one inActDataSRAM, " +
                  s"but ${inActDataTmp.flatten.length} > $inActDataSRAMSize")
                require(pSumDataTmp.flatten.length <= pSumSRAMSize, s"current pSumData should fit in one pSumSRAM, " +
                  s"but ${pSumDataTmp.flatten.length} > $pSumSRAMSize")
                inActAdr = inActAdr:::List(inActAdrTmp.flatten:::List(0))
                inActData = inActData:::List(inActDataTmp.flatten:::List(0))
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
                            val goldFlatPSum = goldenFlatResult(weightStreamTmp(weightIdx),
                              inActStreamTmp(inActIdx))
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

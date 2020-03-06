package dla.tests.petest

import chisel3._
import chisel3.tester._
import dla.pe._
import org.scalatest._
import dla.tests.GenOnePETestData
import scala.math.pow

class ProcessingElementSpecTest extends FlatSpec with ChiselScalatestTester with Matchers with SPadSizeConfig with MCRENFConfig with PESizeConfig {
  // def some common parameters and functions
  private val randOrNot = true // for debug, true then use random inAct and weight
  private val genHp = new GenOnePETestData
  private val pSumMax = pow(2, psDataWidth).toInt
  private val inActAdrMax = pow(2, inActAdrWidth).toInt
  private val weightAdrMax = pow(2, weightAdrWidth)
  private val scsDataMax = pow(2, cscDataWidth).toInt
  private val inActList: List[List[Int]] = genHp.inActList
  private val weightList: List[List[Int]] = genHp.weightList
  private val inInActAdrRand = genHp.inInActAdrRand
  private val inInActCountRand = genHp.inInActCountRand
  private val inInActDataRand = genHp.inInActDataRand
  private val inWeightAdrRand = genHp.inWeightAdrRand
  private val inWeightCountRand = genHp.inWeightCountRand
  private val inWeightDataRand = genHp.inWeightDataRand
  private val outPSumRand: List[Int] = genHp.outPSumRand
  if (randOrNot) {
    require(inInActAdrRand.head != inActZeroColumnCode, "the head of inAct should not be zero column") // TODO: remove this requirement
    inInActAdrRand.foreach(x => require(x <= inActAdrMax, s"inActAdr value should less than max, do $x < $inActAdrMax ?"))
    require(inInActAdrRand.length <= inActAdrSPadSize, "need to store all inAct address")
    require(inInActCountRand.length <= inActDataSPadSize, "need to store all inAct data")
    require(inWeightAdrRand.head != weightZeroColumnCode, "the head of weight should not be zero column") // TODO: remove this requirement
    inWeightAdrRand.foreach(x => require(x <= weightAdrMax, s"weightAdr value should less than max, do $x < $weightAdrMax ?"))
    require(inWeightAdrRand.length <= weightAdrSPadSize, "need to store all weight address")
    require(inWeightDataRand.length <= weightDataSPadSize, "need to store all weight data")
    outPSumRand.foreach(x => require(x <= pSumMax, s"pSum should less than max, do $x < $pSumMax ?"))
  }
  private val inWeightAdr = Seq(2, 5, weightZeroColumnCode, 6, 7, weightZeroColumnCode, 9, 12, 0) // weightZeroColumnCode means it is a zero column, don't record the first number
  private val inWeightData = Seq(1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 0) // zero column between 15 & 18, 24 & 27
  private val inWeightCount = Seq(1, 2, 0, 1, 3, 2, 3, 1, 3, 0, 1, 2, 0)
  private val inInActAdr = Seq(5, 9, inActZeroColumnCode, 11, 14, 0)
  private val inInActData = Seq(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 0)
  private val inInActCount = Seq(1, 3, 4, 6, 7, 2, 5, 6, 7, 0, 5, 0, 1, 3, 5, 0)
  private val outPSum = Seq(282, 318, 330, 132, 486, 834, 774, 336, 0, 482, 60, 528, 0, 0, 0, 0, 156, 258, 72, 312, 0, 630, 0, 720)
  // for basically write and read test
  private val inInActTestAdr = Seq(2, 5, inActZeroColumnCode, 6, 7, inActZeroColumnCode, 9, 12, 0) // 15 means it is a zero column, don't record the first number
  private val inInActTestAdr2 = Seq(2, 5, 7, 8, inActZeroColumnCode, inActZeroColumnCode, inActZeroColumnCode, 9, 0)
  private val outWeightColumn = Seq(0, 0, 1, 1, 1, 2, 4, 5, 5, 7, 7, 7) // 3, 6 are zero column
  private val outWeightColumn2 = Seq(0, 0, 1, 1, 1, 2, 2, 3, 4, 8, 8, 8)
  val outWeightCycleType = Seq(2, 1, 2, 1, 1, 0, 2, 2, 0, 2, 1, 2, 1, 1) // 0 for zero column, 1 for data read only read cycle,
                    // 2 for read cycle which contains both address and data read
  val outWeightCycleType2 = Seq(2, 1, 2, 1, 1, 2, 1, 2, 0, 0, 0, 2, 2, 1, 1) // read the zero numbers after the column then read data
  /*
  * The first matrix is           The second matrix is
  * | data | row | col |          | data | row | col |
  * | ---- | --- | --- |          | ---- | --- | --- |
  * |   1  |  1  |  0  |          |   1  |  1  |  0  |
  * |   3  |  2  |  0  |          |   3  |  2  |  0  |
  * |   6  |  0  |  1  |          |   6  |  0  |  1  |
  * |   9  |  1  |  1  |          |   9  |  1  |  1  |
  * |  12  |  3  |  1  |          |  12  |  3  |  1  |
  * |  15  |  2  |  2  |          |  15  |  2  |  2  |
  * |  18  |  3  |  4  |          |  18  |  3  |  2  |
  * |  21  |  1  |  5  |          |  21  |  1  |  3  |
  * |  24  |  3  |  5  |          |  24  |  3  |  4  |
  * |  27  |  0  |  7  |          |  27  |  0  |  8  |
  * |  30  |  1  |  7  |          |  30  |  1  |  8  |
  * |  33  |  2  |  7  |          |  33  |  2  |  8  |
  */
  private def toBinary(i: Int, digits: Int = 8): String =
    String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0')
  private def combineDataAndCount(theData: Seq[Int], theCount: Seq[Int]): Seq[Int] = { // input data and count, and combine them together
    val theDataWithCount: Seq[(Int, Int)] = theData zip theCount
    val theDataCountBinary: Seq[String] = theDataWithCount.map{case (x: Int, y: Int) => toBinary(x) + toBinary(y, 4)}
    val theDataCountDec: Seq[Int] = theDataCountBinary.map(x => Integer.parseInt(x, 2))
    theDataCountDec
  }
  val inWeightDataCountDec: Seq[Int] = combineDataAndCount(inWeightData, inWeightCount)
  val inInActDataCountDec: Seq[Int] = combineDataAndCount(inInActData, inInActCount)
  val inWeightDataCountDecRand: Seq[Int] = combineDataAndCount(inWeightDataRand, inWeightCountRand)
  val inInActDataCountDecRand: Seq[Int] = combineDataAndCount(inInActDataRand, inInActCountRand)
  private def signalReadInFuc(readInData: Seq[Int], readInIO: StreamBitsIO, theClock: Clock, theWF: Bool): Any = {
    readInIO.data.valid.poke(true.B)
    for (i <- readInData.indices) {
      readInIO.data.bits.poke(readInData(i).U)
      theWF.expect((i == readInData.length - 1).B, s"[write cycle $i @ $readInIO] should it finish writing?")
      readInIO.data.ready.expect(true.B, s"[write cycle $i @ $readInIO] write valid, after receive the data, it should be ready")
      theClock.step()
    }
    readInIO.data.valid.poke(false.B)
  }

  private def inActAndWeightReadInFuc(IAData: Seq[Int], IDData: Seq[Int], WAData: Seq[Int], WDData: Seq[Int], theSPadIO: DataStreamIO, theClock: Clock, theWF: PEPadWriteFinIO): Any = {
    require(IAData.length <= inActAdrSPadSize, s"input address data has ${IAData.length} elements, which exceeds the size of InActAdrSPad size $inActAdrSPadSize")
    require(IDData.length <= inActDataSPadSize, s"input address data has ${IDData.length} elements, which exceeds the size of InActDataSPad size $inActDataSPadSize")
    require(WAData.length <= weightAdrSPadSize, s"input address data has ${WAData.length} elements, which exceeds the size of WeightAdrSPad size $weightAdrSPadSize")
    require(WDData.length <= weightDataSPadSize, s"input address data has ${WDData.length} elements, which exceeds the size of WeightDataSPad size $weightDataSPadSize")
    fork {
      signalReadInFuc(IAData, theSPadIO.inActIOs.adrIOs, theClock, theWF.inActWriteFin.adrWriteFin)
    } .fork {
      signalReadInFuc(IDData, theSPadIO.inActIOs.dataIOs, theClock, theWF.inActWriteFin.dataWriteFin)
    } .fork {
      signalReadInFuc(WAData, theSPadIO.weightIOs.adrIOs, theClock, theWF.weightWriteFin.adrWriteFin)
    } .fork {
      signalReadInFuc(WDData, theSPadIO.weightIOs.dataIOs, theClock, theWF.weightWriteFin.dataWriteFin)
    } .join()
  }

  private def PEScratchPadWriteIn(inInActAdr: Seq[Int], inInActData: Seq[Int], inWeightAdr: Seq[Int], inWeightData: Seq[Int], topModule: ProcessingElementPad): Any = {
    val theTopSPadIO = topModule.io.dataStream
    val theClock = topModule.clock
    topModule.io.padCtrl.doMACEn.poke(false.B)
    inActAndWeightReadInFuc(inInActAdr, inInActData, inWeightAdr, inWeightData, theTopSPadIO, theClock, topModule.io.padWF)
  }

  private def simplyWriteInDataAndAdr(inAddress: Seq[Int], inData: Seq[Int], topModule: SimplyCombineAdrDataSPad): Any = {
    require(inAddress.length <= inActAdrSPadSize, s"input address data has ${inAddress.length} elements, which exceeds the size of SPad size $inActAdrSPadSize")
    require(inData.length <= inActDataSPadSize, s"input address data has ${inData.length} elements, which exceeds the size of SPad size $inActDataSPadSize")
    val theTopIO = topModule.io.inActIOs
    val theDataReq = topModule.io.inActDataReq
    val theClock = topModule.clock
    theDataReq.poke(false.B)
    topModule.io.writeEn.poke(true.B)
    fork { // run them in parallel
      signalReadInFuc(inAddress, theTopIO.adrIOs, theClock, topModule.io.inActWF.adrWriteFin)
    } .fork {
      signalReadInFuc(inData, theTopIO.dataIOs, theClock, topModule.io.inActWF.dataWriteFin)
    } .join()
    topModule.io.writeEn.poke(false.B)
  }

  private def simplyCheckSignal(cycle: Int, topModule: SimplyCombineAdrDataSPad, outWeightCycleType: Seq[Int], readDataTimes: Int, readInData: Seq[Int], readInRow: Seq[Int], readInColumn: Seq[Int]): Any = outWeightCycleType(cycle) match {
    case 0 =>
      println(s"---------------- read cycle $cycle --------------")
      println(s"--- meets a zero column at $cycle read cycle ---")
      println(s"adrReadEn = ${topModule.io.inActAdrReadEn.peek()}, adrReadData = ${topModule.io.inActAdrReadData.peek()}, dataReadIndex = ${topModule.io.inActDataReadIndex.peek()}")
      println(s"data = ${topModule.io.inActMatrixData.peek()}, row = ${topModule.io.inActMatrixRow.peek()}, column = ${topModule.io.inActMatrixColumn.peek()}")
      topModule.clock.step() // from address SPad to next address SPad read
    case 1 =>
      println(s"---------------- read cycle $cycle --------------")
      println(s"--- meets a data read only cycle at $cycle read cycle ---")
      topModule.io.inActMatrixData.expect(readInData(readDataTimes).U, s"read out data should be ${readInData(readDataTimes)} at $readDataTimes-th read cycle $cycle")
      topModule.io.inActMatrixRow.expect(readInRow(readDataTimes).U, s"read out data should be ${readInRow(readDataTimes)} at $readDataTimes-th read cycle $cycle")
      topModule.io.inActMatrixColumn.expect(readInColumn(readDataTimes).U, s"read out data should be ${readInColumn(readDataTimes)} at $readDataTimes-th read cycle $cycle")
      println(s"data = ${topModule.io.inActMatrixData.peek()}, row = ${topModule.io.inActMatrixRow.peek()}, column = ${topModule.io.inActMatrixColumn.peek()}")
      topModule.clock.step() // goto next one
    case 2 =>
      println(s"---------------- read cycle $cycle --------------")
      println(s"--- meets a common read cycle at $cycle read cycle ---")
      println(s"adrReadEn = ${topModule.io.inActAdrReadEn.peek()}, adrReadData = ${topModule.io.inActAdrReadData.peek()}, dataReadIndex = ${topModule.io.inActDataReadIndex.peek()}")
      topModule.clock.step() // from address SPad read to data SPad read
      topModule.io.inActMatrixData.expect(readInData(readDataTimes).U, s"read out data should be ${readInData(readDataTimes)} at $readDataTimes-th read cycle $cycle")
      topModule.io.inActMatrixRow.expect(readInRow(readDataTimes).U, s"read out data should be ${readInRow(readDataTimes)} at $readDataTimes-th read cycle $cycle")
      topModule.io.inActMatrixColumn.expect(readInColumn(readDataTimes).U, s"read out data should be ${readInColumn(readDataTimes)} at $readDataTimes-th read cycle $cycle")
      println(s"data = ${topModule.io.inActMatrixData.peek()}, row = ${topModule.io.inActMatrixRow.peek()}, column = ${topModule.io.inActMatrixColumn.peek()}")
      topModule.clock.step() // goto next one
  }

  /*private def peSpecSignalCheck(cycle: Int, topModule: ProcessingElementPad, outWeightCycleType: Seq[Int], outInActCycleType: Seq[Int]): Any = (outWeightCycleType(cycle), outInActCycleType(cycle)) match {
    case (0, _) =>
      println(s"---------------- read cycle $cycle --------------")
      println(s"--- meets a zero weight column at $cycle read cycle ---")
      topModule.io.debugIO.sPadState.expect(2.U, s"the SPad state should be 2, inAct data read at $cycle")
      topModule.clock.step()
      topModule.io.debugIO.sPadState.expect(3.U, s"the SPad state should be 3 after one clock, weight address read at $cycle")
      topModule.io.debugIO.weightAdrSPadReadOut.expect(weightZeroColumnCode.U, s"the weight address read out should be $weightZeroColumnCode, weight data read at $cycle")
      topModule.clock.step()
    case (1, _) =>
      println(s"---------------- read cycle $cycle --------------")
      println(s"--- meets a weight data read only cycle at $cycle read cycle ---")

  }
  */

  behavior of "test the spec of Processing Element"

  it should "try to run PE with control and CSC SPad module" in {
    test(new ProcessingElement(true)) { thePE =>
      val theTopIO = thePE.io
      val theClock = thePE.clock
      println("----------------- test begin -----------------")
      println("----------- Processing Element Module ------------")
      thePE.reset.poke(true.B)
      theClock.step()
      thePE.reset.poke(false.B)
      println("--------------- begin to write ---------------")
      theTopIO.topCtrl.pSumEnqOrProduct.poke(false.B)
      theTopIO.topCtrl.doLoadEn.poke(true.B)
      if (randOrNot) {
        println("outPSum    = " + outPSumRand)
        //println("inActList  = " + inActList)
        println("inActAdr   = " + inInActAdrRand)
        println("inActData   = " + inInActDataRand)
        //println("weightList = " + weightList)
        println("weightAdr  = " + inWeightAdrRand)
        println("weightData  = " + inWeightDataRand)
        inActAndWeightReadInFuc(inInActAdrRand, inInActDataCountDecRand, inWeightAdrRand, inWeightDataCountDecRand,
          theTopIO.dataStream, theClock, theTopIO.padWF)
      } else {
        inActAndWeightReadInFuc(inInActAdr, inInActDataCountDec, inWeightAdr, inWeightDataCountDec,
          theTopIO.dataStream, theClock, theTopIO.padWF)
      }
      theTopIO.debugIO.peControlDebugIO.peState.expect(1.U, "wait it jump from load to cal")
      theTopIO.debugIO.peControlDebugIO.doMACEnDebug.expect(false.B, s"now it should be load")
      theClock.step()
      theTopIO.topCtrl.writeFinish.expect(true.B, s"after write in all data, write should finish")
      theTopIO.debugIO.peControlDebugIO.doMACEnDebug.expect(true.B, s"now it should be calculating state")
      theTopIO.debugIO.peControlDebugIO.peState.expect(2.U, "now it should be calculating state")
      theTopIO.topCtrl.doLoadEn.poke(false.B)
      theClock.step()
      println("--------------- begin to MAC ----------------")
      var i = 0
      while (!theTopIO.topCtrl.calFinish.peek().litToBoolean) {
        println(s"--------------- $i-th MAC cycle -----------")
        println(s"----- SPad State   =  ${theTopIO.debugIO.peSPadDebugIO.sPadState.peek()}")
        println(s"----- inActMatrix   = ( value = ${theTopIO.debugIO.peSPadDebugIO.inActMatrixData.peek().litValue()}, " +
          s"row = ${theTopIO.debugIO.peSPadDebugIO.inActMatrixRow.peek().litValue()}, " +
          s"column = ${theTopIO.debugIO.peSPadDebugIO.inActMatrixColumn.peek().litValue()})")
        println(s"----- IAdrIndex   =  ${theTopIO.debugIO.peSPadDebugIO.inActAdrIdx.peek()}, " +
          s"${theTopIO.debugIO.peSPadDebugIO.inActAdrInc.peek()}")
        println(s"----- IDataInc     =  ${theTopIO.debugIO.peSPadDebugIO.inActDataInc.peek()}")
        println(s"----- weightMatrix = ( value = ${theTopIO.debugIO.peSPadDebugIO.weightMatrixData.peek().litValue()}, " +
          s"row = ${theTopIO.debugIO.peSPadDebugIO.weightMatrixRow.peek().litValue()}, " +
          s"column = ${theTopIO.debugIO.peSPadDebugIO.inActMatrixRow.peek().litValue()})")
        println(s"----- WAdrData    =  ${theTopIO.debugIO.peSPadDebugIO.weightAdrSPadReadOut.peek()}")
        println(s"----- WAInIndex    =  ${theTopIO.debugIO.peSPadDebugIO.weightAdrInIdx.peek()}")
        println(s"----- product      =  ${theTopIO.debugIO.peSPadDebugIO.productResult.peek()}")
        println(s"----- pSumResult   =  ${theTopIO.debugIO.peSPadDebugIO.pSumResult.peek()}")
        println(s"----- pSumLoad     =  ${theTopIO.debugIO.peSPadDebugIO.pSumLoad.peek()}")
        theClock.step()
        i = i + 1
      }
      theClock.step()
      println("-------------- MAC now finish ----------------")
      println(s"----- peState      =  ${theTopIO.debugIO.peControlDebugIO.peState.peek()}")
      println(s"----- doMACEn      =  ${theTopIO.debugIO.peControlDebugIO.doMACEnDebug.peek()}")
      println(s"whether MAC finish =  ${theTopIO.topCtrl.calFinish.peek()}")
      println(s"----- pSumReadOut  =  ${theTopIO.dataStream.opsIO.bits.peek()}")
      println("-------------- read partial sum ---------------")
      theTopIO.topCtrl.doLoadEn.poke(true.B)
      theTopIO.debugIO.peSPadDebugIO.sPadState.expect(0.U, "the SPad state should be idle when read out partial sum")
      println(s"----- peState      =  ${theTopIO.debugIO.peControlDebugIO.peState.peek()}")
      println(s"----- doMACEn      =  ${theTopIO.debugIO.peControlDebugIO.doMACEnDebug.peek()}")
      println(s"whether MAC finish =  ${theTopIO.topCtrl.calFinish.peek()}")
      println(s"----- pSumReadOut  =  ${theTopIO.dataStream.opsIO.bits.peek()}")
      theClock.step()
      theTopIO.dataStream.opsIO.ready.poke(true.B)
      theClock.step(fifoSize) // wait for fifo size clock cycle to let data flow out
      for (i <- 0 until M0*E*N0*F0) {
        println(s"--------- $i-th pSumSPad read cycle --------")
        theTopIO.debugIO.peControlDebugIO.peState.expect(1.U, s"the control state should be load when read out partial sun")
        theTopIO.debugIO.peSPadDebugIO.sPadState.expect(0.U, "the SPad state should be idle when read out partial sum")
        println(s"----- pSumReadOut  =  ${theTopIO.dataStream.opsIO.bits.peek()}")
        if (randOrNot) {
          theTopIO.dataStream.opsIO.bits.expect(outPSumRand(i).U, s"the out partial sum should be ${outPSumRand(i)} at $i-th index")
        } else {
          theTopIO.dataStream.opsIO.bits.expect(outPSum(i).U, s"the out partial sum should be ${outPSum(i)} at $i-th index")
        }
        theClock.step()
      }
    }
  }

  it should "try to run PE SPad with CSC compressed data" in {
    test(new ProcessingElementPad(true)) { thePESPad =>
      val theTopIO = thePESPad.io
      val theClock = thePESPad.clock
      println("----------------- test begin -----------------")
      println("----------- PE Scratch Pad Module ------------")
      thePESPad.reset.poke(true.B)
      theClock.step()
      thePESPad.reset.poke(false.B)
      println("--------------- begin to write ---------------")
      theTopIO.padCtrl.fromTopIO.pSumEnqOrProduct.poke(false.B)
      theTopIO.padCtrl.fromTopIO.doLoadEn.poke(true.B)
      if (randOrNot) {
        PEScratchPadWriteIn(inInActAdrRand, inInActDataCountDecRand, inWeightAdrRand, inWeightDataCountDecRand, thePESPad)
      } else {
        PEScratchPadWriteIn(inInActAdr, inInActDataCountDec, inWeightAdr, inWeightDataCountDec, thePESPad)
      }
      theTopIO.padCtrl.fromTopIO.doLoadEn.poke(false.B)
      println("--------------- begin to MAC ----------------")
      theTopIO.padCtrl.doMACEn.poke(true.B) // start the state machine
      theClock.step() // from idle to address SPad read
      theTopIO.padCtrl.doMACEn.poke(false.B) // end the state machine
      for (i<- 0 until 133) {
        println(s"--------------- $i-th MAC cycle -----------")
        println(s"----- SPad State   =  ${theTopIO.debugIO.sPadState.peek()}")
        println(s"----- inActMatrix   = (${theTopIO.debugIO.inActMatrixData.peek()}, ${theTopIO.debugIO.inActMatrixRow.peek()}, ${theTopIO.debugIO.inActMatrixColumn.peek()})")
        println(s"----- IAdrIndex   =  ${theTopIO.debugIO.inActAdrIdx.peek()}, ${theTopIO.debugIO.inActAdrInc.peek()}")
        println(s"----- IDataInc     =  ${theTopIO.debugIO.inActDataInc.peek()}")
        println(s"----- weightMatrix = (${theTopIO.debugIO.weightMatrixData.peek()}, ${theTopIO.debugIO.weightMatrixRow.peek()}, ${theTopIO.debugIO.inActMatrixRow.peek()})")
        println(s"----- WAdrData    =  ${theTopIO.debugIO.weightAdrSPadReadOut.peek()}")
        println(s"----- WAInIndex    =  ${theTopIO.debugIO.weightAdrInIdx.peek()}")
        println(s"----- product      =  ${theTopIO.debugIO.productResult.peek()}")
        println(s"----- pSumResult   =  ${theTopIO.debugIO.pSumResult.peek()}")
        println(s"----- pSumLoad     =  ${theTopIO.debugIO.pSumLoad.peek()}")
        theClock.step()
      }
      theClock.step(2)
      println("-------------- read partial sum ---------------")
      theTopIO.padCtrl.fromTopIO.doLoadEn.poke(true.B) // begin to read out partial sum
      theTopIO.dataStream.opsIO.ready.poke(true.B)
      theTopIO.debugIO.sPadState.expect(0.U, "the SPad state should be idle when read out partial sum")
      for (i <- 0 until M0*E*N0*F0) {
        println(s"--------- $i-th pSumSPad read cycle --------")
        println(s"----- pSumReadOut  =  ${theTopIO.dataStream.opsIO.bits.peek()}")
        if (randOrNot) {
          theTopIO.dataStream.opsIO.bits.expect(outPSumRand(i).U, s"the out partial sum should be ${outPSumRand(i)} at $i-th index")
        } else {
          theTopIO.dataStream.opsIO.bits.expect(outPSum(i).U, s"the out partial sum should be ${outPSum(i)} at $i-th index")
        }
        theClock.step()
      }
    }
  }

  it should "try to read and write data in csc format" in {
    test(new SimplyCombineAdrDataSPad) { inActSPad =>
      val theTopIO = inActSPad.io
      val theClock = inActSPad.clock
      println("---------- begin to test the read ----------")
      println("------ and write address in InAct SPad ------")
      inActSPad.reset.poke(true.B)
      theClock.step()
      inActSPad.reset.poke(false.B)
      println("-------------- begin to write --------------")
      simplyWriteInDataAndAdr(inInActTestAdr, inWeightDataCountDec, inActSPad)
      println("-------------- begin to read ---------------")
      theTopIO.inActDataReq.poke(true.B) // start the state machine
      theClock.step() // from idle to address SPad read
      var j: Int = 0
      for (i <- outWeightCycleType.indices) {
        simplyCheckSignal(i, inActSPad, outWeightCycleType, j, inWeightData, inWeightCount, outWeightColumn)
        if (outWeightCycleType(i) != 0) {
          j = j + 1
        }
      }
    }
  }

  it should "write and read data in csc format with continued zero columns" in {
    test(new SimplyCombineAdrDataSPad) { inActSPad =>
      val theTopIO = inActSPad.io
      val theClock = inActSPad.clock
      println("---------- begin to test the read ----------")
      println("------ and write address in InAct SPad ------")
      println("-------- with continued zero columns -------")
      inActSPad.reset.poke(true.B)
      theClock.step()
      inActSPad.reset.poke(false.B)
      println("------------- begin to write ---------------")
      simplyWriteInDataAndAdr(inInActTestAdr2, inWeightDataCountDec, inActSPad)
      println("------------- begin to read ----------------")
      theTopIO.inActDataReq.poke(true.B) // start the state machine
      theClock.step() // from idle to address SPad read
      var j: Int = 0
      for (i <- outWeightCycleType2.indices) {
        simplyCheckSignal(i, inActSPad, outWeightCycleType2, j, inWeightData, inWeightCount, outWeightColumn2)
        if (outWeightCycleType2(i) != 0) {
          j = j + 1
        }
      }
    }
  }

  it should "basically write and read address in InAct SPad with CSC format data" in {
    test(new SPadAdrModule( 9, 4)) { adrSPad =>
      val theDataPath = adrSPad.io.dataPath
      val theCtrlPath = adrSPad.io.ctrlPath
      val theDataIO = theDataPath.writeInData.data
      val theClock = adrSPad.clock
      println("--- begin to test the read and write address in InAct SPad ---")
      println("----------- begin to write -----------")
      theDataIO.valid.poke(true.B)
      theCtrlPath.writeEn.poke(true.B)
      theCtrlPath.readEn.poke(false.B)
      for (i <- inInActTestAdr.indices) {
        theDataIO.bits.poke(inInActTestAdr(i).U)
        theCtrlPath.writeIdx.expect(i.U, s"i = $i")
        theCtrlPath.writeFin.expect((i == inInActTestAdr.length - 1).B, s"i = $i")
        theDataIO.ready.expect(true.B, "write valid, after receive the data, it should be ready")
        theClock.step()
      }
      println("----------- begin to read -----------")
      theDataIO.valid.poke(false.B)
      theCtrlPath.writeEn.poke(false.B)
      theCtrlPath.readEn.poke(true.B)
      theCtrlPath.indexInc.poke(true.B) // INCREASE ALL THE TIME
      for (i <- 0 until (inInActTestAdr.length - 1)) {
        println(s"----------- read clock $i -----------")
        theDataPath.columnNum.expect(i.U, s"columnNum = $i")
        theDataPath.readOutData.expect(inInActTestAdr(i).U, s"readOutData = inWeightData($i) = ${inInActTestAdr(i)}")
        println(s"theCtrlPath.columnNum = $i")
        println(s"theCtrlPath.readOutData = ${inInActTestAdr(i)}")
        theClock.step()
      }
      theDataPath.readOutData.expect(inInActTestAdr.last.U, s"readOutData = inWeightData(${inInActTestAdr.length - 1}) = ${inInActTestAdr.last}")
    }
  }

  it should "basically write and read data in InAct SPad" in {
    test(new SPadDataModule( 16, 12, false)) { dataSPad =>
      val theDataPath = dataSPad.io.dataPath
      val theCtrlPath = dataSPad.io.ctrlPath
      val theDataIO = theDataPath.writeInData.data
      val theClock = dataSPad.clock
      val theData = if (randOrNot) inWeightDataCountDecRand else inWeightDataCountDec
      println("--- begin to test the read and write data in InAct SPad ---")
      println("----------- begin to write -----------")
      theDataIO.valid.poke(true.B)
      theCtrlPath.writeEn.poke(true.B)
      theCtrlPath.readEn.poke(false.B)
      for (i <- theData.indices) {
        theDataIO.bits.poke(theData(i).U)
        theDataIO.ready.expect(true.B, "write valid, after receive the data, it should be ready")
        theCtrlPath.writeFin.expect((i == theData.length - 1).B, s"i = $i")
        theCtrlPath.writeIdx.expect(i.U, s"i = $i")
        theClock.step()
      }
      println("----------- begin to read -----------")
      theDataIO.valid.poke(false.B)
      theCtrlPath.writeEn.poke(false.B)
      theCtrlPath.readEn.poke(true.B)
      theCtrlPath.indexInc.poke(true.B) // INCREASE ALL THE TIME
      for (i <- theData.indices) {
        println(s"----------- read clock $i -----------")
        theDataPath.columnNum.expect(i.U, s"columnNum = $i in read clock $i")
        theDataPath.readOutData.expect(theData(i).U, s"readOutData = inWeightDataCountDec($i) = ${theData(i)}")
        println(s"theCtrlPath.columnNum = $i")
        println(s"theCtrlPath.readOutData = ${theData(i)}")
        theClock.step()
      }
      theDataPath.columnNum.expect(0.U, s"new read turn begins, columnNum = 0")
      theDataPath.readOutData.expect(theData.head.U, s"new read turn begins, readOutData = ${theData.head}")
    }
  }
}

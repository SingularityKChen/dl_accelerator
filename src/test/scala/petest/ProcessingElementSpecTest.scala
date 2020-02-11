package dla.test.petest

import chisel3._
import chisel3.tester._
import dla.pe._
import org.scalatest._

class ProcessingElementSpecTest extends FlatSpec with ChiselScalatestTester with Matchers with SPadSizeConfig with MCRENFConfig {
  // def some common parameters and functions
  val inWeightAddr = Seq(2, 5, weightZeroColumnCode, 6, 7, weightZeroColumnCode, 9, 12, 0) // weightZeroColumnCode means it is a zero column, don't record the first number
  val inWeightData = Seq(1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 0) // zero column between 15 & 18, 24 & 27
  val inWeightCount = Seq(1, 2, 0, 1, 3, 2, 3, 1, 3, 0, 1, 2, 0)
  val inIactAddr = Seq(5, 9, iactZeroColumnCode, 11, 14, 0)
  val inIactData = Seq(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 0)
  val inIactCount = Seq(1, 3, 4, 6, 7, 2, 5, 6, 7, 0, 5, 0, 1, 3, 5, 0)
  val outpSum = Seq(282, 318, 330, 132, 486, 834, 774, 336, 0, 482, 60, 528, 0, 0, 0, 0, 156, 258, 72, 312, 0, 630, 0, 720)
  // for basically write and read test
  val inIactTestAddr = Seq(2, 5, iactZeroColumnCode, 6, 7, iactZeroColumnCode, 9, 12, 0) // 15 means it is a zero column, don't record the first number
  val inIactTestAddr2 = Seq(2, 5, 7, 8, iactZeroColumnCode, iactZeroColumnCode, iactZeroColumnCode, 9, 0)
  val outWeightColumn = Seq(0, 0, 1, 1, 1, 2, 4, 5, 5, 7, 7, 7) // 3, 6 are zero column
  val outWeightColumn2 = Seq(0, 0, 1, 1, 1, 2, 2, 3, 4, 8, 8, 8)
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
  def toBinary(i: Int, digits: Int = 8): String =
    String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0')
  def combineDataAndCount(theData: Seq[Int], theCount: Seq[Int]): Seq[Int] = { // input data and count, and combine them together
    val theDataWithCount: Seq[(Int, Int)] = theData zip theCount
    val theDataCountBinary: Seq[String] = theDataWithCount.map{case (x: Int, y: Int) => toBinary(x) + toBinary(y, 4)}
    val theDataCountDec: Seq[Int] = theDataCountBinary.map(x => Integer.parseInt(x, 2))
    theDataCountDec
  }
  val inWeightDataCountDec: Seq[Int] = combineDataAndCount(inWeightData, inWeightCount)
  val inIactDataCountDec: Seq[Int] = combineDataAndCount(inIactData,inIactCount)

  def signalReadInFuc(readInData: Seq[Int], readInIO: StreamBitsIO, theClock: Clock, theWF: Bool): Any = {
    readInIO.data.valid.poke(true.B)
    for (i <- readInData.indices) {
      readInIO.data.bits.poke(readInData(i).U)
      theWF.expect((i == readInData.length - 1).B, s"[write cycle $i @ $readInIO] should it finish writing?")
      readInIO.data.ready.expect(true.B, s"[write cycle $i @ $readInIO] write valid, after receive the data, it should be ready")
      theClock.step(1)
    }
    readInIO.data.valid.poke(false.B)
  }

  def iactAndWeightReadInFuc(IAData: Seq[Int], IDData: Seq[Int], WAData: Seq[Int], WDData: Seq[Int], theSPadIO: DataStreamIO, theClock: Clock, theWF: PEPadWriteFinIO): Any = {
    require(IAData.length <= iactAddrSPadSize, s"input address data has ${IAData.length} elements, which exceeds the size of SPad size ${iactAddrSPadSize}")
    require(IDData.length <= iactDataSPadSize, s"input address data has ${IDData.length} elements, which exceeds the size of SPad size ${iactDataSPadSize}")
    require(WAData.length <= weightAddrSPadSize, s"input address data has ${WAData.length} elements, which exceeds the size of SPad size ${weightAddrSPadSize}")
    require(WDData.length <= weightDataSPadSize, s"input address data has ${WDData.length} elements, which exceeds the size of SPad size ${weightDataSPadSize}")
    fork {
      signalReadInFuc(IAData, theSPadIO.iactIOs.addrIOs, theClock, theWF.iactWriteFin.addrWriteFin)
    } .fork {
      signalReadInFuc(IDData, theSPadIO.iactIOs.dataIOs, theClock, theWF.iactWriteFin.dataWriteFin)
    } .fork {
      signalReadInFuc(WAData, theSPadIO.weightIOs.addrIOs, theClock, theWF.weightWriteFin.addrWriteFin)
    } .fork {
      signalReadInFuc(WDData, theSPadIO.weightIOs.dataIOs, theClock, theWF.weightWriteFin.dataWriteFin)
    } .join()
  }

  def PEScratchPadWriteIn(inIactAddr: Seq[Int], inIactData: Seq[Int], inWeightAddr: Seq[Int], inWeightData: Seq[Int], topModule: ProcessingElementPad): Any = {
    val theTopSPadIO = topModule.io.dataStream
    val theClock = topModule.clock
    topModule.io.padCtrl.fromTopIO.pSumEnqOrProduct.bits.poke(false.B)
    topModule.io.padCtrl.doMACEn.poke(false.B)
    iactAndWeightReadInFuc(inIactAddr, inIactData, inWeightAddr, inWeightData, theTopSPadIO, theClock, topModule.io.padWF)
  }

  def simplyWriteInDataAndAddr(inAddress: Seq[Int], inData: Seq[Int], topModule: SimplyCombineAddrDataSPad): Any = {
    require(inAddress.length <= iactAddrSPadSize, s"input address data has ${inAddress.length} elements, which exceeds the size of SPad size ${iactAddrSPadSize}")
    require(inData.length <= iactDataSPadSize, s"input address data has ${inData.length} elements, which exceeds the size of SPad size ${iactDataSPadSize}")
    val theTopIO = topModule.io.iactIOs
    val theDataReq = topModule.io.iactDataReq
    val theClock = topModule.clock
    theDataReq.poke(false.B)
    topModule.io.writeEn.poke(true.B)
    fork { // run them in parallel
      signalReadInFuc(inAddress, theTopIO.addrIOs, theClock, topModule.io.iactWF.addrWriteFin)
    } .fork {
      signalReadInFuc(inData, theTopIO.dataIOs, theClock, topModule.io.iactWF.dataWriteFin)
    } .join()
    topModule.io.writeEn.poke(false.B)
  }

  def simplyCheckSignal(cycle: Int, topModule: SimplyCombineAddrDataSPad, outWeightCycleType: Seq[Int], readDataTimes: Int, readInData: Seq[Int], readInRow: Seq[Int], readInColumn: Seq[Int]): Any = outWeightCycleType(cycle) match {
    case 0 =>
      println(s"---------------- read cycle $cycle --------------")
      println(s"--- meets a zero column at $cycle read cycle ---")
      println(s"addrReadEn = ${topModule.io.iactAddrReadEn.peek()}, addrReadData = ${topModule.io.iactAddrReadData.peek()}, dataReadIndex = ${topModule.io.iactDataReadIndex.peek()}")
      println(s"data = ${topModule.io.iactMatrixData.peek()}, row = ${topModule.io.iactMatrixRow.peek()}, column = ${topModule.io.iactMatrixColumn.peek()}")
      topModule.clock.step(1) // from address SPad to next address SPad read
    case 1 =>
      println(s"---------------- read cycle $cycle --------------")
      println(s"--- meets a data read only cycle at $cycle read cycle ---")
      topModule.io.iactMatrixData.expect(readInData(readDataTimes).U, s"read out data should be ${readInData(readDataTimes)} at $readDataTimes-th read cycle $cycle")
      topModule.io.iactMatrixRow.expect(readInRow(readDataTimes).U, s"read out data should be ${readInRow(readDataTimes)} at $readDataTimes-th read cycle $cycle")
      topModule.io.iactMatrixColumn.expect(readInColumn(readDataTimes).U, s"read out data should be ${readInColumn(readDataTimes)} at $readDataTimes-th read cycle $cycle")
      println(s"data = ${topModule.io.iactMatrixData.peek()}, row = ${topModule.io.iactMatrixRow.peek()}, column = ${topModule.io.iactMatrixColumn.peek()}")
      topModule.clock.step(1) // goto next one
    case 2 =>
      println(s"---------------- read cycle $cycle --------------")
      println(s"--- meets a common read cycle at $cycle read cycle ---")
      println(s"addrReadEn = ${topModule.io.iactAddrReadEn.peek()}, addrReadData = ${topModule.io.iactAddrReadData.peek()}, dataReadIndex = ${topModule.io.iactDataReadIndex.peek()}")
      topModule.clock.step(1) // from address SPad read to data SPad read
      topModule.io.iactMatrixData.expect(readInData(readDataTimes).U, s"read out data should be ${readInData(readDataTimes)} at $readDataTimes-th read cycle $cycle")
      topModule.io.iactMatrixRow.expect(readInRow(readDataTimes).U, s"read out data should be ${readInRow(readDataTimes)} at $readDataTimes-th read cycle $cycle")
      topModule.io.iactMatrixColumn.expect(readInColumn(readDataTimes).U, s"read out data should be ${readInColumn(readDataTimes)} at $readDataTimes-th read cycle $cycle")
      println(s"data = ${topModule.io.iactMatrixData.peek()}, row = ${topModule.io.iactMatrixRow.peek()}, column = ${topModule.io.iactMatrixColumn.peek()}")
      topModule.clock.step(1) // goto next one
  }

  def peSpecSignalCheck(cycle: Int, topModule: ProcessingElementPad, outWeightCycleType: Seq[Int], outIactCycleType: Seq[Int]): Any = (outWeightCycleType(cycle), outIactCycleType(cycle)) match {
    case (0, _) =>
      println(s"---------------- read cycle $cycle --------------")
      println(s"--- meets a zero weight column at $cycle read cycle ---")
      topModule.io.debugIO.sPadState.expect(2.U, s"the SPad state should be 2, iact data read at $cycle")
      topModule.clock.step(1)
      topModule.io.debugIO.sPadState.expect(3.U, s"the SPad state should be 3 after one clock, weight address read at $cycle")
      topModule.io.debugIO.weightAddrSPadReadOut.expect(weightZeroColumnCode.U, s"the weight address read out should be $weightZeroColumnCode, weight data read at $cycle")
      topModule.clock.step(1)
    case (1, _) =>
      println(s"---------------- read cycle $cycle --------------")
      println(s"--- meets a weight data read only cycle at $cycle read cycle ---")

  }

  behavior of "test the spec of Processing Element Module with CSC format data"

  it should "try to run PE with control and CSC SPad module" in {
    test(new ProcessingElement(true)) { thePE =>
      val theTopIO = thePE.io
      val theClock = thePE.clock
      println("----------------- test begin -----------------")
      println("----------- Processing Element Module ------------")
      thePE.reset.poke(true.B)
      theClock.step(1)
      thePE.reset.poke(false.B)
      println("--------------- begin to write ---------------")
      theTopIO.topCtrl.doLoadEn.poke(true.B)
      iactAndWeightReadInFuc(inIactAddr, inIactDataCountDec, inWeightAddr, inWeightDataCountDec, theTopIO.dataStream, theClock, theTopIO.padWF)
      theTopIO.debugIO.peControlDebugIO.peState.expect(1.U, "wait it jump from load to cal")
      theTopIO.debugIO.peControlDebugIO.doMACEnDebug.expect(false.B, s"now it should be load")
      theClock.step(1)
      theTopIO.topCtrl.writeFinish.expect(true.B, s"after write in all data, write should finish")
      theTopIO.debugIO.peControlDebugIO.doMACEnDebug.expect(true.B, s"now it should be calculating state")
      theTopIO.debugIO.peControlDebugIO.peState.expect(2.U, "now it should be calculating state")
      theTopIO.topCtrl.doLoadEn.poke(false.B)
      theTopIO.topCtrl.pSumEnqOrProduct.bits.poke(false.B)
      theTopIO.topCtrl.pSumEnqOrProduct.valid.poke(true.B)
      theClock.step(1)
      for (i<- 0 until 133) {
        println(s"--------------- $i-th read cycle -----------")
        println(s"----- SPad State   =  ${theTopIO.debugIO.peSPadDebugIO.sPadState.peek()}")
        println(s"----- iactMatrix   = (${theTopIO.debugIO.peSPadDebugIO.iactMatrixData.peek()}, ${theTopIO.debugIO.peSPadDebugIO.iactMatrixRow.peek()}, ${theTopIO.debugIO.peSPadDebugIO.iactMatrixColumn.peek()})")
        println(s"----- IAddrIndex   =  ${theTopIO.debugIO.peSPadDebugIO.iactAddrIdx.peek()}, ${theTopIO.debugIO.peSPadDebugIO.iactAddrInc.peek()}")
        println(s"----- IDataInc     =  ${theTopIO.debugIO.peSPadDebugIO.iactDataInc.peek()}")
        println(s"----- weightMatrix = (${theTopIO.debugIO.peSPadDebugIO.weightMatrixData.peek()}, ${theTopIO.debugIO.peSPadDebugIO.weightMatrixRow.peek()}, ${theTopIO.debugIO.peSPadDebugIO.iactMatrixRow.peek()})")
        println(s"----- WAddrData    =  ${theTopIO.debugIO.peSPadDebugIO.weightAddrSPadReadOut.peek()}")
        println(s"----- WAInIndex    =  ${theTopIO.debugIO.peSPadDebugIO.weightAddrInIdx.peek()}")
        println(s"----- product      =  ${theTopIO.debugIO.peSPadDebugIO.productResult.peek()}")
        println(s"----- pSumResult   =  ${theTopIO.debugIO.peSPadDebugIO.pSumResult.peek()}")
        println(s"----- pSumLoad     =  ${theTopIO.debugIO.peSPadDebugIO.pSumLoad.peek()}")
        theClock.step(1)
      }
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
      theClock.step(1)
      theTopIO.dataStream.opsIO.ready.poke(true.B)
      theClock.step(fifoSize) // wait for fifo size clock cycle to let data flow out
      for (i <- 0 until M0*E*N0*F0) {
        println(s"--------- $i-th pSumSPad read cycle --------")
        theTopIO.debugIO.peControlDebugIO.peState.expect(1.U, s"the control state should be load when read out partial sun")
        theTopIO.debugIO.peSPadDebugIO.sPadState.expect(0.U, "the SPad state should be idle when read out partial sum")
        println(s"----- pSumReadOut  =  ${theTopIO.dataStream.opsIO.bits.peek()}")
        theTopIO.dataStream.opsIO.bits.expect(outpSum(i).U, s"the out partial sum should be ${outpSum(i)} at $i-th index")
        theClock.step(1)
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
      theClock.step(1)
      thePESPad.reset.poke(false.B)
      println("--------------- begin to write ---------------")
      theTopIO.padCtrl.fromTopIO.doLoadEn.poke(true.B)
      PEScratchPadWriteIn(inIactAddr, inIactDataCountDec, inWeightAddr, inWeightDataCountDec, thePESPad)
      theTopIO.padCtrl.fromTopIO.doLoadEn.poke(false.B)
      println("--------------- begin to read ----------------")
      theTopIO.padCtrl.fromTopIO.pSumEnqOrProduct.bits.poke(false.B)
      theTopIO.padCtrl.doMACEn.poke(true.B) // start the state machine
      theClock.step(1) // from idle to address SPad read
      theTopIO.padCtrl.doMACEn.poke(false.B) // end the state machine
      for (i<- 0 until 133) {
        println(s"--------------- $i-th read cycle -----------")
        println(s"----- SPad State   =  ${theTopIO.debugIO.sPadState.peek()}")
        println(s"----- iactMatrix   = (${theTopIO.debugIO.iactMatrixData.peek()}, ${theTopIO.debugIO.iactMatrixRow.peek()}, ${theTopIO.debugIO.iactMatrixColumn.peek()})")
        println(s"----- IAddrIndex   =  ${theTopIO.debugIO.iactAddrIdx.peek()}, ${theTopIO.debugIO.iactAddrInc.peek()}")
        println(s"----- IDataInc     =  ${theTopIO.debugIO.iactDataInc.peek()}")
        println(s"----- weightMatrix = (${theTopIO.debugIO.weightMatrixData.peek()}, ${theTopIO.debugIO.weightMatrixRow.peek()}, ${theTopIO.debugIO.iactMatrixRow.peek()})")
        println(s"----- WAddrData    =  ${theTopIO.debugIO.weightAddrSPadReadOut.peek()}")
        println(s"----- WAInIndex    =  ${theTopIO.debugIO.weightAddrInIdx.peek()}")
        println(s"----- product      =  ${theTopIO.debugIO.productResult.peek()}")
        println(s"----- pSumResult   =  ${theTopIO.debugIO.pSumResult.peek()}")
        println(s"----- pSumLoad     =  ${theTopIO.debugIO.pSumLoad.peek()}")
        theClock.step(1)
      }
      theClock.step(2)
      println("-------------- read partial sum ---------------")
      theTopIO.padCtrl.fromTopIO.doLoadEn.poke(true.B) // begin to read out partial sum
      theTopIO.dataStream.opsIO.ready.poke(true.B)
      theTopIO.debugIO.sPadState.expect(0.U, "the SPad state should be idle when read out partial sum")
      for (i <- 0 until M0*E*N0*F0) {
        println(s"--------- $i-th pSumSPad read cycle --------")
        println(s"----- pSumReadOut  =  ${theTopIO.dataStream.opsIO.bits.peek()}")
        theTopIO.dataStream.opsIO.bits.expect(outpSum(i).U, s"the out partial sum should be ${outpSum(i)} at $i-th index")
        //assertResult(outpSum(i).asUInt(psDataWidth.W))(theTopIO.dataStream.opsIO.bits.peek())
        theClock.step(1)
      }
    }
  }

  it should "try to read and write data in csc format" in {
    test(new SimplyCombineAddrDataSPad) { iactSPad =>
      val theTopIO = iactSPad.io
      val theClock = iactSPad.clock
      println("---------- begin to test the read ----------")
      println("------ and write address in Iact SPad ------")
      iactSPad.reset.poke(true.B)
      theClock.step(1)
      iactSPad.reset.poke(false.B)
      println("-------------- begin to write --------------")
      simplyWriteInDataAndAddr(inIactTestAddr, inWeightDataCountDec, iactSPad)
      println("-------------- begin to read ---------------")
      theTopIO.iactDataReq.poke(true.B) // start the state machine
      theClock.step(1) // from idle to address SPad read
      var j: Int = 0
      for (i <- outWeightCycleType.indices) {
        simplyCheckSignal(i, iactSPad, outWeightCycleType, j, inWeightData, inWeightCount, outWeightColumn)
        if (outWeightCycleType(i) != 0) {
          j = j + 1
        }
      }
    }
  }

  it should "write and read data in csc format with continued zero columns" in {
    test(new SimplyCombineAddrDataSPad) { iactSPad =>
      val theTopIO = iactSPad.io
      val theClock = iactSPad.clock
      println("---------- begin to test the read ----------")
      println("------ and write address in Iact SPad ------")
      println("-------- with continued zero columns -------")
      iactSPad.reset.poke(true.B)
      theClock.step(1)
      iactSPad.reset.poke(false.B)
      println("------------- begin to write ---------------")
      simplyWriteInDataAndAddr(inIactTestAddr2, inWeightDataCountDec, iactSPad)
      println("------------- begin to read ----------------")
      theTopIO.iactDataReq.poke(true.B) // start the state machine
      theClock.step(1) // from idle to address SPad read
      var j: Int = 0
      for (i <- outWeightCycleType2.indices) {
        simplyCheckSignal(i, iactSPad, outWeightCycleType2, j, inWeightData, inWeightCount, outWeightColumn2)
        if (outWeightCycleType2(i) != 0) {
          j = j + 1
        }
      }
    }
  }

  it should "basically write and read address in Iact SPad with CSC format data" in {
    test(new SPadAddrModule( 9, 4)) { addrSPad =>
      val theDataPath = addrSPad.io.dataPath
      val theCtrlPath = addrSPad.io.ctrlPath
      val theDataIO = theDataPath.writeInData.data
      val theClock = addrSPad.clock
      println("--- begin to test the read and write address in Iact SPad ---")
      println("----------- begin to write -----------")
      theDataIO.valid.poke(true.B)
      theCtrlPath.writeEn.poke(true.B)
      theCtrlPath.readEn.poke(false.B)
      for (i <- inIactTestAddr.indices) {
        theDataIO.bits.poke(inIactTestAddr(i).U)
        theCtrlPath.writeIdx.expect(i.U, s"i = $i")
        theCtrlPath.writeFin.expect((i == inIactTestAddr.length - 1).B, s"i = $i")
        theDataIO.ready.expect(true.B, "write valid, after receive the data, it should be ready")
        theClock.step(1)
      }
      println("----------- begin to read -----------")
      theDataIO.valid.poke(false.B)
      theCtrlPath.writeEn.poke(false.B)
      theCtrlPath.readEn.poke(true.B)
      theCtrlPath.indexInc.poke(true.B) // INCREASE ALL THE TIME
      for (i <- 0 until (inIactTestAddr.length - 1)) {
        println(s"----------- read clock $i -----------")
        theDataPath.columnNum.expect(i.U, s"columnNum = $i")
        theDataPath.readOutData.expect(inIactTestAddr(i).U, s"readOutData = inWeightData($i) = ${inIactTestAddr(i)}")
        println(s"theCtrlPath.columnNum = $i")
        println(s"theCtrlPath.readOutData = ${inIactTestAddr(i)}")
        theClock.step(1)
      }
      theDataPath.readOutData.expect(inIactTestAddr.last.U, s"readOutData = inWeightData(${inIactTestAddr.length - 1}) = ${inIactTestAddr.last}")
    }
  }

  it should "basically write and read data in Iact SPad" in {
    test(new SPadDataModule( 16, 12, false)) { dataSPad =>
      val theDataPath = dataSPad.io.dataPath
      val theCtrlPath = dataSPad.io.ctrlPath
      val theDataIO = theDataPath.writeInData.data
      val theClock = dataSPad.clock
      println("--- begin to test the read and write data in Iact SPad ---")
      println("----------- begin to write -----------")
      theDataIO.valid.poke(true.B)
      theCtrlPath.writeEn.poke(true.B)
      theCtrlPath.readEn.poke(false.B)
      for (i <- inWeightDataCountDec.indices) {
        theDataIO.bits.poke(inWeightDataCountDec(i).U)
        theDataIO.ready.expect(true.B, "write valid, after receive the data, it should be ready")
        theCtrlPath.writeFin.expect((i == inWeightDataCountDec.length - 1).B, s"i = $i")
        theCtrlPath.writeIdx.expect(i.U, s"i = $i")
        theClock.step(1)
      }
      println("----------- begin to read -----------")
      theDataIO.valid.poke(false.B)
      theCtrlPath.writeEn.poke(false.B)
      theCtrlPath.readEn.poke(true.B)
      theCtrlPath.indexInc.poke(true.B) // INCREASE ALL THE TIME
      for (i <- inWeightDataCountDec.indices) {
        println(s"----------- read clock $i -----------")
        theDataPath.columnNum.expect(i.U, s"columnNum = $i in read clock $i")
        theDataPath.readOutData.expect(inWeightDataCountDec(i).U, s"readOutData = inWeightDataCountDec($i) = ${inWeightDataCountDec(i)}")
        println(s"theCtrlPath.columnNum = $i")
        println(s"theCtrlPath.readOutData = ${inWeightDataCountDec(i)}")
        theClock.step(1)
      }
      theDataPath.columnNum.expect(0.U, s"new read turn begins, columnNum = 0")
      theDataPath.readOutData.expect(inWeightDataCountDec.head.U, s"new read turn begins, readOutData = ${inWeightDataCountDec.head}")
    }
  }
}
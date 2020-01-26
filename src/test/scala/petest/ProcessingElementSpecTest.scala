package dla.test.petest

import chisel3._
import chisel3.tester._
import dla.pe.{ProcessingElement, ProcessingElementPad, SPadAddrModule, SPadDataModule, MCRENFConfig}
import org.scalatest._

class ProcessingElementSpecTest extends FlatSpec with ChiselScalatestTester with Matchers with MCRENFConfig {
  // def some common parameters and functions
  val inIactTestAddr = Seq(2, 5, iactZeroColumnCode, 6, 7, iactZeroColumnCode, 9, 12) // 15 means it is a zero column, don't record the first number
  val inIactTestAddr2 = Seq(2, 5, 7, 8, iactZeroColumnCode, iactZeroColumnCode, iactZeroColumnCode, iactZeroColumnCode, 9)
  val inWeightAddr = Seq(2, 5, weightZeroColumnCode, 6, 7, weightZeroColumnCode, 9, 12) // 15 means it is a zero column, don't record the first number
  val inWeightData = Seq(1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33) // zero column between 15 & 18, 24 & 27
  val inWeightCount = Seq(1, 2, 0, 1, 3, 2, 3, 1, 3, 0, 1, 2)
  val inIactAddr = Seq(5, 9, iactZeroColumnCode, 11, 14)
  val inIactData = Seq(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32)
  val inIactCount = Seq(1, 3, 4, 6, 7, 2, 5, 6, 7, 0, 5, 0, 1, 3, 5, 7)
  val outpSum = Seq(282, 318, 330, 132, 486, 834, 774, 336, 0, 482, 60, 528, 0, 0, 0, 0, 156, 258, 72, 312, 864, 1590, 1056, 720)
  val outWeightColumn = Seq(0, 0, 1, 1, 1, 2, 4, 5, 5, 7, 7, 7) // 3, 6 are zero column
  val outWeightColumn2 = Seq(0, 0, 1, 1, 1, 2, 2, 3, 4, 9, 9, 9)
  val outIactColumn = Seq(0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 4, 4, 4, 5, 5)
  val outWeightCycleType = Seq(2, 1, 2, 1, 1, 0, 2, 2, 0, 2, 1, 2, 1, 1) // 0 for zero column, 1 for data read only read cycle,
                    // 2 for read cycle which contains both address and data read
  val outWeightCycleType2 = Seq(2, 1, 2, 1, 1, 2, 1, 2, 0, 0, 0, 0, 2, 2, 1, 1) // read the zero numbers after the column then read data
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
  * |  27  |  0  |  7  |          |  27  |  0  |  9  |
  * |  30  |  1  |  7  |          |  30  |  1  |  9  |
  * |  33  |  2  |  7  |          |  33  |  2  |  9  |
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
  def PEScratchPadWriteIn(inIactAddr: Seq[Int], inIactData: Seq[Int], inIactTestAddr: Seq[Int], inWeightData: Seq[Int], topModule: ProcessingElementPad): Any = {
    val theTopSPadIO = topModule.io.dataStream
    val theClock = topModule.clock
    theTopSPadIO.iactIOs.addrIOs.streamLen.poke(inIactAddr.length.U)
    theTopSPadIO.iactIOs.dataIOs.streamLen.poke(inIactData.length.U)
    theTopSPadIO.weightIOs.addrIOs.streamLen.poke(inIactTestAddr.length.U)
    theTopSPadIO.weightIOs.dataIOs.streamLen.poke(inWeightData.length.U)
    topModule.io.padCtrl.pSumEnqOrProduct.bits.poke(false.B)
    topModule.io.padCtrl.doMACEn.poke(false.B)
    fork {
      theTopSPadIO.iactIOs.addrIOs.writeInDataIO.valid.poke(true.B)
      for (i <- inIactAddr.indices) {
        theTopSPadIO.iactIOs.addrIOs.writeInDataIO.bits.data.poke(inIactAddr(i).U)
        theTopSPadIO.iactIOs.addrIOs.writeFin.expect((i == inIactAddr.length - 1).B, s"[write cycle $i @ iactAddrSPad] should it finish writing?")
        theTopSPadIO.iactIOs.addrIOs.writeInDataIO.ready.expect(true.B, s"[write cycle $i @ iactAddrSPad] write valid, after receive the data, it should be ready")
        theClock.step(1)
      }
      theTopSPadIO.iactIOs.addrIOs.writeInDataIO.valid.poke(false.B)
    } .fork {
      theTopSPadIO.iactIOs.dataIOs.writeInDataIO.valid.poke(true.B)
      for (i <- inIactData.indices) {
        theTopSPadIO.iactIOs.dataIOs.writeInDataIO.bits.data.poke(inIactData(i).U)
        theTopSPadIO.iactIOs.dataIOs.writeFin.expect((i == inIactData.length - 1).B, s"[write cycle $i @ iactDataSPad] should it finish writing?")
        theTopSPadIO.iactIOs.dataIOs.writeInDataIO.ready.expect(true.B, s"[write cycle $i @ iactDataSPad] write valid, after receive the data, it should be ready")
        theClock.step(1)
      }
      theTopSPadIO.iactIOs.dataIOs.writeInDataIO.valid.poke(false.B)
    } .fork {
      theTopSPadIO.weightIOs.addrIOs.writeInDataIO.valid.poke(true.B)
      for (i <- inIactTestAddr.indices) {
        theTopSPadIO.weightIOs.addrIOs.writeInDataIO.bits.data.poke(inIactTestAddr(i).U)
        theTopSPadIO.weightIOs.addrIOs.writeFin.expect((i == inIactTestAddr.length - 1).B, s"[write cycle $i @ weightAddrSPad] should it finish writing?")
        theTopSPadIO.weightIOs.addrIOs.writeInDataIO.ready.expect(true.B, s"[write cycle $i @ weightAddrSPad] write valid, after receive the data, it should be ready")
        theClock.step(1)
      }
      theTopSPadIO.weightIOs.addrIOs.writeInDataIO.valid.poke(false.B)
    } .fork {
      theTopSPadIO.weightIOs.dataIOs.writeInDataIO.valid.poke(true.B)
      for (i <- inWeightData.indices) {
        theTopSPadIO.weightIOs.dataIOs.writeInDataIO.bits.data.poke(inWeightData(i).U)
        theTopSPadIO.weightIOs.dataIOs.writeFin.expect((i == inWeightData.length - 1).B, s"[write cycle $i @ weightDataSPad] should it finish writing?")
        theTopSPadIO.weightIOs.dataIOs.writeInDataIO.ready.expect(true.B, s"[write cycle $i @ weightDataSPad] write valid, after receive the data, it should be ready")
        theClock.step(1)
      }
      theTopSPadIO.weightIOs.dataIOs.writeInDataIO.valid.poke(false.B)
    } .join()
  }

  def simplyWriteInDataAndAddr(inIactTestAddr: Seq[Int], inWeightData: Seq[Int], topModule: SimplyCombineAddrDataSPad): Any = {
    val theTopIO = topModule.io.iactIOs
    val theDataWriteIdx = topModule.io.iactDataWriteIdx
    val theAddrWriteIdx = topModule.io.iactAddrWriteIdx
    val theDataReq = topModule.io.iactDataReq
    val theClock = topModule.clock
    theTopIO.addrIOs.streamLen.poke(inIactTestAddr.length.U)
    theTopIO.dataIOs.streamLen.poke(inWeightData.length.U)
    theDataReq.poke(false.B)
    topModule.io.writeEn.poke(true.B)
    fork { // run them in parallel
      theTopIO.addrIOs.writeInDataIO.valid.poke(true.B)
      for (i <- inIactTestAddr.indices) {
        theTopIO.addrIOs.writeInDataIO.bits.data.poke(inIactTestAddr(i).U)
        theAddrWriteIdx.expect(i.U, s"i = $i")
        theTopIO.addrIOs.writeFin.expect((i == inIactTestAddr.length - 1).B, s"i = $i")
        theTopIO.addrIOs.writeInDataIO.ready.expect(true.B, "write valid, after receive the data, it should be ready")
        theClock.step(1)
      }
      theTopIO.addrIOs.writeInDataIO.valid.poke(false.B)
    } .fork {
      theTopIO.dataIOs.writeInDataIO.valid.poke(true.B)
      for (i <- inWeightData.indices) {
        theTopIO.dataIOs.writeInDataIO.bits.data.poke(inWeightData(i).U)
        theDataWriteIdx.expect(i.U, s"i = $i")
        theTopIO.dataIOs.writeFin.expect((i == inWeightData.length - 1).B, s"i = $i")
        theTopIO.dataIOs.writeInDataIO.ready.expect(true.B, "write valid, after receive the data, it should be ready")
        theClock.step(1)
      }
      theTopIO.dataIOs.writeInDataIO.valid.poke(false.B)
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
/*
  it should "try to run PE with control and CSC SPad module" in {
    test(new ProcessingElement) { thePE =>
      val theTopIO = thePE.io
      val theClock = thePE.clock
      println("----------------- test begin -----------------")
      println("----------- Processing Element Module ------------")
      println("--------------- begin to write ---------------")
      theTopIO.topCtrl.doLoadEn.poke(true.B)
    }
  }
*/
  it should "try to run PE SPad with CSC compressed data" in {
    test(new ProcessingElementPad(true)) { thePESPad =>
      val theTopIO = thePESPad.io
      val theClock = thePESPad.clock
      println("----------------- test begin -----------------")
      println("----------- PE Scratch Pad Module ------------")
      println("--------------- begin to write ---------------")
      theTopIO.padCtrl.doLoadEn.poke(true.B)
      PEScratchPadWriteIn(inIactAddr,inIactDataCountDec, inWeightAddr, inWeightDataCountDec, thePESPad)
      theTopIO.padCtrl.doLoadEn.poke(false.B)
      println("--------------- begin to read ----------------")
      theTopIO.padCtrl.pSumEnqOrProduct.bits.poke(false.B)
      theTopIO.padCtrl.doMACEn.poke(true.B) // start the state machine
      theClock.step(1) // from idle to address SPad read
      for (i<- 0 until 146) {
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
      theTopIO.padCtrl.doMACEn.poke(false.B) // start the state machine
      theClock.step(2)
      theTopIO.padCtrl.doLoadEn.poke(true.B) // begin to read out partial sum
      theTopIO.dataStream.opsIO.ready.poke(true.B)
      theTopIO.debugIO.sPadState.expect(0.U, "the SPad state should be idle when read out partial sum")
      for (i <- 0 until M0*E*N0*F0 - 1) {
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
    test(new SPadAddrModule(4, 9, 4)) { addrSPad =>
      val theCommonIO = addrSPad.io.commonIO
      val theDataIO = addrSPad.io.commonIO.dataLenFinIO.writeInDataIO
      val theClock = addrSPad.clock
      println("--- begin to test the read and write address in Iact SPad ---")
      println("----------- begin to write -----------")
      theCommonIO.dataLenFinIO.streamLen.poke(inIactTestAddr.length.U)
      theDataIO.valid.poke(true.B)
      theCommonIO.writeEn.poke(true.B)
      theCommonIO.readEn.poke(false.B)
      for (i <- inIactTestAddr.indices) {
        theDataIO.bits.data.poke(inIactTestAddr(i).U)
        theCommonIO.writeIdx.expect(i.U, s"i = $i")
        theCommonIO.dataLenFinIO.writeFin.expect((i == inIactTestAddr.length - 1).B, s"i = $i")
        theDataIO.ready.expect(true.B, "write valid, after receive the data, it should be ready")
        theClock.step(1)
      }
      println("----------- begin to read -----------")
      theDataIO.valid.poke(false.B)
      theCommonIO.writeEn.poke(false.B)
      theCommonIO.readEn.poke(true.B)
      addrSPad.io.addrIO.indexInc.poke(true.B) // INCREASE ALL THE TIME
      for (i <- 0 until (inIactTestAddr.length - 1)) {
        println(s"----------- read clock $i -----------")
        theCommonIO.columnNum.expect(i.U, s"columnNum = $i")
        theCommonIO.readOutData.expect(inIactTestAddr(i).U, s"readOutData = inWeightData($i) = ${inIactTestAddr(i)}")
        println(s"theCommonIO.columnNum = $i")
        println(s"theCommonIO.readOutData = ${inIactTestAddr(i)}")
        theClock.step(1)
      }
      theCommonIO.readOutData.expect(inIactTestAddr.last.U, s"readOutData = inWeightData(${inIactTestAddr.length - 1}) = ${inIactTestAddr.last}")
    }
  }

  it should "basically write and read data in Iact SPad" in {
    test(new SPadDataModule(4, 16, 12, false)) { dataSPad =>
      val theCommonIO = dataSPad.io.commonIO
      val theDataIO = dataSPad.io.commonIO.dataLenFinIO.writeInDataIO
      val theClock = dataSPad.clock
      println("--- begin to test the read and write data in Iact SPad ---")
      println("----------- begin to write -----------")
      theCommonIO.dataLenFinIO.streamLen.poke(inWeightDataCountDec.length.U)
      theDataIO.valid.poke(true.B)
      theCommonIO.writeEn.poke(true.B)
      theCommonIO.readEn.poke(false.B)
      for (i <- inWeightDataCountDec.indices) {
        theDataIO.bits.data.poke(inWeightDataCountDec(i).U)
        theDataIO.ready.expect(true.B, "write valid, after receive the data, it should be ready")
        theCommonIO.dataLenFinIO.writeFin.expect((i == inWeightDataCountDec.length - 1).B, s"i = $i")
        theCommonIO.writeIdx.expect(i.U, s"i = $i")
        theClock.step(1)
      }
      println("----------- begin to read -----------")
      theDataIO.valid.poke(false.B)
      theCommonIO.writeEn.poke(false.B)
      theCommonIO.readEn.poke(true.B)
      dataSPad.io.dataIO.indexInc.poke(true.B) // INCREASE ALL THE TIME
      for (i <- inWeightDataCountDec.indices) {
        println(s"----------- read clock $i -----------")
        theCommonIO.columnNum.expect(i.U, s"columnNum = $i in read clock $i")
        theCommonIO.readOutData.expect(inWeightDataCountDec(i).U, s"readOutData = inWeightDataCountDec($i) = ${inWeightDataCountDec(i)}")
        println(s"theCommonIO.columnNum = $i")
        println(s"theCommonIO.readOutData = ${inWeightDataCountDec(i)}")
        theClock.step(1)
      }
      theCommonIO.columnNum.expect(0.U, s"new read turn begins, columnNum = 0")
      theCommonIO.readOutData.expect(inWeightDataCountDec.head.U, s"new read turn begins, readOutData = ${inWeightDataCountDec.head}")
    }
  }
}
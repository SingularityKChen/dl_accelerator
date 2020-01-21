package dla.tests

import chisel3._
import chisel3.tester._
import dla.pe.{SPadAddrModule, SPadDataModule, SPadSizeConfig, SimplyCombineAddrDataSPad, ProcessingElementPad}
import org.scalatest._
class CompressedSparseColumnSpecTest extends FlatSpec with ChiselScalatestTester with Matchers with SPadSizeConfig {
  // def some common parameters and functions
  val inAddr = Seq(2, 5, iactZeroColumnCode, 6, 7, iactZeroColumnCode, 9, 12) // 15 means it is a zero column, don't record the first number
  val inAddr2 = Seq(2, iactZeroColumnCode, iactZeroColumnCode, iactZeroColumnCode, 5, iactZeroColumnCode, iactZeroColumnCode, 7, 8, iactZeroColumnCode, iactZeroColumnCode, iactZeroColumnCode, iactZeroColumnCode, 9) // FIXME: this will exceed the recommend Address SPad size
  val inData = Seq(1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33) // zero column between 15 & 18, 24 & 27
  val inCount = Seq(1, 2, 0, 1, 3, 2, 3, 1, 3, 0, 1, 2)
  val inWeightAddr = Seq(5, 9, weightZeroColumnCode, 11, 14)
  val inWeightData = Seq(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32)
  val inWeightCount = Seq(1, 3, 4, 6, 7, 2, 5, 6, 7, 0, 5, 0, 1, 3, 5, 7)
  val outColumn = Seq(0, 0, 1, 1, 1, 2, 4, 5, 5, 7, 7, 7) // 3, 6 are zero column
  val outColumn2 = Seq(0, 0, 1, 1, 1, 5, 5, 8, 9, 14, 14, 14)
  val outDataReadIndex = Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
  val outCycleType = Seq(2, 1, 2, 1, 1, 0, 2, 2, 0, 2, 1, 2, 1, 1) // 0 for zero column, 1 for data read only read cycle,
                    // 2 for read cycle which contains both address and data read
  val outCycleType2 = Seq(2, 1, 0, 0, 0, 2, 1, 1, 0, 0, 2, 1, 2, 0, 0, 0, 0, 2, 2, 1, 1) // read the zero numbers after the column then read data
  /*
  * The first matrix is           The second matrix is
  * | data | row | col |          | data | row | col |
  * | ---- | --- | --- |          | ---- | --- | --- |
  * |   1  |  1  |  0  |          |   1  |  1  |  0  |
  * |   3  |  2  |  0  |          |   3  |  2  |  0  |
  * |   6  |  0  |  1  |          |   6  |  0  |  1  |
  * |   9  |  1  |  1  |          |   9  |  1  |  1  |
  * |  12  |  3  |  1  |          |  12  |  3  |  1  |
  * |  15  |  2  |  2  |          |  15  |  2  |  5  |
  * |  18  |  3  |  4  |          |  18  |  3  |  5  |
  * |  21  |  1  |  5  |          |  21  |  1  |  8  |
  * |  24  |  3  |  5  |          |  24  |  3  |  9  |
  * |  27  |  0  |  7  |          |  27  |  0  |  14 |
  * |  30  |  1  |  7  |          |  30  |  1  |  14 |
  * |  33  |  2  |  7  |          |  33  |  2  |  14 |
  */
  def toBinary(i: Int, digits: Int = 8): String =
    String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0')
  def combineDataAndCount(theData: Seq[Int], theCount: Seq[Int]): Seq[Int] = { // input data and count, and combine them together
    val theDataWithCount: Seq[(Int, Int)] = theData zip theCount
    val theDataCountBinary: Seq[String] = theDataWithCount.map{case (x: Int, y: Int) => toBinary(x) + toBinary(y, 4)}
    val theDataCountDec: Seq[Int] = theDataCountBinary.map(x => Integer.parseInt(x, 2))
    theDataCountDec
  }
  val inDataCountDec: Seq[Int] = combineDataAndCount(inData, inCount)
  def simplyWriteInDataAndAddr(inAddr: Seq[Int], inData: Seq[Int], topModule: SimplyCombineAddrDataSPad): Any = {
    val theTopIO = topModule.io.iactIOs
    val theDataWriteIdx = topModule.io.iactDataWriteIdx
    val theAddrWriteIdx = topModule.io.iactAddrWriteIdx
    val theDataReq = topModule.io.iactDataReq
    val theClock = topModule.clock
    theTopIO.addrIOs.streamLen.poke(inAddr.length.U)
    theTopIO.dataIOs.streamLen.poke(inDataCountDec.length.U)
    theDataReq.poke(false.B)
    fork { // run them in parallel
      theTopIO.addrIOs.writeInDataIO.valid.poke(true.B)
      for (i <- inAddr.indices) {
        theTopIO.addrIOs.writeInDataIO.bits.data.poke(inAddr(i).U)
        theAddrWriteIdx.expect(i.U, s"i = $i")
        theTopIO.addrIOs.writeFin.expect((i == inAddr.length - 1).B, s"i = $i")
        theTopIO.addrIOs.writeInDataIO.ready.expect(true.B, "write valid, after receive the data, it should be ready")
        theClock.step(1)
      }
      theTopIO.addrIOs.writeInDataIO.valid.poke(false.B)
    } .fork {
      theTopIO.dataIOs.writeInDataIO.valid.poke(true.B)
      for (i <- inDataCountDec.indices) {
        theTopIO.dataIOs.writeInDataIO.bits.data.poke(inDataCountDec(i).U)
        theDataWriteIdx.expect(i.U, s"i = $i")
        theTopIO.dataIOs.writeFin.expect((i == inDataCountDec.length - 1).B, s"i = $i")
        theTopIO.dataIOs.writeInDataIO.ready.expect(true.B, "write valid, after receive the data, it should be ready")
        theClock.step(1)
      }
      theTopIO.dataIOs.writeInDataIO.valid.poke(false.B)
    }.join()
  }

  def simplyCheckSignal(cycle: Int, topModule: SimplyCombineAddrDataSPad, outCycleType: Seq[Int], readDataTimes: Int, readInData: Seq[Int], readInRow: Seq[Int], readInColumn: Seq[Int]): Any = outCycleType(cycle) match {
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
  behavior of "read and write the compressed sparse column format data"

  it should "try to run PE SPad with CSC compressed data" in {
    test(new ProcessingElementPad) { thePESPad =>
      val theTopIO = thePESPad.io
      val theClock = thePESPad.clock
      println("begin to test the PE Scratch Pad Module")
      println("begin to write")
    }
  }

  it should "try to read and write data in csc format" in {
    test(new SimplyCombineAddrDataSPad) { iactSPad =>
      val theTopIO = iactSPad.io
      val theClock = iactSPad.clock
      println("---------- begin to test the read ----------")
      println("------ and write address in Iact SPad ------")
      println("--------------- begin to write -------------")
      simplyWriteInDataAndAddr(inAddr, inData, iactSPad)
      println("------------- begin to read ----------------")
      theTopIO.iactDataReq.poke(true.B) // start the state machine
      theClock.step(1) // from idle to address SPad read
      var j: Int = 0
      for (i <- outCycleType.indices) {
        simplyCheckSignal(i, iactSPad, outCycleType, j, inData, inCount, outColumn)
        if (outCycleType(i) != 0) {
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
      simplyWriteInDataAndAddr(inAddr2, inData, iactSPad)
      println("------------- begin to read ----------------")
      theTopIO.iactDataReq.poke(true.B) // start the state machine
      theClock.step(1) // from idle to address SPad read
      var j: Int = 0
      for (i <- outCycleType2.indices) {
        simplyCheckSignal(i, iactSPad, outCycleType2, j, inData, inCount, outColumn2)
        if (outCycleType2(i) != 0) {
          j = j + 1
        }
      }
    }
  }

  it should "basically write and read address in Iact SPad" in {
    test(new SPadAddrModule(4, 9, 4)) { addrSPad =>
      val theCommonIO = addrSPad.io.commonIO
      val theDataIO = addrSPad.io.commonIO.dataLenFinIO.writeInDataIO
      val theClock = addrSPad.clock
      println("--- begin to test the read and write address in Iact SPad ---")
      println("----------- begin to write -----------")
      theCommonIO.dataLenFinIO.streamLen.poke(inAddr.length.U)
      theDataIO.valid.poke(true.B)
      theCommonIO.readEn.poke(false.B)
      for (i <- inAddr.indices) {
        theDataIO.bits.data.poke(inAddr(i).U)
        theCommonIO.writeIdx.expect(i.U, s"i = $i")
        theCommonIO.dataLenFinIO.writeFin.expect((i == inAddr.length - 1).B, s"i = $i")
        theDataIO.ready.expect(true.B, "write valid, after receive the data, it should be ready")
        theClock.step(1)
      }
      println("----------- begin to read -----------")
      theDataIO.valid.poke(false.B)
      theCommonIO.readEn.poke(true.B)
      addrSPad.io.addrIO.indexInc.poke(true.B) // INCREASE ALL THE TIME
      for (i <- 0 until (inAddr.length - 1)) {
        println(s"----------- read clock $i -----------")
        theCommonIO.columnNum.expect(i.U, s"columnNum = $i")
        theCommonIO.readOutData.expect(inAddr(i).U, s"readOutData = inData($i) = ${inAddr(i)}")
        println(s"theCommonIO.columnNum = $i")
        println(s"theCommonIO.readOutData = ${inAddr(i)}")
        theClock.step(1)
      }
      theCommonIO.readOutData.expect(inAddr.last.U, s"readOutData = inData(${inAddr.length - 1}) = ${inAddr.last}")
    }
  }
  it should "basically write and read data in Iact SPad" in {
    test(new SPadDataModule(4, 16, 12, false)) { dataSPad =>
      val theCommonIO = dataSPad.io.commonIO
      val theDataIO = dataSPad.io.commonIO.dataLenFinIO.writeInDataIO
      val theClock = dataSPad.clock
      println("--- begin to test the read and write data in Iact SPad ---")
      println("----------- begin to write -----------")
      theCommonIO.dataLenFinIO.streamLen.poke(inDataCountDec.length.U)
      theDataIO.valid.poke(true.B)
      theCommonIO.readEn.poke(false.B)
      for (i <- inDataCountDec.indices) {
        theDataIO.bits.data.poke(inDataCountDec(i).U)
        theDataIO.ready.expect(true.B, "write valid, after receive the data, it should be ready")
        theCommonIO.dataLenFinIO.writeFin.expect((i == inDataCountDec.length - 1).B, s"i = $i")
        theCommonIO.writeIdx.expect(i.U, s"i = $i")
        theClock.step(1)
      }
      println("----------- begin to read -----------")
      theDataIO.valid.poke(false.B)
      theCommonIO.readEn.poke(true.B)
      dataSPad.io.dataIO.indexInc.poke(true.B) // INCREASE ALL THE TIME
      for (i <- inDataCountDec.indices) {
        println(s"----------- read clock $i -----------")
        theCommonIO.columnNum.expect(i.U, s"columnNum = $i in read clock $i")
        theCommonIO.readOutData.expect(inDataCountDec(i).U, s"readOutData = inDataCountDec($i) = ${inDataCountDec(i)}")
        println(s"theCommonIO.columnNum = $i")
        println(s"theCommonIO.readOutData = ${inDataCountDec(i)}")
        theClock.step(1)
      }
      theCommonIO.columnNum.expect(0.U, s"new read turn begins, columnNum = 0")
      theCommonIO.readOutData.expect(inDataCountDec.head.U, s"new read turn begins, readOutData = ${inDataCountDec.head}")
    }
  }
}
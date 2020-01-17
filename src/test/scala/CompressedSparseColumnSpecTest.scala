package dla.tests

import chisel3._
import chisel3.tester._
import chisel3.util._
import dla.pe.{DataAddrStreanIO, IactSPadAddrModule, IactSPadDataModule, SPadSizeConfig}
import org.scalatest._
class CompressedSparseColumnSpecTest extends FlatSpec with ChiselScalatestTester with Matchers {
  // def some common parameters and functions
  val inAddr = Seq(0, 2, 5, 6, 6, 7, 9, 9, 12)
  val inData = Seq(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)
  val inCount = Seq(1, 2, 0, 1, 3, 2, 3, 1, 3, 0, 1, 2)
  val inDataWithIndex: Seq[(Int, Int)] = inData zip inCount
  def toBinary(i: Int, digits: Int = 8): String =
    String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0')
  val inDataCountBinary: Seq[String] = inDataWithIndex.map{case (x: Int, y: Int) => toBinary(x, 8) + toBinary(y, 4)}
  val inDataCountDec: Seq[Int] = inDataCountBinary.map(x => Integer.parseInt(x, 2))

  behavior of "read and write the compressed sparse column format data"
  class SimplyCombineAddrDataSPad extends Module with SPadSizeConfig{
    val io = IO(new Bundle{
      val iactIOs = new DataAddrStreanIO(iactDataWidth, iactAddrWidth, commonLenWidth, commonLenWidth)
      val iactAddrWriteIdx: UInt = Output(UInt(commonLenWidth.W)) // use for test
      val iactDataReadEn: Bool = Input(Bool()) // control to read data vector
      // we are supposed to see address SPad and data SPad together
      val iactMatrixColumn: UInt = Output(UInt(commonLenWidth.W))
      val iactMatrixRow: UInt = Output(UInt(cscCountWidth.W))
      val iactMatrixData: UInt = Output(UInt(cscDataWidth.W))
    })
    val iactAddrSPad: IactSPadAddrModule = Module(new IactSPadAddrModule(commonLenWidth, iactAddrSPadSize, iactAddrWidth))
    val iactDataSPad: IactSPadDataModule = Module(new IactSPadDataModule(commonLenWidth, iactDataSPadSize, iactDataWidth))

    val iactAddrIndexWire: UInt = Wire(UInt(commonLenWidth.W))
    val iactAddrDataWire: UInt = Wire(UInt(iactAddrWidth.W))
    val iactAddrDataNextWire: UInt = Wire(UInt(iactAddrWidth.W))
    val iactDataIndexWire: UInt = Wire(UInt(commonLenWidth.W)) // use for address vector readEn
    val iactSPadZeroColumnReg: Bool = RegInit(false.B) // true, it is a zero column, then need read again
    val iactAddrSPadReadEnReg: Bool = RegInit(false.B)
    val iactDataSPadReadEnReg: Bool = RegInit(false.B)

    iactAddrSPad.io.commonIO.dataLenFinIO <> io.iactIOs.addrIOs // this is different from the real module
    iactDataSPad.io.commonIO.dataLenFinIO <> io.iactIOs.dataIOs
    io.iactAddrWriteIdx := iactAddrSPad.io.commonIO.writeIdx
    // AddrSPad
    io.iactMatrixColumn := iactAddrIndexWire
    iactAddrIndexWire := iactAddrSPad.io.commonIO.columnNum
    iactAddrDataWire := iactAddrSPad.io.commonIO.readOutData
    iactAddrDataNextWire := iactAddrSPad.io.addrIO.nextReadOutData
    iactAddrSPad.io.commonIO.readEn := iactAddrSPadReadEnReg
    iactAddrSPad.io.dataIO.readInIdx := 0.U
    iactAddrSPad.io.addrIO.readInIdx := 0.U
    // DataSPad
    iactDataIndexWire := iactDataSPad.io.commonIO.columnNum
    val iactDataCountVec: Seq[Bool] = iactDataSPad.io.commonIO.readOutData.toBools
    io.iactMatrixData := Cat(iactDataCountVec.take(8)).asUInt
    io.iactMatrixRow := Cat(iactDataCountVec.takeRight(4)).asUInt
    iactDataSPad.io.commonIO.readEn := io.iactDataReadEn
    iactDataSPad.io.commonIO.readEn := iactDataSPadReadEnReg
    iactDataSPad.io.dataIO.readInIdx := 0.U
    iactDataSPad.io.addrIO.readInIdx := 0.U
    val padIdle :: padIactAddr :: padIactData :: Nil = Enum(3)
    val sPad: UInt = RegInit(padIdle)
    // the_index_m0 = m0 + count_m0
    // addr_m0_index*M0
    // SPad read state machine
    switch (sPad) {
      is(padIdle) {
        when(io.iactDataReadEn) {
          sPad := padIactAddr
          when (iactAddrIndexWire === 0.U) { // then it is the beginning of this read
            iactSPadZeroColumnReg := false.B
            iactAddrSPadReadEnReg := true.B
          }.otherwise{
            iactSPadZeroColumnReg := iactAddrDataWire === iactAddrDataNextWire
            iactAddrSPadReadEnReg := iactAddrDataNextWire === iactDataIndexWire
          }
        }
      }
      is(padIactAddr) {
        // state transform
        when(iactSPadZeroColumnReg) {
          sPad := padIactAddr
        }.otherwise {
          sPad := padIactData
          iactDataSPadReadEnReg := true.B
        }
      }
      is(padIactData) {
        sPad := padIdle
      }
    }
  }

  it should "try to read and write the data with csc format" in {
    test(new SimplyCombineAddrDataSPad) { iactSPad =>
      val theTopIO = iactSPad.io
      val theClock = iactSPad.clock
      println("------ begin to test the read ------")
      println("------ and write address in Iact SPad ------")
      println("----------- begin to write -----------")
      theTopIO.iactIOs.addrIOs.streamLen.poke(inAddr.length.U)
      theTopIO.iactIOs.dataIOs.streamLen.poke(inDataCountDec.length.U)
      theTopIO.iactIOs.addrIOs.streamDecoupledDataIO.valid.poke(true.B)
      theTopIO.iactIOs.dataIOs.streamDecoupledDataIO.valid.poke(true.B)
      theTopIO.iactDataReadEn.poke(false.B)
      fork { // run them in parallel
        for (i <- inAddr.indices) {
          theTopIO.iactIOs.addrIOs.streamDecoupledDataIO.bits.data.poke(inAddr(i).U)
          theTopIO.iactAddrWriteIdx.expect(i.U, s"i = $i")
          theTopIO.iactIOs.addrIOs.writeFin.expect((i == inAddr.length - 1).B, s"i = $i")
          theTopIO.iactIOs.addrIOs.streamDecoupledDataIO.ready.expect(true.B, "write valid, after receive the data, it should be ready")
          theClock.step(1)
        }
      } .fork {
        for (i <- inDataCountDec.indices) {
          theTopIO.iactIOs.dataIOs.streamDecoupledDataIO.bits.data.poke(inDataCountDec(i).U)
          theTopIO.iactIOs.dataIOs.writeFin.expect((i == inDataCountDec.length - 1).B, s"i = $i")
          theTopIO.iactIOs.dataIOs.streamDecoupledDataIO.ready.expect(true.B, "write valid, after receive the data, it should be ready")
          theClock.step(1)
        }
      }.join()
      println("----------- begin to read -----------")
      theTopIO.iactIOs.addrIOs.streamDecoupledDataIO.valid.poke(false.B)
      theTopIO.iactIOs.dataIOs.streamDecoupledDataIO.valid.poke(false.B)
      theTopIO.iactDataReadEn.poke(true.B) // start the state machine
      println("------ read cycle one ---------")
      theClock.step(1) // from idle to address SPad read
      theTopIO.iactMatrixColumn.expect(0.U, "the column number should be zero at the beginning")
      theClock.step(1) // from address SPad read to data SPad read
      theTopIO.iactMatrixRow.expect(inCount.head.U, s"the row number should be ${inCount.head}")
      theTopIO.iactMatrixData.expect(inData.head.U, s"the data value should be ${inData.head}")
      println(s"data = ${inData.head}, row = ${inCount.head}, column = 0")
      println("------ read cycle two ---------")
      theClock.step(2) // from data SPad read to idle, then to address SPad read
      theTopIO.iactMatrixColumn.expect(1.U, "the column number should be 1 at the second read cycle")
      theClock.step(2) // from address SPad read to data SPad read
      theTopIO.iactMatrixRow.expect(inCount(1).U, s"the row number should be ${inCount(1)}")
      theTopIO.iactMatrixData.expect(inData(1).U, s"the data value should be ${inData(1)}")
      println(s"data = ${inData(1)}, row = ${inCount(1)}, column = 1")
    }
  }

  it should "basically write and read address in Iact SPad" in {
    test(new IactSPadAddrModule(4, 9, 4)) { addrSPad =>
      val theCommonIO = addrSPad.io.commonIO
      val theAddrIO = addrSPad.io.addrIO
      val theDataIO = addrSPad.io.commonIO.dataLenFinIO.streamDecoupledDataIO
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
      for (i <- 0 until (inAddr.length - 1)) {
        println(s"----------- read clock $i -----------")
        theCommonIO.columnNum.expect(i.U, s"columnNum = $i")
        theCommonIO.readOutData.expect(inAddr(i).U, s"readOutData = inData($i) = ${inAddr(i)}")
        theAddrIO.nextReadOutData.expect(inAddr(i + 1).U, s"nextReadOutData = inData(${i + 1}) = ${inAddr(i + 1)}")
        println(s"theCommonIO.columnNum = $i")
        println(s"theCommonIO.readOutData = ${inAddr(i)}")
        println(s"theAddrIO.nextReadOutData = ${inAddr(i + 1)}")
        theClock.step(1)
      }
      theCommonIO.readOutData.expect(inAddr.last.U, s"readOutData = inData(${inAddr.length - 1}) = ${inAddr.last}")
      theAddrIO.nextReadOutData.expect(0.U, s"nextReadOutData = inData(${inAddr.length}) exceed index, so nexRead = 0.U")
    }
  }
  it should "basically write and read data in Iact SPad" in {
    test(new IactSPadDataModule(4, 16, 12)) { dataSPad =>
      val theCommonIO = dataSPad.io.commonIO
      val theDataIO = dataSPad.io.commonIO.dataLenFinIO.streamDecoupledDataIO
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
      for (i <- inDataCountDec.indices) {
        println(s"----------- read clock $i -----------")
        theCommonIO.columnNum.expect(i.U, s"columnNum = $i")
        theCommonIO.readOutData.expect(inDataCountDec(i).U, s"readOutData = inData($i) = ${inData(i)}")
        println(s"theCommonIO.columnNum = $i")
        println(s"theCommonIO.readOutData = ${inDataCountDec(i)}")
        theClock.step(1)
      }
      theCommonIO.columnNum.expect(0.U, s"new read turn begins, columnNum = 0")
      theCommonIO.readOutData.expect(inDataCountDec.head.U, s"new read turn begins, readOutData = ${inDataCountDec.head}")
    }
  }
}
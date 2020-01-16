package dla.tests

import chisel3._
import chisel3.tester._
import dla.pe.{DataAddrStreanIO, IactSPadAddrModule, IactSPadDataModule, SPadSizeConfig}
import org.scalatest._
class CompressedSparseColumnSpecTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "read the compressed sparse column format data"
  class SimplyCombineAddrDataSPad extends Module with SPadSizeConfig{
    val io = IO(new Bundle{
      val iactIOs = new DataAddrStreanIO(iactDataWidth, iactAddrWidth, commonLenWidth, commonLenWidth)
      val iactAddrWriteIdx: UInt = Output(UInt(commonLenWidth.W))
    })
    val iactAddrSPad: IactSPadAddrModule = Module(new IactSPadAddrModule(commonLenWidth, iactAddrSPadSize, iactAddrWidth))
    val iactDataSPad: IactSPadDataModule = Module(new IactSPadDataModule(commonLenWidth, iactDataSPadSize, iactDataWidth))
    iactAddrSPad.io.commonIO.dataLenFinIO <> io.iactIOs.addrIOs // this is different from the real module
    iactDataSPad.io.commonIO.dataLenFinIO <> io.iactIOs.dataIOs
    io.iactAddrWriteIdx := iactAddrSPad.io.commonIO.writeIdx

    val iactAddrIndexWire: UInt = Wire(UInt(commonLenWidth.W))
    iactAddrIndexWire := iactAddrSPad.io.commonIO.columnNum
    val iactAddrDataWire: UInt = Wire(UInt(iactAddrWidth.W))
    iactAddrDataWire := iactAddrSPad.io.commonIO.readOutData
    val iactAddrDataNextWire: UInt = Wire(UInt(iactAddrWidth.W))
    iactAddrDataNextWire := iactDataSPad.io.addrIO.nextReadOutData
    val iactDataIndexWire: UInt = Wire(UInt(commonLenWidth.W)) // use for address vector readEn
    iactDataIndexWire := iactDataSPad.io.commonIO.columnNum
    val iactSPadZeroColumn: Bool = Wire(Bool()) // true, it is a zero column, then need read again
    when (iactAddrIndexWire === 0.U) { // then it is the beginning of this read
      iactSPadZeroColumn := false.B
      iactAddrSPad.io.commonIO.readEn := true.B
    }.otherwise{
      iactSPadZeroColumn := iactAddrDataWire === iactAddrDataNextWire
      iactAddrSPad.io.commonIO.readEn := iactAddrDataNextWire === iactDataIndexWire
    }

  }

  it should "try to read and write the data with csc format" in {
    test(new SimplyCombineAddrDataSPad) { iactSPad =>
      val theTopIO = iactSPad.io
      val theClock = iactSPad.clock
      val inAddr = Seq(0, 2, 5, 6, 6, 7, 9, 9, 12)
      val inData = Seq(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)
      println("--- begin to test the read and write address in Iact SPad ---")
      println("----------- begin to write -----------")
      theTopIO.iactIOs.addrIOs.streamLen.poke(inAddr.length.U)
      theTopIO.iactIOs.addrIOs.streamDecoupledDataIO.valid.poke(true.B)
      for (i <- inAddr.indices) {
        theTopIO.iactIOs.addrIOs.streamDecoupledDataIO.bits.data.poke(inAddr(i).U)
        theTopIO.iactAddrWriteIdx.expect(i.U, s"i = $i")
        theTopIO.iactIOs.addrIOs.writeFin.expect((i == inAddr.length - 1).B, s"i = $i")
        theTopIO.iactIOs.addrIOs.streamDecoupledDataIO.ready.expect(true.B, "write valid, after receive the data, it should be ready")
        theClock.step(1)
      }
    }
  }

  it should "basically write and read address in Iact SPad" in {
    test(new IactSPadAddrModule(4, 9, 4)) { addrSPad =>
      val theCommonIO = addrSPad.io.commonIO
      val theAddrIO = addrSPad.io.addrIO
      val theDataIO = addrSPad.io.commonIO.dataLenFinIO.streamDecoupledDataIO
      val theClock = addrSPad.clock
      val inAddr = Seq(0, 2, 5, 6, 6, 7, 9, 9, 12)
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
      val inData = Seq(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)
      val theCommonIO = dataSPad.io.commonIO
      val theDataIO = dataSPad.io.commonIO.dataLenFinIO.streamDecoupledDataIO
      val theClock = dataSPad.clock
      println("--- begin to test the read and write data in Iact SPad ---")
      println("----------- begin to write -----------")
      theCommonIO.dataLenFinIO.streamLen.poke(inData.length.U)
      theDataIO.valid.poke(true.B)
      theCommonIO.readEn.poke(false.B)
      for (i <- inData.indices) {
        theDataIO.bits.data.poke(inData(i).U)
        theDataIO.ready.expect(true.B, "write valid, after receive the data, it should be ready")
        theCommonIO.dataLenFinIO.writeFin.expect((i == inData.length - 1).B, s"i = $i")
        theCommonIO.writeIdx.expect(i.U, s"i = $i")
        theClock.step(1)
      }
      println("----------- begin to read -----------")
      theDataIO.valid.poke(false.B)
      theCommonIO.readEn.poke(true.B)
      for (i <- inData.indices) {
        println(s"----------- read clock $i -----------")
        theCommonIO.columnNum.expect(i.U, s"columnNum = $i")
        theCommonIO.readOutData.expect(inData(i).U, s"readOutData = inData($i) = ${inData(i)}")
        println(s"theCommonIO.columnNum = $i")
        println(s"theCommonIO.readOutData = ${inData(i)}")
        theClock.step(1)
      }
      theCommonIO.readOutData.expect(0.U, s"new read turn begins, readOutData = 0")
      theCommonIO.columnNum.expect(0.U, s"new read turn begins, columnNum = 0")
    }
  }
}
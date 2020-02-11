package dla.test.petest

import chisel3._
import chisel3.util._
import dla.pe.{CSCStreamIO, CSCWriteFinIO, SPadAddrModule, SPadDataModule, SPadSizeConfig}

class SimplyCombineAddrDataSPad extends Module with SPadSizeConfig{
  val io = IO(new Bundle{
    val iactIOs: CSCStreamIO = Flipped(new CSCStreamIO(iactDataWidth, iactAddrWidth))
    val iactWF = new CSCWriteFinIO
    val iactAddrWriteIdx: UInt = Output(UInt(iactAddrIdxWidth.W)) // use for test
    val iactDataReq: Bool = Input(Bool()) // control to read data vector
    // we are supposed to see address SPad and data SPad together
    val iactMatrixColumn: UInt = Output(UInt(iactAddrIdxWidth.W))
    val iactMatrixRow: UInt = Output(UInt(cscCountWidth.W))
    val iactMatrixData: UInt = Output(UInt(cscDataWidth.W))
    val iactMatrixDataBin: UInt = Output(UInt(iactDataWidth.W))
    val iactAddrReadEn: Bool = Output(Bool())
    val iactAddrReadData: UInt = Output(UInt(iactAddrWidth.W))
    // val iactAddrReadIndex: UInt = Output(UInt(commonLenWidth.W)) = iactMatrixColumn
    val iactDataReadIndex: UInt = Output(UInt(iactDataIdxWidth.W))
    val iactDataWriteIdx: UInt = Output(UInt(log2Ceil(iactDataSPadSize).W))
    val writeEn: Bool = Input(Bool())
  })
  val iactAddrSPad: SPadAddrModule = Module(new SPadAddrModule(iactAddrSPadSize, iactAddrWidth))
  val iactDataSPad: SPadDataModule = Module(new SPadDataModule(iactDataSPadSize, iactDataWidth, false))

  val padIdle :: padIactAddr :: padIactData :: Nil = Enum(3)
  val sPad: UInt = RegInit(padIdle)
  val iactAddrIndexWire: UInt = Wire(UInt(iactAddrIdxWidth.W))
  val iactAddrDataWire: UInt = Wire(UInt(iactAddrWidth.W))
  val iactDataIndexWire: UInt = Wire(UInt(iactDataIdxWidth.W)) // use for address vector readEn
  val iactSPadZeroColumnReg: Bool = RegInit(false.B) // true, it is a zero column, then need read again
  val iactAddrSPadReadEnReg: Bool = RegInit(false.B)
  val iactDataSPadReadEnReg: Bool = RegInit(false.B)
  val iactAddrSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of address SPad
  val iactDataSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of data SPad
  val iactMatrixColumnReg: UInt = RegInit(0.U(iactAddrIdxWidth.W))
  val iactZeroColumnNumber: UInt = RegInit(0.U(iactAddrIdxWidth.W)) // use for get the right column number
  val iactDataSPadFirstReadReg: Bool = RegInit(true.B)
  iactAddrSPadIdxIncWire := ((sPad === padIactData) && (iactAddrDataWire === (iactDataIndexWire + 1.U))) || ((sPad === padIactAddr) && (iactAddrDataWire === iactZeroColumnCode.U))
  iactDataSPadIdxIncWire := ((sPad === padIactAddr) && !iactSPadZeroColumnReg && !iactDataSPadFirstReadReg) || ((sPad === padIactData) && !iactAddrSPadIdxIncWire)// if first read, then keep the read index of zero
  // AddrSPad
  iactAddrSPad.io.dataPath.writeInData <> io.iactIOs.addrIOs
  iactAddrIndexWire := iactAddrSPad.io.dataPath.columnNum
  io.iactMatrixColumn := iactMatrixColumnReg // for debug
  iactAddrDataWire := iactAddrSPad.io.dataPath.readOutData
  io.iactAddrReadData := iactAddrDataWire // for debug
  io.iactAddrReadEn := iactAddrSPadReadEnReg
  iactAddrSPad.io.ctrlPath.writeEn := io.writeEn
  io.iactAddrWriteIdx := iactAddrSPad.io.ctrlPath.writeIdx
  io.iactWF.addrWriteFin := iactAddrSPad.io.ctrlPath.writeFin
  iactAddrSPad.io.ctrlPath.readEn := iactAddrSPadReadEnReg
  iactAddrSPad.io.ctrlPath.readInIdx := DontCare
  iactAddrSPad.io.ctrlPath.indexInc := iactAddrSPadIdxIncWire
  iactAddrSPad.io.ctrlPath.readInIdxEn := DontCare
  // DataSPad
  iactDataSPad.io.dataPath.writeInData <> io.iactIOs.dataIOs
  iactDataIndexWire := iactDataSPad.io.dataPath.columnNum
  io.iactDataReadIndex := iactDataIndexWire // for debug
  io.iactMatrixDataBin := iactDataSPad.io.dataPath.readOutData
  val iactDataCountVec: Seq[Bool] = iactDataSPad.io.dataPath.readOutData.asBools
  io.iactMatrixData := Cat(iactDataCountVec.reverse.take(cscDataWidth)).asUInt
  io.iactMatrixRow := Cat(iactDataCountVec.reverse.takeRight(cscCountWidth)).asUInt
  iactDataSPad.io.ctrlPath.writeEn := io.writeEn
  io.iactDataWriteIdx := iactDataSPad.io.ctrlPath.writeIdx
  io.iactWF.dataWriteFin := iactDataSPad.io.ctrlPath.writeFin
  iactDataSPad.io.ctrlPath.readEn := iactDataSPadReadEnReg
  iactDataSPad.io.ctrlPath.readInIdx := DontCare
  iactDataSPad.io.ctrlPath.indexInc := iactDataSPadIdxIncWire
  iactDataSPad.io.ctrlPath.readInIdxEn := DontCare
  // the_index_m0 = m0 + count_m0
  // addr_m0_index*M0
  // SPad read state machine
  switch (sPad) {
    is(padIdle) {
      when(io.iactDataReq) {
        sPad := padIactAddr
        iactAddrSPadReadEnReg := true.B
        iactDataSPadReadEnReg := false.B
        iactDataSPadFirstReadReg := true.B
      }
    }
    is(padIactAddr) {
      // state transform
      when (iactAddrDataWire === iactZeroColumnCode.U) {
        sPad := padIactAddr
        iactSPadZeroColumnReg := true.B
        iactAddrSPadReadEnReg := true.B
        iactDataSPadReadEnReg := false.B
        iactZeroColumnNumber := iactZeroColumnNumber + 1.U
      }.otherwise {
        sPad := padIactData
        iactAddrSPadReadEnReg := false.B
        iactDataSPadReadEnReg := true.B
      }
    }
    is(padIactData) {
      iactDataSPadFirstReadReg := false.B
      when (io.iactMatrixData === 0.U) { // that's means we're finished
        sPad := padIdle
        iactDataSPadReadEnReg := false.B
        iactSPadZeroColumnReg := false.B
      }
      when (iactAddrSPadIdxIncWire) {
        sPad := padIactAddr
        iactSPadZeroColumnReg := false.B // only after the last data read and then
        // the reg can be set to false (like the column is 5, 5, 7)
        iactAddrSPadReadEnReg := true.B
        iactDataSPadReadEnReg := false.B
        when (iactSPadZeroColumnReg) {
          iactMatrixColumnReg := iactMatrixColumnReg + 1.U + iactZeroColumnNumber
          // if zero, then add one and the number of continued zero column
          iactZeroColumnNumber := 0.U // then reset it to default zero
        } .otherwise {
          iactMatrixColumnReg := iactMatrixColumnReg + 1.U // if normal column, add one
        }
      } .otherwise {
        sPad := padIactData
        iactAddrSPadReadEnReg := false.B
        iactDataSPadReadEnReg := true.B
      }
    }
  }
}


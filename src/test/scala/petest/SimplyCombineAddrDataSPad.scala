package dla.test.petest

import chisel3._
import chisel3.util._
import dla.pe.{DataAddrStreanIO, SPadAddrModule, SPadDataModule, SPadSizeConfig}

class SimplyCombineAddrDataSPad extends Module with SPadSizeConfig{
  val io = IO(new Bundle{
    val iactIOs = new DataAddrStreanIO(iactDataWidth, iactAddrWidth, commonLenWidth, commonLenWidth)
    val iactAddrWriteIdx: UInt = Output(UInt(commonLenWidth.W)) // use for test
    val iactDataReq: Bool = Input(Bool()) // control to read data vector
    // we are supposed to see address SPad and data SPad together
    val iactMatrixColumn: UInt = Output(UInt(commonLenWidth.W))
    val iactMatrixRow: UInt = Output(UInt(cscCountWidth.W))
    val iactMatrixData: UInt = Output(UInt(cscDataWidth.W))
    val iactMatrixDataBin: UInt = Output(UInt(iactDataWidth.W))
    val iactAddrReadEn: Bool = Output(Bool())
    val iactAddrReadData: UInt = Output(UInt(iactAddrWidth.W))
    // val iactAddrReadIndex: UInt = Output(UInt(commonLenWidth.W)) = iactMatrixColumn
    val iactDataReadIndex: UInt = Output(UInt(commonLenWidth.W))
    val iactDataWriteIdx: UInt = Output(UInt(commonLenWidth.W))
    val writeEn: Bool = Input(Bool())
  })
  val iactAddrSPad: SPadAddrModule = Module(new SPadAddrModule(commonLenWidth, iactAddrSPadSize, iactAddrWidth))
  val iactDataSPad: SPadDataModule = Module(new SPadDataModule(commonLenWidth, iactDataSPadSize, iactDataWidth, false))

  val padIdle :: padIactAddr :: padIactData :: Nil = Enum(3)
  val sPad: UInt = RegInit(padIdle)
  val iactAddrIndexWire: UInt = Wire(UInt(commonLenWidth.W))
  val iactAddrDataWire: UInt = Wire(UInt(iactAddrWidth.W))
  val iactDataIndexWire: UInt = Wire(UInt(commonLenWidth.W)) // use for address vector readEn
  val iactSPadZeroColumnReg: Bool = RegInit(false.B) // true, it is a zero column, then need read again
  val iactAddrSPadReadEnReg: Bool = RegInit(false.B)
  val iactDataSPadReadEnReg: Bool = RegInit(false.B)
  val iactAddrSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of address SPad
  val iactDataSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of data SPad
  val iactMatrixColumnReg: UInt = RegInit(0.U(commonLenWidth.W))
  val iactZeroColumnNumber: UInt = RegInit(0.U(commonLenWidth.W)) // use for get the right column number
  val iactDataSPadFirstReadReg: Bool = RegInit(true.B)
  iactAddrSPad.io.addrIO.indexInc := iactAddrSPadIdxIncWire
  iactDataSPad.io.dataIO.indexInc := iactDataSPadIdxIncWire
  iactAddrSPadIdxIncWire := ((sPad === padIactData) && (iactAddrDataWire === (iactDataIndexWire + 1.U))) || ((sPad === padIactAddr) && (iactAddrDataWire === iactZeroColumnCode.U))
  iactDataSPadIdxIncWire := ((sPad === padIactAddr) && !iactSPadZeroColumnReg && !iactDataSPadFirstReadReg) || ((sPad === padIactData) && !iactAddrSPadIdxIncWire)// if first read, then keep the read index of zero
  iactAddrSPad.io.commonIO.dataLenFinIO <> io.iactIOs.addrIOs // this is different from the real module
  iactDataSPad.io.commonIO.dataLenFinIO <> io.iactIOs.dataIOs
  io.iactAddrWriteIdx := iactAddrSPad.io.commonIO.writeIdx
  io.iactDataWriteIdx := iactDataSPad.io.commonIO.writeIdx
  // AddrSPad
  iactAddrIndexWire := iactAddrSPad.io.commonIO.columnNum
  io.iactMatrixColumn := iactMatrixColumnReg // for debug
  iactAddrDataWire := iactAddrSPad.io.commonIO.readOutData
  io.iactAddrReadData := iactAddrDataWire // for debug
  iactAddrSPad.io.commonIO.readEn := iactAddrSPadReadEnReg
  io.iactAddrReadEn := iactAddrSPadReadEnReg
  iactAddrSPad.io.commonIO.writeEn := io.writeEn
  iactAddrSPad.io.dataIO := DontCare
  iactAddrSPad.io.addrIO.readInIdx := DontCare
  iactAddrSPad.io.addrIO.readInIdxEn := DontCare
  // DataSPad
  iactDataIndexWire := iactDataSPad.io.commonIO.columnNum
  io.iactDataReadIndex := iactDataIndexWire // for debug
  io.iactMatrixDataBin := iactDataSPad.io.commonIO.readOutData
  val iactDataCountVec: Seq[Bool] = iactDataSPad.io.commonIO.readOutData.asBools
  io.iactMatrixData := Cat(iactDataCountVec.reverse.take(cscDataWidth)).asUInt
  io.iactMatrixRow := Cat(iactDataCountVec.reverse.takeRight(cscCountWidth)).asUInt
  iactDataSPad.io.commonIO.readEn := iactDataSPadReadEnReg
  iactDataSPad.io.commonIO.writeEn := io.writeEn
  // disable the unused IOs
  iactDataSPad.io.dataIO.readInIdx := DontCare
  iactDataSPad.io.dataIO.readInIdxEn := DontCare
  iactDataSPad.io.addrIO := DontCare
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
      when (iactDataIndexWire === io.iactIOs.dataIOs.streamLen - 1.U) {
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


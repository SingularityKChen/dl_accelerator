package dla.test.petest

import chisel3._
import chisel3.util._
import dla.pe.{CSCStreamIO, CSCWriteFinIO, SPadAdrModule, SPadDataModule, SPadSizeConfig}

class SimplyCombineAdrDataSPad extends Module with SPadSizeConfig {
  val io: SimplyCombineAdrDataSPadIO = IO(new SimplyCombineAdrDataSPadIO)
  private val inActAdrSPad: SPadAdrModule = Module(new SPadAdrModule(inActAdrSPadSize, inActAdrWidth))
  private val inActDataSPad: SPadDataModule = Module(new SPadDataModule(inActDataSPadSize, inActDataWidth, false))
  private val padIdle :: padInActAdr :: padInActData :: Nil = Enum(3)
  private val sPad: UInt = RegInit(padIdle)
  private val inActAdrIndexWire: UInt = Wire(UInt(inActAdrIdxWidth.W))
  private val inActAdrDataWire: UInt = Wire(UInt(inActAdrWidth.W))
  private val inActDataIndexWire: UInt = Wire(UInt(inActDataIdxWidth.W)) // use for address vector readEn
  private val inActSPadZeroColumnReg: Bool = RegInit(false.B) // true, it is a zero column, then need read again
  private val inActAdrSPadReadEnReg: Bool = RegInit(false.B)
  private val inActDataSPadReadEnReg: Bool = RegInit(false.B)
  private val inActAdrSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of address SPad
  private val inActDataSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of data SPad
  private val inActMatrixColumnReg: UInt = RegInit(0.U(inActAdrIdxWidth.W))
  private val inActZeroColumnNumber: UInt = RegInit(0.U(inActAdrIdxWidth.W)) // use for get the right column number
  private val inActDataSPadFirstReadReg: Bool = RegInit(true.B)
  inActAdrSPadIdxIncWire := ((sPad === padInActData) && (inActAdrDataWire === (inActDataIndexWire + 1.U))) || ((sPad === padInActAdr) && (inActAdrDataWire === inActZeroColumnCode.U))
  inActDataSPadIdxIncWire := ((sPad === padInActAdr) && !inActSPadZeroColumnReg && !inActDataSPadFirstReadReg) || ((sPad === padInActData) && !inActAdrSPadIdxIncWire)// if first read, then keep the read index of zero
  // AdrSPad
  inActAdrSPad.io.dataPath.writeInData <> io.inActIOs.adrIOs
  inActAdrIndexWire := inActAdrSPad.io.dataPath.columnNum
  io.inActMatrixColumn := inActMatrixColumnReg // for debug
  inActAdrDataWire := inActAdrSPad.io.dataPath.readOutData
  io.inActAdrReadData := inActAdrDataWire // for debug
  io.inActAdrReadEn := inActAdrSPadReadEnReg
  inActAdrSPad.io.ctrlPath.writeEn := io.writeEn
  io.inActAdrWriteIdx := inActAdrSPad.io.ctrlPath.writeIdx
  io.inActWF.adrWriteFin := inActAdrSPad.io.ctrlPath.writeFin
  inActAdrSPad.io.ctrlPath.readEn := inActAdrSPadReadEnReg
  inActAdrSPad.io.ctrlPath.readInIdx := DontCare
  inActAdrSPad.io.ctrlPath.indexInc := inActAdrSPadIdxIncWire
  inActAdrSPad.io.ctrlPath.readInIdxEn := DontCare
  // DataSPad
  inActDataSPad.io.dataPath.writeInData <> io.inActIOs.dataIOs
  inActDataIndexWire := inActDataSPad.io.dataPath.columnNum
  io.inActDataReadIndex := inActDataIndexWire // for debug
  io.inActMatrixDataBin := inActDataSPad.io.dataPath.readOutData
  private val inActDataCountVec: Seq[Bool] = inActDataSPad.io.dataPath.readOutData.asBools
  io.inActMatrixData := Cat(inActDataCountVec.reverse.take(cscDataWidth)).asUInt
  io.inActMatrixRow := Cat(inActDataCountVec.reverse.takeRight(cscCountWidth)).asUInt
  inActDataSPad.io.ctrlPath.writeEn := io.writeEn
  io.inActDataWriteIdx := inActDataSPad.io.ctrlPath.writeIdx
  io.inActWF.dataWriteFin := inActDataSPad.io.ctrlPath.writeFin
  inActDataSPad.io.ctrlPath.readEn := inActDataSPadReadEnReg
  inActDataSPad.io.ctrlPath.readInIdx := DontCare
  inActDataSPad.io.ctrlPath.indexInc := inActDataSPadIdxIncWire
  inActDataSPad.io.ctrlPath.readInIdxEn := DontCare
  // the_index_m0 = m0 + count_m0
  // adr_m0_index*M0
  // SPad read state machine
  switch (sPad) {
    is(padIdle) {
      when(io.inActDataReq) {
        sPad := padInActAdr
        inActAdrSPadReadEnReg := true.B
        inActDataSPadReadEnReg := false.B
        inActDataSPadFirstReadReg := true.B
      }
    }
    is(padInActAdr) {
      // state transform
      when (inActAdrDataWire === inActZeroColumnCode.U) {
        sPad := padInActAdr
        inActSPadZeroColumnReg := true.B
        inActAdrSPadReadEnReg := true.B
        inActDataSPadReadEnReg := false.B
        inActZeroColumnNumber := inActZeroColumnNumber + 1.U
      }.otherwise {
        sPad := padInActData
        inActAdrSPadReadEnReg := false.B
        inActDataSPadReadEnReg := true.B
      }
    }
    is(padInActData) {
      inActDataSPadFirstReadReg := false.B
      when (io.inActMatrixData === 0.U) { // that's means we're finished
        sPad := padIdle
        inActDataSPadReadEnReg := false.B
        inActSPadZeroColumnReg := false.B
      }
      when (inActAdrSPadIdxIncWire) {
        sPad := padInActAdr
        inActSPadZeroColumnReg := false.B // only after the last data read and then
        // the reg can be set to false (like the column is 5, 5, 7)
        inActAdrSPadReadEnReg := true.B
        inActDataSPadReadEnReg := false.B
        when (inActSPadZeroColumnReg) {
          inActMatrixColumnReg := inActMatrixColumnReg + 1.U + inActZeroColumnNumber
          // if zero, then add one and the number of continued zero column
          inActZeroColumnNumber := 0.U // then reset it to default zero
        } .otherwise {
          inActMatrixColumnReg := inActMatrixColumnReg + 1.U // if normal column, add one
        }
      } .otherwise {
        sPad := padInActData
        inActAdrSPadReadEnReg := false.B
        inActDataSPadReadEnReg := true.B
      }
    }
  }
}

class SimplyCombineAdrDataSPadIO extends Bundle with SPadSizeConfig {
  val inActIOs: CSCStreamIO = Flipped(new CSCStreamIO(inActDataWidth, inActAdrWidth))
  val inActWF = new CSCWriteFinIO
  val inActAdrWriteIdx: UInt = Output(UInt(inActAdrIdxWidth.W)) // use for test
  val inActDataReq: Bool = Input(Bool()) // control to read data vector
  // we are supposed to see address SPad and data SPad together
  val inActMatrixColumn: UInt = Output(UInt(inActAdrIdxWidth.W))
  val inActMatrixRow: UInt = Output(UInt(cscCountWidth.W))
  val inActMatrixData: UInt = Output(UInt(cscDataWidth.W))
  val inActMatrixDataBin: UInt = Output(UInt(inActDataWidth.W))
  val inActAdrReadEn: Bool = Output(Bool())
  val inActAdrReadData: UInt = Output(UInt(inActAdrWidth.W))
  // val inActAdrReadIndex: UInt = Output(UInt(commonLenWidth.W)) = inActMatrixColumn
  val inActDataReadIndex: UInt = Output(UInt(inActDataIdxWidth.W))
  val inActDataWriteIdx: UInt = Output(UInt(log2Ceil(inActDataSPadSize).W))
  val writeEn: Bool = Input(Bool())
}

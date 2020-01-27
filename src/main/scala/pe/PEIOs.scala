package dla.pe

import chisel3._
import chisel3.util._

class PETopDebugIO extends Bundle {
  val peControlDebugIO = new PEControlDebugIO
  val peSPadDebugIO = new PESPadDebugIO
}

class PEControlDebugIO extends Bundle {
  val peState: UInt = Output(UInt(2.W))
}

class PESPadDebugIO extends Bundle with PESizeConfig {
  val iactMatrixData: UInt = Output(UInt(cscDataWidth.W))
  val iactMatrixRow: UInt = Output(UInt(cscCountWidth.W))
  val iactMatrixColumn: UInt = Output(UInt(commonLenWidth.W))
  val iactAddrInc: Bool = Output(Bool())
  val iactDataInc: Bool = Output(Bool())
  val iactAddrIdx: UInt = Output(UInt(iactAddrWidth.W))
  val weightAddrSPadReadOut: UInt = Output(UInt(weightAddrWidth.W))
  val weightMatrixData: UInt = Output(UInt(cscDataWidth.W))
  val weightMatrixRow: UInt = Output(UInt(cscCountWidth.W))
  val productResult: UInt = Output(UInt(psDataWidth.W))
  val pSumResult: UInt = Output(UInt(psDataWidth.W))
  val pSumLoad: UInt = Output(UInt(psDataWidth.W))
  val weightAddrInIdx: UInt = Output(UInt(cscCountWidth.W))
  val sPadState: UInt = Output(UInt(3.W))
}

class SPadAddrIO(val dataWidth: Int, val padSize: Int) extends Bundle {
  val readInIdx: UInt = Input(UInt(log2Ceil(padSize).W))
  val indexInc: Bool = Input(Bool())
  val readInIdxEn: Bool = Input(Bool()) // for weight SPad only, true then padReadIndexReg = readInIdx
}

class SPadDataIO(val dataWidth: Int, val padSize: Int) extends Bundle {
  val readInIdx: UInt = Input(UInt(log2Ceil(padSize).W))
  val indexInc: Bool = Input(Bool())
  val readInIdxEn: Bool = Input(Bool()) // for weight SPad only, true then padReadIndexReg = readInIdx
}

class SPadCommonIO(val dataLenWidth: Int, val dataWidth: Int, val padSize: Int) extends Bundle {
  val columnNum: UInt = Output(UInt(dataLenWidth.W)) // the column number, address only
  val readOutData: UInt = Output(UInt(dataWidth.W)) // the data read from SPad
  val readEn: Bool = Input(Bool())
  val writeEn: Bool = Input(Bool())
  val writeIdx: UInt = Output(UInt(log2Ceil(padSize).W))
  val dataLenFinIO = new StreamDataLenFinIO(dataWidth, dataLenWidth)
}

class DataStreamIO extends Bundle with PESizeConfig {
  val ipsIO: DecoupledIO[UInt] = Flipped(Decoupled(UInt(psDataWidth.W)))
  val opsIO: DecoupledIO[UInt] = Decoupled(UInt(psDataWidth.W))
  val iactIOs = new DataAddrStreanIO(iactDataWidth, iactAddrWidth, commonLenWidth, commonLenWidth)
  val weightIOs = new DataAddrStreanIO(weightDataWidth, weightAddrWidth, weightDataLenWidth, commonLenWidth)
}

class DataAddrStreanIO(val dataWidth: Int, addr_width: Int, dataLenWidth: Int, addrLen_width: Int) extends Bundle {
  val dataIOs = new StreamDataLenFinIO(dataWidth, dataLenWidth) // dataSPad inputs and output writeFin
  val addrIOs = new StreamDataLenFinIO(addr_width, addrLen_width) // addrSPad inputs and output writeFin
}

class StreamDataLenFinIO(val streamWidth: Int, streamLenWidth: Int) extends Bundle { // for write data into SPad
  val writeInDataIO: DecoupledIO[StreamBitsIO] = Flipped(Decoupled(new StreamBitsIO(streamWidth)))
  val streamLen: UInt = Input(UInt(streamLenWidth.W)) // length of addr
  val writeFin: Bool = Output(Bool())
}

class StreamBitsIO(val dataWidth: Int) extends Bundle {
  val data: UInt = UInt(dataWidth.W)
}

class PECtrlToPadIO extends Bundle {
  val doMACEn: Bool = Output(Bool()) // true, then begin MAC computations
  val fromTopIO: PETopToHigherIO = Flipped(new PETopToHigherIO)
}

class PETopToHigherIO extends Bundle {
  val pSumEnqOrProduct: DecoupledIO[Bool] = Flipped(Decoupled(Bool())) // true, then read from FIFO; false, then use product
  val doLoadEn: Bool = Input(Bool()) // true, then write data into iact and weight SPad and read data out from psData SPad
  val writeFinish: Bool = Output(Bool()) // true then write data into the Scratch Pad finished
  val calFinish: Bool = Output(Bool()) // true then MAC computations finished
}

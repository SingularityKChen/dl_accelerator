package dla.pe

import chisel3._
import chisel3.util._

class ProcessingElementIO extends Bundle {
  val dataStream = new DataStreamIO
  val padWF = new PEPadWriteFinIO
  val topCtrl = new PETopToHigherIO
  val debugIO = new PETopDebugIO
}

class PETopDebugIO extends Bundle {
  val peControlDebugIO = new PEControlDebugIO
  val peSPadDebugIO = new PESPadDebugIO
}

class PEControlDebugIO extends Bundle {
  val peState: UInt = Output(UInt(2.W))
  val doMACEnDebug: Bool = Output(Bool())
}

class PESPadDebugIO extends Bundle with PESizeConfig {
  val iactMatrixData: UInt = Output(UInt(cscDataWidth.W))
  val iactMatrixRow: UInt = Output(UInt(cscCountWidth.W))
  val iactMatrixColumn: UInt = Output(UInt(iactAddrWidth.W))
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

class SPadCommonDataIO(dataWidth: Int, padSize: Int) extends Bundle {
  val columnNum: UInt = Output(UInt(log2Ceil(padSize).W)) // the column number, address only
  val readOutData: UInt = Output(UInt(dataWidth.W)) // the data read from SPad
  val writeInData: StreamBitsIO = Flipped(new StreamBitsIO(dataWidth))
}

class SPadCommonCtrlIO(padSize: Int) extends Bundle {
  val writeEn: Bool = Input(Bool())
  val writeIdx: UInt = Output(UInt(log2Ceil(padSize).W))
  val writeFin: Bool = Output(Bool())
  val readEn: Bool = Input(Bool())
  val readInIdx: UInt = Input(UInt(log2Ceil(padSize).W))
  val indexInc: Bool = Input(Bool()) // true to increase the index of address
  val readInIdxEn: Bool = Input(Bool())
}

class DataStreamIO extends Bundle with PESizeConfig {
  val ipsIO: DecoupledIO[UInt] = Flipped(Decoupled(UInt(psDataWidth.W)))
  val opsIO: DecoupledIO[UInt] = Decoupled(UInt(psDataWidth.W))
  val iactIOs: CSCStreamIO = Flipped(new CSCStreamIO(iactDataWidth, iactAddrWidth))
  val weightIOs: CSCStreamIO = Flipped(new CSCStreamIO(weightDataWidth, weightAddrWidth))
}

class CSCStreamIO(val dataWidth: Int, val addrWidth: Int) extends Bundle {
  val addrIOs = new StreamBitsIO(addrWidth) // output bits and valid
  val dataIOs = new StreamBitsIO(dataWidth) // output bits and valid
}

class CSCWriteFinIO extends Bundle {
  val addrWriteFin: Bool = Output(Bool())
  val dataWriteFin: Bool = Output(Bool())
}

class PEPadWriteFinIO extends Bundle {
  val iactWriteFin = new CSCWriteFinIO
  val weightWriteFin = new CSCWriteFinIO
}

class StreamBitsIO(val dataWidth: Int) extends Bundle {
  val data: DecoupledIO[UInt] = Decoupled(UInt(dataWidth.W))
}

class PECtrlToPadIO extends Bundle {
  val doMACEn: Bool = Output(Bool()) // true, then begin MAC computations
  val fromTopIO: PETopToHigherIO = Flipped(new PETopToHigherIO)
}

class PEControlToTopIO extends PETopToHigherIO {
  override val writeFinish: Bool = Input(Bool())
}

class PETopToHigherIO extends Bundle {
  val pSumEnqOrProduct: DecoupledIO[Bool] = Flipped(Decoupled(Bool())) // true, then read from FIFO; false, then use product
  val doLoadEn: Bool = Input(Bool()) // true, then write data into iact and weight SPad and read data out from psData SPad
  val writeFinish: Bool = Output(Bool()) // true then write data into the Scratch Pad finished
  val calFinish: Bool = Output(Bool()) // true then MAC computations finished
}

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
  val inActMatrixData: UInt = Output(UInt(cscDataWidth.W))
  val inActMatrixRow: UInt = Output(UInt(cscCountWidth.W))
  val inActMatrixColumn: UInt = Output(UInt(inActAdrWidth.W))
  val inActAdrInc: Bool = Output(Bool())
  val inActDataInc: Bool = Output(Bool())
  val inActAdrIdx: UInt = Output(UInt(inActAdrWidth.W))
  val weightAdrSPadReadOut: UInt = Output(UInt(weightAdrWidth.W))
  val weightMatrixData: UInt = Output(UInt(cscDataWidth.W))
  val weightMatrixRow: UInt = Output(UInt(cscCountWidth.W))
  val productResult: UInt = Output(UInt(psDataWidth.W))
  val pSumResult: UInt = Output(UInt(psDataWidth.W))
  val pSumLoad: UInt = Output(UInt(psDataWidth.W))
  val weightAdrInIdx: UInt = Output(UInt(cscCountWidth.W))
  val sPadState: UInt = Output(UInt(3.W))
}

class SPadCommonModuleIO(private val dataWidth: Int, private val padSize: Int) extends Bundle {
  val dataPath = new SPadCommonDataIO(dataWidth, padSize)
  val ctrlPath = new SPadCommonCtrlIO(padSize)
}

class SPadCommonDataIO(private val dataWidth: Int, private val padSize: Int) extends Bundle {
  val columnNum: UInt = Output(UInt(log2Ceil(padSize).W)) // the column number, address only
  val readOutData: UInt = Output(UInt(dataWidth.W)) // the data read from SPad
  val writeInData: StreamBitsIO = Flipped(new StreamBitsIO(dataWidth))
}

class SPadCommonCtrlIO(private val padSize: Int) extends Bundle {
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
  // TODO: combine ips and ops
  //val pSumDataIOs = new PSumSPadDataIO
  val inActIOs: CSCStreamIO = Flipped(new CSCStreamIO(adrWidth = inActAdrWidth, dataWidth = inActDataWidth))
  val weightIOs: CSCStreamIO = Flipped(new CSCStreamIO(adrWidth = weightAdrWidth, dataWidth = weightDataWidth))
}

class CSCStreamIO(private val adrWidth: Int, private val dataWidth: Int) extends Bundle {
  val adrIOs = new StreamBitsIO(adrWidth) // output bits and valid
  val dataIOs = new StreamBitsIO(dataWidth) // output bits and valid
}

class CSCWriteFinIO extends Bundle {
  val adrWriteFin: Bool = Output(Bool())
  val dataWriteFin: Bool = Output(Bool())
}

class ProcessingElementPadIO extends Bundle {
  val padCtrl: PECtrlToPadIO = Flipped(new PECtrlToPadIO)
  val dataStream = new DataStreamIO
  val debugIO = new PESPadDebugIO
  val padWF = new PEPadWriteFinIO
}

class PEPadWriteFinIO extends Bundle {
  val inActWriteFin = new CSCWriteFinIO
  val weightWriteFin = new CSCWriteFinIO
  val pSumWriteFin: Bool = Output(Bool())
}

class StreamBitsIO(private val dataWidth: Int) extends Bundle {
  val data: DecoupledIO[UInt] = Decoupled(UInt(dataWidth.W))
}

class ProcessingElementControlIO extends Bundle {
  val ctrlPad = new PECtrlToPadIO
  val ctrlTop = new PEControlToTopIO
  val debugIO = new PEControlDebugIO
}

class PECtrlToPadIO extends Bundle {
  val doMACEn: Bool = Output(Bool()) // true, then begin MAC computations
  val fromTopIO: PETopToHigherIO = Flipped(new PETopToHigherIO)
}

class PEControlToTopIO extends PETopToHigherIO {
  override val writeFinish: Bool = Input(Bool())
}

class PETopToHigherIO extends Bundle {
  val pSumEnqEn: Bool = Input(Bool()) // true, then read from FIFO and write the data into SPad
  val doLoadEn: Bool = Input(Bool()) // true, then write data into inAct and weight SPad and read data out from psData SPad
  val writeFinish: Bool = Output(Bool()) // true then write data into the Scratch Pad finished
  val calFinish: Bool = Output(Bool()) // true then MAC computations finished
}

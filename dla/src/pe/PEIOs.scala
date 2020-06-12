package dla.pe

import chisel3.{UInt, _}
import chisel3.util.{log2Ceil, _}

class ProcessingElementIO extends Bundle {
  val dataStream = new DataStreamIO
  val padWF = new PEPadWriteFinIO
  val topCtrl = new PETopToHigherIO
  val debugIO = new PETopDebugIO
}

class PETopDebugIO extends Bundle {
  val peControlDebugIO = new PEControlDebugIO
  val peSPadDebugIO = new PESPadDebugIO
  val writeFinishRegVec: Vec[Bool] = Output(Vec(4, Bool()))
}

class PEControlDebugIO extends Bundle {
  val peState: UInt = Output(UInt(2.W))
  val doMACEnDebug: Bool = Output(Bool())
}

class PESPadDebugIO extends Bundle with PESizeConfig with SPadSizeConfig with MCRENFConfigRS {
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
  val pSumReadIdx: UInt = Output(UInt(log2Ceil(pSumDataSPadSize).W))
  //f: for debug, original IdxReg
  val pSumPadReadIdx: UInt = Output(UInt(log2Ceil(pSumDataSPadSize).W))
  val inActDataSliding: UInt = Output(UInt(log2Ceil(2).W))
  val inActDataSlidingFire: Bool = Output(Bool())
  val futureLBStart: UInt = Output(UInt(log2Ceil(C0 * (S + F)).W))
  val inActDataIndex: UInt = Output(UInt(inActDataIdxWidth.W))
  val inActAdrData:UInt = Output(UInt(inActAdrWidth.W))
}

class SPadCommonModuleIO(private val dataWidth: Int, private val padSize: Int) extends Bundle {
  val dataPath = new SPadCommonDataIO(dataWidth, padSize)
  val ctrlPath = new SPadCommonCtrlIO(padSize)
}

class SPadNonTpModuleIO(private val dataWidth: Int, private val padSize: Int) extends Bundle {
  val dataPath = new SPadNonTpDataIO(dataWidth, padSize)
  val ctrlPath = new SPadNonTpCtrlIO(padSize)
}

class SPadCommonDataIO(private val dataWidth: Int, private val padSize: Int) extends Bundle with MCRENFConfigRS {
  val columnNum: UInt = Output(UInt(log2Ceil(padSize).W)) // padReadIndexReg, idx which is currently reading, address only
  val readOutData: UInt = Output(UInt(dataWidth.W)) // the data read from SPad
  val writeInData: StreamBitsIO = Flipped(new StreamBitsIO(dataWidth))
}

class SPadNonTpDataIO(private val dataWidth: Int, private val padSize: Int) extends SPadCommonDataIO(dataWidth, padSize) with MCRENFConfigRS with PESizeConfig {
  val currentSliding: UInt = Input(UInt(log2Ceil(F).W))
  val slidingBoxUB: UInt = Output(UInt(log2Ceil(C0 * (S + F) ).W))
  val slidingBoxLB: UInt = Output(UInt(log2Ceil(C0 * (S + F) ).W))
  val futureLBStart: UInt = Output(UInt(log2Ceil(C0 * (S + F) ).W))
}

class SPadCommonCtrlIO(private val padSize: Int) extends Bundle {
  val writeEn: Bool = Input(Bool())
  val writeIdx: UInt = Output(UInt(log2Ceil(padSize).W))
  val writeFin: Bool = Output(Bool())
  val readEn: Bool = Input(Bool())       // todo seems could be removed
  val readInIdx: UInt = Input(UInt(log2Ceil(padSize).W))
  val indexInc: Bool = Input(Bool()) // true to increase the index of address
  val readInIdxEn: Bool = Input(Bool())
}

class SPadNonTpCtrlIO(private val padSize: Int) extends SPadCommonCtrlIO(padSize) {
  val slidingInc: Bool = Output(Bool()) // true to slide the window for inAct
  val inActColInc: Bool = Input(Bool())
  val weightIdxInc: Bool = Input(Bool())
  val padEqMpyBool: Bool = Input(Bool())
  val mightWeightZeroColumn: Bool = Input(Bool())
  val padEqWABool: Bool = Input(Bool())
  val padEqIDBool: Bool = Input(Bool())
  val inActReadIdxBeyondUB: Bool = Output(Bool())
  val inActLastNonZeroEle: Bool = Output(Bool())
}

class DataStreamIO extends Bundle with PESizeConfig {
  val ipsIO: DecoupledIO[UInt] = Flipped(Decoupled(UInt(psDataWidth.W))) /** input PSum*/
  val opsIO: DecoupledIO[UInt] = Decoupled(UInt(psDataWidth.W))  /** output PSum*/
  // TODO: combine ips and ops
  //val pSumDataIOs = new PSumSPadDataIO            // above for psum, below for inputs ports of the CSC inAct & weight data
  val inActIOs: CSCStreamIO = Flipped(new CSCStreamIO(adrWidth = inActAdrWidth, dataWidth = inActDataWidth))
  val weightIOs: CSCStreamIO = Flipped(new CSCStreamIO(adrWidth = weightAdrWidth, dataWidth = weightDataWidth))
}

class CSCStreamIO(private val adrWidth: Int, private val dataWidth: Int) extends Bundle {
  val adrIOs = new StreamBitsIO(adrWidth) // output bits and valid for queue
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
  val pSumAddFin: Bool = Output(Bool())
}

class StreamBitsIO(private val dataWidth: Int) extends Bundle {
  val data: DecoupledIO[UInt] = Decoupled(UInt(dataWidth.W))       //fred: queue output port
}

class ProcessingElementControlIO extends Bundle {
  val ctrlPad = new PECtrlToPadIO
  val ctrlTop = new PEControlToTopIO
  val debugIO = new PEControlDebugIO
}

class PECtrlToPadIO extends Bundle {
  val doMACEn: Bool = Output(Bool()) // true, then begin MAC computations
  val fromTopIO: PETopToHigherIO = Flipped(new PETopToHigherIO) // fred: it should be spadToTopIO
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

/** PSumSPad IOs*/
class PSumSPadIO extends Bundle {
  val dataPath = new PSumSPadDataIO
  val ctrlPath = new PSumSPadCtrlIO
}

class PSumSPadDataIO extends Bundle with PESizeConfig {
  val ipsIO: DecoupledIO[UInt] = Flipped(Decoupled(UInt(psDataWidth.W)))
  val opsIO: DecoupledIO[UInt] = Decoupled(UInt(psDataWidth.W))
}

class PSumSPadCtrlIO extends Bundle with SPadSizeConfig {
  val readIdx: UInt = Input(UInt(log2Ceil(pSumDataSPadSize).W))
  val writeIdx: UInt = Input(UInt(log2Ceil(pSumDataSPadSize).W))
}

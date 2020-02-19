package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe.{CSCStreamIO, MCRENFConfig, StreamBitsIO}

class GLBCluster(debug: Boolean) extends Module with ClusterSRAMConfig {
  val io: GLBClusterIO = IO(new GLBClusterIO)
  private val iSRAMs: Vec[InActSRAMBankIO] = Vec(inActSRAMNum, Module(new InActSRAMBank(debug: Boolean)).io)
  private val pSRAMs: Vec[PSumSRAMBankIO] = Vec(pSumSRAMNum, Module(new PSumSRAMBank(pSumSRAMSize, psDataWidth, debug)).io)
  // connections of data path
  io.dataPath.weightIO.foreach(x => x.inIOs <> x.outIOs)
  io.dataPath.inActIO.zip(iSRAMs).foreach({ case (topIO, sramIO) => topIO <> sramIO.dataPath})
  io.dataPath.pSumIO.zip(pSRAMs).foreach({ case (topIO, sramIO) => topIO <> sramIO.dataPath})
  // connections of control path
}

class PSumSRAMBank(private val theSRAMSize: Int, private val theDataWidth: Int, debug: Boolean) extends SRAMCommon(theSRAMSize, theDataWidth) with ClusterSRAMConfig with MCRENFConfig with GNMFCS2Config {
  private val oneSPadPSum: Int = M0*E*N0*F0 // when read counts this, then stop
  private val oneSRAMPSum: Int = oneSPadPSum * N2*M2*F2 // when write counts this, then finished
  require(oneSRAMPSum <= pSumSRAMSize, s"the size of PSum SRAM is $pSumSRAMSize, " +
    s"which should be larger than the config's requirement, which is $oneSRAMPSum")
  val io: PSumSRAMBankIO = IO(new PSumSRAMBankIO)
  // SRAM read write logic
  private val startIdx = Wire(UInt(log2Ceil(pSumSRAMSize).W)) // the first do address, from io
  private val doIdxWire: UInt = Wire(UInt(log2Ceil(theSRAMSize).W))
  // read or write one SPad size partial sums
  // doIdxCount means current number of PSums that has been done
  // when finish, doDone = true.B
  private val (doIdxCount, doDoneWire) = Counter(doIdxIncWire, oneSPadPSum)
  startIdx := io.ctrlPath.startIdx
  writeOrRead := io.ctrlPath.writeOrRead
  io.ctrlPath.done := doDoneWire
  doDoneReg := doDoneWire
  //doEnWire := !doDoneReg && io.ctrlPath.doEn // FIXME
  doEnWire := io.ctrlPath.doEn // FIXME
  doIdxWire := startIdx + doIdxCount
  // write logic
  writeLogic(io.dataPath.inIOs, doIdxWire)
  // read logic
  readLogic(io.dataPath.outIOs, doIdxWire)
  // debug io
  if (debug) {
    debugLogic(io.debugIO, doIdxWire)
    io.debugIO.idxCount := doIdxCount
  } else {
    io.debugIO <> DontCare
  }
}

abstract class SRAMCommon(private val theSRAMSize: Int, private val theDataWidth: Int) extends Module {
  protected val theSRAM: SyncReadMem[UInt] = SyncReadMem(theSRAMSize, UInt(theDataWidth.W))
  // SRAM read write logic
  protected val writeOrRead: Bool = Wire(Bool()) // true then "do" means write, false then "do" means read
  // because it will not read and write at the same time
  // so we just need one pair of regs to record process
  protected val doEnWire: Bool = Wire(Bool())
  protected val doReadWire: Bool = Wire(Bool())
  protected val doWriteWire: Bool = Wire(Bool())
  protected val writeInData: UInt = Wire(UInt(theDataWidth.W))
  protected val readOutData: UInt = Wire(UInt(theDataWidth.W))
  protected val waitForRead: Bool = RegInit(true.B) // use for letting increase slower when read
  protected val doIdxIncWire: Bool = Wire(Bool()) // true, then the index increase
  protected val nextValid: Bool = Wire(Bool())
  protected val doDoneReg: Bool = RegInit(false.B) // or combinational loop detected
  doIdxIncWire := Mux(writeOrRead, doWriteWire, doReadWire && waitForRead)
  def readLogic(readOutIO: DecoupledIO[UInt], doIdx: UInt): Any = {
    nextValid := doEnWire
    waitForRead := !waitForRead
    readOutIO.valid := RegNext(nextValid) // reg next, so one cycle later, data will be read out with valid signal
    doReadWire := readOutIO.ready && nextValid
    readOutData := theSRAM.read(doIdx, doReadWire && !waitForRead)
    readOutIO.bits := Mux(writeOrRead, 0.U, readOutData)
  }

  def writeLogic(writeInIO: DecoupledIO[UInt], doIdx: UInt): Any = {
    writeInData := Mux(writeOrRead, writeInIO.bits, 0.U)
    doWriteWire := doEnWire && writeInIO.valid
    writeInIO.ready := doWriteWire
    when (doIdxIncWire) {
      theSRAM.write(doIdx, writeInData)
    }
  }

  def debugLogic(debugIO: SRAMCommonDebugIO, doIdx: UInt): Any = {
    debugIO.idx := doIdx
    debugIO.idxInc := doIdxIncWire
  }
}

class InActSRAMCommon(private val theSRAMSize: Int, private val theDataWidth: Int, debug: Boolean) extends SRAMCommon(theSRAMSize, theDataWidth) {
  val io: InACTSRAMCommonIO = IO(new InACTSRAMCommonIO(theSRAMSize, theDataWidth))
  private val doMeetZeroWire = Wire(Bool()) // current data equals to zero
  private val doMightFinishedReg = RegInit(false.B) // that's the end of one SPad's data
  private val writeDoneWire = Wire(Bool())
  private val readDoneWire = Wire(Bool())
  private val doIdxReg: UInt = RegInit(0.U(log2Ceil(theSRAMSize).W))
  // SRAM read write logic
  // we assume that one write cycle won't start until read finishes, i.e., the index can be hold when calculating,
  // then continue reading
  private val currentData = Wire(UInt(log2Ceil(theSRAMSize).W))
  private val doDoneWire = Wire(Bool())
  // TODO: add some logic to check whether one SPad of data is a zero matrix, true then jump
  writeOrRead := io.ctrlPath.writeOrRead
  io.ctrlPath.done := doDoneWire
  doDoneReg := doDoneWire
  doEnWire := !doDoneReg && io.ctrlPath.doEn // FIXME
  currentData := Mux(writeOrRead, writeInData, readOutData)
  when (doIdxIncWire) {
    doIdxReg := doIdxReg + 1.U
  }
  // write logic
  writeLogic(io.dataPath.inIOs.data, doIdxReg)
  // read logic
  readLogic(io.dataPath.outIOs.data, doIdxReg)
  // do finish?
  doMeetZeroWire := currentData === 0.U
  doMightFinishedReg := doMeetZeroWire && doEnWire // meets one zero, that's the end of one SPad
  // if meet two continuous zeros, then the group of data finishes
  writeDoneWire := doMeetZeroWire && doEnWire && doMightFinishedReg
  readDoneWire := doMightFinishedReg
  doDoneWire := Mux(writeOrRead, writeDoneWire, readDoneWire)
  // debug io
  if (debug) {
    debugLogic(io.debugIO, doIdxReg)
    io.debugIO.idxCount := DontCare
  } else {
    io.debugIO <> DontCare
  }
}

class InActSRAMBank(debug: Boolean) extends Module with ClusterSRAMConfig {
  val io = new InActSRAMBankIO
  private val adrSRAM = Module(new InActSRAMCommon(inActAdrSRAMSize, inActAdrWidth, debug)).io
  private val dataSRAM = Module(new InActSRAMCommon(inActDataSRAMSize, inActDataWidth, debug)).io
  // SRAM connections
  adrSRAM.dataPath <> io.dataPath.inIOs.adrIOs
  dataSRAM.dataPath <> io.dataPath.inIOs.dataIOs
  io.ctrlPath.done := adrSRAM.ctrlPath.done && dataSRAM.ctrlPath.done
  adrSRAM.ctrlPath.writeOrRead := io.ctrlPath.writeOrRead
  dataSRAM.ctrlPath.writeOrRead := io.ctrlPath.writeOrRead
}

class SRAMCommonCtrlIO extends Bundle {
  val writeOrRead: Bool = Input(Bool())
  val doEn: Bool = Input(Bool())
  val done: Bool = Output(Bool())
}

class SRAMCommonDebugIO(private val theSRAMSize: Int, private val theDataWidth: Int) extends Bundle {
  val idx: UInt = Output(UInt(log2Ceil(theSRAMSize).W))
  val idxInc: Bool = Output(Bool())
  val idxCount: UInt = Output(UInt(log2Ceil(theSRAMSize).W))
}

trait WithStratIdxIO extends Bundle with ClusterSRAMConfig {
  val startIdx: UInt = Input(UInt(log2Ceil(pSumSRAMSize).W))
}

class InACTSRAMCommonIO(private val theSRAMSize: Int, private val theDataWidth: Int) extends Bundle {
  val dataPath = new Bundle {
    val inIOs: StreamBitsIO = Flipped(new StreamBitsIO(theDataWidth))
    val outIOs = new StreamBitsIO(theDataWidth)
  }
  val ctrlPath = new SRAMCommonCtrlIO
  val debugIO = new SRAMCommonDebugIO(theSRAMSize, theDataWidth)
}

class InActSRAMBankIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new StreamBitsInOutIO(inActAdrWidth, inActDataWidth)
  val ctrlPath = new SRAMCommonCtrlIO
}

class StreamBitsInOutIO(private val adrWidth: Int, private val dataWidth: Int) extends Bundle {
  val inIOs: CSCStreamIO = Flipped(new CSCStreamIO(adrWidth, dataWidth))
  val outIOs = new CSCStreamIO(adrWidth, dataWidth)
}

class PSumSRAMBankIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new PSumSRAMDataIO(psDataWidth)
  val ctrlPath = new SRAMCommonCtrlIO with WithStratIdxIO
  val debugIO = new SRAMCommonDebugIO(pSumSRAMSize, psDataWidth)
}

class PSumSRAMDataIO(dataWidth: Int) extends Bundle {
  val inIOs: DecoupledIO[UInt] = Flipped(Decoupled(UInt(dataWidth.W)))
  val outIOs: DecoupledIO[UInt] = Decoupled(UInt(dataWidth.W))
}

class GLBClusterIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new GLBClusterDataIO
  val ctrlPath = new GLBClusterCtrlIO
}

class WeightGLBIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new StreamBitsInOutIO(weightAdrWidth, weightDataWidth)
}

class GLBClusterCtrlIO extends Bundle {

}

class GLBClusterDataIO extends Bundle with ClusterSRAMConfig {
  val inActIO: Vec[StreamBitsInOutIO] = Vec(inActSRAMNum, new StreamBitsInOutIO(inActAdrWidth, inActDataWidth))
  val weightIO: Vec[StreamBitsInOutIO] = Vec(weightRouterNum, new StreamBitsInOutIO(weightAdrWidth, weightDataWidth))
  val pSumIO: Vec[PSumSRAMDataIO] = Vec(pSumSRAMNum, new PSumSRAMDataIO(psDataWidth))
}
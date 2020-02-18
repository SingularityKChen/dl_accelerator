package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe.{CSCStreamIO, MCRENFConfig, StreamBitsIO}

class GLBCluster(debug: Boolean) extends Module with ClusterSRAMConfig {
  val io = new GLBClusterIO
  private val iSRAMs: Vec[InActSRAMBankIO] = Vec(inActSRAMNum, Module(new InActSRAMBank).io)
  private val pSRAMs: Vec[PSumSRAMBankIO] = Vec(pSumSRAMNum, Module(new PSumSRAMBank).io)
  // connections of data path
  io.dataPath.weightIO.foreach(x => x.inIOs <> x.outIOs)
  io.dataPath.inActIO.zip(iSRAMs).foreach({ case (topIO, sramIO) => topIO <> sramIO.dataPath})
  io.dataPath.pSumIO.zip(pSRAMs).foreach({ case (topIO, sramIO) => topIO <> sramIO.dataPath})
  // connections of control path
}

class PSumSRAMBank extends Module with ClusterSRAMConfig with MCRENFConfig with GNMFCS2Config {
  private val oneSPadPSum: Int = M0*E*N0*F0 // when read counts this, then stop
  private val oneSRAMPSum: Int = oneSPadPSum * N2*M2*F2 // when write counts this, then finished
  require(oneSRAMPSum <= pSumSRAMSize, s"the size of PSum SRAM is $pSumSRAMSize, " +
    s"which should be larger than the config's requirement, which is $oneSRAMPSum")
  val io = new PSumSRAMBankIO
  private val dataSRAM: SyncReadMem[UInt] = SyncReadMem(pSumSRAMSize, UInt(psDataWidth.W))
  // SRAM read write logic
  // because it will not read and write at the same time
  // so we just need one pair of regs to record process
  private val writeOrRead = RegInit(false.B) // true then "do" means write, false then "do" means read
  private val startIdx = RegInit(0.U(log2Ceil(pSumSRAMSize).W)) // the first do address, from io
  private val doIdxReg: UInt = RegInit(0.U(log2Ceil(pSumSRAMSize).W)) // connect to index
  private val doEnReg = RegInit(false.B)
  private val countIncWire = Wire(Bool())
  // read or write one SPad size partial sums
  // doIdxCount means current number of PSums that has been done
  // when finish, doDone = true.B
  private val (doIdxCount, doDone) = Counter(countIncWire, oneSPadPSum + 1)
  private val doWriteWire = Wire(Bool())
  private val doReadWire = Wire(Bool())
  startIdx := io.ctrlPath.startIdx
  writeOrRead := io.ctrlPath.writeOrRead
  io.ctrlPath.done := doDone
  countIncWire := Mux(writeOrRead, doWriteWire, doReadWire)
  doEnReg := !doDone // FIXME
  doIdxReg := startIdx + doIdxCount
  // write logic
  doWriteWire := io.dataPath.inIOs.valid && doEnReg && writeOrRead
  when (doWriteWire) {
    io.dataPath.inIOs.ready := true.B
    dataSRAM.write(doIdxReg, io.dataPath.inIOs.bits)
  } .otherwise {
    io.dataPath.inIOs.ready := false.B
  }
  // read logic
  private val nextValid = Wire(Bool())
  nextValid := doEnReg && !writeOrRead
  io.dataPath.outIOs.valid := RegNext(nextValid) // reg next, so one cycle later, data will be read out with valid signal
  doReadWire := io.dataPath.outIOs.ready && nextValid
  io.dataPath.outIOs.bits := dataSRAM.read(doIdxReg, doReadWire)
}

class InActSRAMCommon(private val theSRAMSize: Int, private val theDataWidth: Int) extends Module {
  val io = new InACTSRAMCommonIO(theDataWidth)
  private val theSRAM = SyncReadMem(theSRAMSize, UInt(theDataWidth.W))
  private val doMeetZeroWire = Wire(Bool()) // current data equals to zero
  private val doMightFinishedReg = RegInit(false.B) // that's the end of one SPad's data
  private val writeDoneWire = Wire(Bool())
  private val readDoneWire = Wire(Bool())
  // SRAM read write logic
  private val writeOrRead = RegInit(false.B) // true then "do" means write, false then "do" means read
  // we assume that one write cycle won't start until read finishes, i.e., the index can be hold when calculating,
  // then continue reading
  private val doIdxReg: UInt = RegInit(0.U(log2Ceil(theSRAMSize).W))
  private val doEnReg = RegInit(false.B)
  private val doReadWire = Wire(Bool())
  private val writeInData = Wire(UInt(log2Ceil(theSRAMSize).W))
  private val readOutData = Wire(UInt(log2Ceil(theSRAMSize).W))
  private val currentData = Wire(UInt(log2Ceil(theSRAMSize).W))
  private val doWriteWire = Wire(Bool())
  private val doDone: Bool = Wire(Bool()) // meet two continuous zeros
  private val doIdxIncWire = Wire(Bool()) // true, then the index increase
  // TODO: add some logic to check whether one SPad of data is a zero matrix, true then jump
  writeOrRead := io.ctrlPath.writeOrRead
  io.ctrlPath.done := doDone
  io.dataPath.outIOs.data.bits := readOutData
  writeInData := io.dataPath.inIOs.data.bits
  doEnReg := !doDone // FIXME
  doIdxIncWire := Mux(writeOrRead, doWriteWire, doReadWire)
  currentData := Mux(writeOrRead, writeInData, readOutData)
  when (doIdxIncWire) {
    doIdxReg := doIdxReg + 1.U
  }
  // write logic
  doWriteWire := writeOrRead && doEnReg && io.dataPath.inIOs.data.valid
  io.dataPath.inIOs.data.ready := doWriteWire
  when (doWriteWire) {
    theSRAM.write(doIdxReg, io.dataPath.inIOs.data.bits)
  }
  // read logic
  private val nextValid = Wire(Bool())
  nextValid := doEnReg && !writeOrRead
  io.dataPath.outIOs.data.valid := RegNext(nextValid) // reg next, so one cycle later, data will be read out with valid signal
  doReadWire := io.dataPath.outIOs.data.ready && nextValid
  readOutData := theSRAM.read(doIdxReg, doReadWire)
  // do finish?
  doMeetZeroWire := currentData === 0.U
  doMightFinishedReg := doMeetZeroWire && doEnReg // meets one zero, that's the end of one SPad
  // if meet two continuous zeros, then the group of data finishes
  writeDoneWire := doMeetZeroWire && doEnReg && doMightFinishedReg
  readDoneWire := doMightFinishedReg
  doDone := Mux(writeOrRead, writeDoneWire, readDoneWire)
}

class InActSRAMBank extends Module with ClusterSRAMConfig {
  val io = new InActSRAMBankIO
  private val adrSRAM = Module(new InActSRAMCommon(inActAdrSRAMSize, inActAdrWidth)).io
  private val dataSRAM = Module(new InActSRAMCommon(inActDataSRAMSize, inActDataWidth)).io
  // SRAM connections
  adrSRAM.dataPath <> io.dataPath.inIOs.adrIOs
  dataSRAM.dataPath <> io.dataPath.inIOs.dataIOs
}

class InACTSRAMCommonIO(private val dataWidth: Int) extends Bundle {
  val dataPath = new Bundle {
    val inIOs = Flipped(new StreamBitsIO(dataWidth))
    val outIOs = new StreamBitsIO(dataWidth)
  }
  val ctrlPath = new Bundle {
    val writeOrRead: Bool = Input(Bool())
    val done: Bool = Output(Bool())
  }
}

class InActSRAMBankIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new StreamBitsInOutIO(inActAdrWidth, inActDataWidth)
}

class StreamBitsInOutIO(private val adrWidth: Int, private val dataWidth: Int) extends Bundle {
  val inIOs: CSCStreamIO = Flipped(new CSCStreamIO(adrWidth, dataWidth))
  val outIOs = new CSCStreamIO(adrWidth, dataWidth)
}

class PSumSRAMBankIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new PSumSRAMDataIO(psDataWidth)
  val ctrlPath = new Bundle {
    val startIdx: UInt = Input(UInt(log2Ceil(pSumSRAMSize).W))
    val writeOrRead: Bool = Input(Bool())
    val done: Bool = Output(Bool()) // done write or read
  }
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
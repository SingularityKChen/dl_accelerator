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
  private val readIdxReg: UInt = RegInit(0.U(log2Ceil(pSumSRAMSize).W))
  private val readEnReg = RegInit(false.B)
  private val doReadWire = Wire(Bool())
  private val (_, readDone) = Counter(doReadWire, oneSPadPSum + 1)
  private val writeIdxReg: UInt = RegInit(0.U(log2Ceil(pSumSRAMSize).W))
  private val writeEnReg = RegInit(false.B)
  private val (_, writeDone) = Counter(io.dataPath.inIOs.valid && writeEnReg, oneSRAMPSum + 1)
  //writeEnReg :=
  when (io.dataPath.inIOs.valid && !writeDone && writeEnReg) {
    io.dataPath.inIOs.ready := true.B
    dataSRAM.write(writeIdxReg, io.dataPath.inIOs.bits)
    writeIdxReg := writeIdxReg + 1.U
  } .otherwise {
    io.dataPath.inIOs.ready := false.B
  }
  io.dataPath.outIOs.valid := !(io.dataPath.inIOs.valid && writeEnReg) && readEnReg && !readDone
  doReadWire := io.dataPath.outIOs.valid && io.dataPath.outIOs.ready
  dataSRAM.read(readIdxReg, doReadWire)
  when (doReadWire) {
    readIdxReg := readIdxReg + 1.U
  }
}

class InActSRAMCommon(private val theSRAMSize: Int, private val theDataWidth: Int) extends Module {
  val io = new InACTSRAMCommonIO(theDataWidth)
  private val theSRAM = SyncReadMem(theSRAMSize, UInt(theDataWidth.W))
  private val writeMeetZeroWire = Wire(Bool())
  private val writeMightFinishedReg: Bool = RegInit(false.B)
  // SRAM read write logic
  private val readIdxReg: UInt = RegInit(0.U(log2Ceil(theSRAMSize).W))
  private val readEnReg = RegInit(false.B)
  private val doReadWire = Wire(Bool())
  private val writeIdxReg: UInt = RegInit(0.U(log2Ceil(theSRAMSize).W))
  private val writeWrapWire: Bool = Wire(Bool())
  private val writeEnReg = RegInit(false.B)
  writeMeetZeroWire := io.dataPath.data.bits === 0.U
  writeMightFinishedReg :=  writeMeetZeroWire && writeEnReg // meets one zero, that's the end of one SPad
  // if meet two continuous zero, then the group of data finished
  writeWrapWire := writeMeetZeroWire && writeEnReg && writeMightFinishedReg
}

class InActSRAMBank extends Module with ClusterSRAMConfig {
  val io = new InActSRAMBankIO
  private val adrSRAM = Module(new InActSRAMCommon(inActAdrSRAMSize, inActAdrWidth)).io
  private val dataSRAM = Module(new InActSRAMCommon(inActDataSRAMSize, inActDataWidth)).io
  // SRAM read write logic
}

class InACTSRAMCommonIO(private val dataWidth: Int) extends Bundle {
  val dataPath = new StreamBitsIO(dataWidth)
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
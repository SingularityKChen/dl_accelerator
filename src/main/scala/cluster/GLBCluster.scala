package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe.CSCStreamIO

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

class PSumSRAMBank extends Module with ClusterSRAMConfig {
  val io = new PSumSRAMBankIO
  private val dataSRAM: SyncReadMem[UInt] = SyncReadMem(pSumSRAMSize, UInt(psDataWidth.W))
  // SRAM read write logic
  private val readIdxReg: UInt = RegInit(0.U(log2Ceil(pSumSRAMSize).W))
  private val writeIdxReg: UInt = RegInit(0.U(log2Ceil(pSumSRAMSize).W))
}

class InActSRAMBank extends Module with ClusterSRAMConfig {
  val io = new InActSRAMBankIO
  private val adrSRAM: SyncReadMem[UInt] = SyncReadMem(inActAdrSRAMSize,UInt(inActAdrWidth.W))
  private val dataSRAM: SyncReadMem[UInt] = SyncReadMem(inActDataSRAMSize,UInt(inActDataWidth.W))
  // SRAM read write logic
  private val readAdrIdxReg: UInt = RegInit(0.U(log2Ceil(inActAdrSRAMSize).W))
  private val writeAdrIdxReg: UInt = RegInit(0.U(log2Ceil(inActAdrSRAMSize).W))
  private val readDataIdxReg: UInt = RegInit(0.U(log2Ceil(inActAdrSRAMSize).W))
  private val writeDataIdxReg: UInt = RegInit(0.U(log2Ceil(inActAdrSRAMSize).W))
}

class InActSRAMBankIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new StreamBitsInOutIO(inActAdrWidth, inActDataWidth)
}

class StreamBitsInOutIO(adrWidth: Int, dataWidth: Int) extends Bundle {
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
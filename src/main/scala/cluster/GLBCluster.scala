package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe.CSCStreamIO

class GLBCluster(debug: Boolean) extends Module with ClusterSRAMConfig {
  val io = new GLBClusterIO
  val iSRAMs: Vec[IactSRAMBankIO] = Vec(iactSRAMNum, Module(new IactSRAMBank).io)
  val pSRAMs: Vec[PSumSRAMBankIO] = Vec(pSumSRAMNum, Module(new PSumSRAMBank).io)
  io.weightIO.foreach(x => x.dataPath.inIOs <> x.dataPath.outIOs)
  io.iactIO.zip(iSRAMs).foreach({ case (topIO, sramIO) => topIO <> sramIO})
  io.pSumIO.zip(pSRAMs).foreach({ case (topIO, sramIO) => topIO <> sramIO})
}

class PSumSRAMBank extends Module with ClusterSRAMConfig {
  val io = new PSumSRAMBankIO
  val dataSRAM: SyncReadMem[UInt] = SyncReadMem(pSumSRAMSize, UInt(psDataWidth.W))
  // SRAM read write logic
}

class IactSRAMBank extends Module with ClusterSRAMConfig {
  val io = new IactSRAMBankIO
  val addrSRAM: SyncReadMem[UInt] = SyncReadMem(iactAddrSRAMSize,UInt(iactAddrWidth.W))
  val dataSRAM: SyncReadMem[UInt] = SyncReadMem(iactDataSRAMSize,UInt(iactDataWidth.W))
  // SRAM read write logic
}

class IactSRAMBankIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new StreamBitsInOutIO(iactAddrWidth, iactDataWidth)
}

class StreamBitsInOutIO(addrWidth: Int, dataWidth: Int) extends Bundle {
  val inIOs: CSCStreamIO = Flipped(new CSCStreamIO(addrWidth, dataWidth))
  val outIOs = new CSCStreamIO(addrWidth, dataWidth)
}

class PSumSRAMBankIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new PSumSRAMDataIO(psDataWidth)
}

class PSumSRAMDataIO(dataWidth: Int) extends Bundle {
  val inIOs: DecoupledIO[UInt] = Flipped(Decoupled(UInt(dataWidth.W)))
  val outIOs: DecoupledIO[UInt] = Decoupled(UInt(dataWidth.W))
}

class GLBClusterIO extends Bundle with ClusterSRAMConfig {
  val iactIO: Vec[IactSRAMBankIO] = Vec(iactSRAMNum, new IactSRAMBankIO)
  val pSumIO: Vec[PSumSRAMBankIO] = Vec(pSumSRAMNum, new PSumSRAMBankIO)
  val weightIO: Vec[WeightGLBIO] = Vec(weightRouterNum, new WeightGLBIO)
}

class WeightGLBIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new StreamBitsInOutIO(weightAddrWidth, weightDataWidth)
}
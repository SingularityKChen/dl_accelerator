package dla.cluster

import chisel3._
import chisel3.util._

class GLBCluster(debug: Boolean) extends Module with ClusterSRAMConfig {
  val io = new GLBClusterIO
  val iSRAMs: Vec[IactSRAMBankIO] = Vec(iactSRAMNum, Module(new IactSRAMBank).io)
  val pSRAMs: Vec[PSumSRAMBankIO] = Vec(pSumSRAMNum, Module(new PSumSRAMBank).io)
  io.weightIO.dataPath.inIOs <> io.weightIO.dataPath.outIOs
}

class PSumSRAMBank extends Module with ClusterSRAMConfig {
  val io = new PSumSRAMBankIO
  val dataSRAM: SyncReadMem[UInt] = SyncReadMem(pSumSRAMSize, UInt(psDataWidth.W))
}

class IactSRAMBank extends Module with ClusterSRAMConfig {
  val io = new IactSRAMBankIO
  val addrSRAM: SyncReadMem[UInt] = SyncReadMem(iactAddrSRAMSize,UInt(iactAddrWidth.W))
  val dataSRAM: SyncReadMem[UInt] = SyncReadMem(iactDataSRAMSize,UInt(iactDataWidth.W))
}

class IactSRAMBankIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new IactSRAMDataIO(iactAddrWidth, iactDataWidth)
}

class IactSRAMDataIO(addrWidth: Int, dataWidth: Int) extends Bundle {
  val inIOs: ClusterAddrWithDataCommonIO = Flipped(new ClusterAddrWithDataCommonIO(addrWidth, dataWidth))
  val outIOs = new ClusterAddrWithDataCommonIO(addrWidth, dataWidth)
}

class PSumSRAMBankIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new PSumSRAMDataIO(psDataWidth)
}

class PSumSRAMDataIO(dataWidth: Int) extends Bundle {
  val inIOs: DecoupledIO[UInt] = Flipped(Decoupled(UInt(dataWidth.W)))
  val outIOs: DecoupledIO[UInt] = Decoupled(UInt(dataWidth.W))
}

class GLBClusterIO extends Bundle {
  val iactIO = new IactSRAMBankIO
  val pSumIO = new PSumSRAMBankIO
  val weightIO = new WeightGLBIO
}

class WeightGLBIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new IactSRAMDataIO(weightAddrWidth, weightDataWidth)
}
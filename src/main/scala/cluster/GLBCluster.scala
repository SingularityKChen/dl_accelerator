package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe.CSCStreamIO

class GLBCluster(debug: Boolean) extends Module with ClusterSRAMConfig {
  val io = new GLBClusterIO
  val iSRAMs: Vec[IactSRAMBankIO] = Vec(iactSRAMNum, Module(new IactSRAMBank).io)
  val pSRAMs: Vec[PSumSRAMBankIO] = Vec(pSumSRAMNum, Module(new PSumSRAMBank).io)
  // connections of data path
  io.dataPath.weightIO.foreach(x => x.inIOs <> x.outIOs)
  io.dataPath.iactIO.zip(iSRAMs).foreach({ case (topIO, sramIO) => topIO <> sramIO.dataPath})
  io.dataPath.pSumIO.zip(pSRAMs).foreach({ case (topIO, sramIO) => topIO <> sramIO.dataPath})
  // connections of control path
}

class PSumSRAMBank extends Module with ClusterSRAMConfig {
  val io = new PSumSRAMBankIO
  val dataSRAM: SyncReadMem[UInt] = SyncReadMem(pSumSRAMSize, UInt(psDataWidth.W))
  // SRAM read write logic
  val readIdxReg: UInt = RegInit(0.U(log2Ceil(pSumSRAMSize).W))
  val writeIdxReg: UInt = RegInit(0.U(log2Ceil(pSumSRAMSize).W))
}

class IactSRAMBank extends Module with ClusterSRAMConfig {
  val io = new IactSRAMBankIO
  val addrSRAM: SyncReadMem[UInt] = SyncReadMem(iactAddrSRAMSize,UInt(iactAddrWidth.W))
  val dataSRAM: SyncReadMem[UInt] = SyncReadMem(iactDataSRAMSize,UInt(iactDataWidth.W))
  // SRAM read write logic
  val readAddrIdxReg: UInt = RegInit(0.U(log2Ceil(iactAddrSRAMSize).W))
  val writeAddrIdxReg: UInt = RegInit(0.U(log2Ceil(iactAddrSRAMSize).W))
  val readDataIdxReg: UInt = RegInit(0.U(log2Ceil(iactAddrSRAMSize).W))
  val writeDataIdxReg: UInt = RegInit(0.U(log2Ceil(iactAddrSRAMSize).W))
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
  val dataPath = new GLBClusterDataIO
  val ctrlPath = new GLBClusterCtrlIO
}

class WeightGLBIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new StreamBitsInOutIO(weightAddrWidth, weightDataWidth)
}

class GLBClusterCtrlIO extends Bundle {

}

class GLBClusterDataIO extends Bundle with ClusterSRAMConfig {
  val iactIO: Vec[StreamBitsInOutIO] = Vec(iactSRAMNum, new StreamBitsInOutIO(iactAddrWidth, iactDataWidth))
  val weightIO: Vec[StreamBitsInOutIO] = Vec(weightRouterNum, new StreamBitsInOutIO(weightAddrWidth, weightDataWidth))
  val pSumIO: Vec[PSumSRAMDataIO] = Vec(pSumSRAMNum, new PSumSRAMDataIO(psDataWidth))
}
package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe.CSCStreamIO

class ClusterGroupIO extends Bundle {
  val ctrlPath: ClusterGroupConfigIO = Flipped(new ClusterGroupConfigIO)
  val dataPath = new ClusterDataPathIO
}

class ClusterDataPathIO extends Bundle with ClusterConfig {
  val glbDataPath = new RouterDataIO // through top to GLB
  val cgDataPath = new RouterDataIO // cluster group
  // routerPSumToPEIO for translating partial sum from each head in PE array to each tail of its northern PE Array
  val routerPSumToPEIOs = new PSumRouterDataIO(pSumRouterNum, psDataWidth) // input and output
}

class ClusterGroupConfigIO extends Bundle {
  val peClusterCtrl = new CommonClusterCtrlIO[Bool, UInt](Bool(), UInt(2.W))
  val routerClusterCtrl: RouterClusterCtrlIO = Flipped(new RouterClusterCtrlIO) // input
}

class RouterClusterIO extends Bundle {
  val ctrlPath = new RouterCtrlIO
  val dataPath = new RouterClusterDataIO
}

class RouterClusterDataIO extends Bundle with ClusterConfig {
  val routerData = new RouterDataIO
  val routerOutPSumToPEIO: Vec[DecoupledIO[UInt]] = Vec(pSumRouterNum, Decoupled(UInt(psDataWidth.W))) // output only
}

class CommonRouterIO[T1<: Data, T2<:Data](dataType1: T1, dataType2: T2, portNum: Int, addrWidth: Int, dataWidth: Int) extends Bundle {
  val dataPath = new CommonRouterDataIO(portNum, addrWidth, dataWidth)
  val ctrlPath = new CommonClusterCtrlIO[T1, T2](dataType1, dataType2)
}

class PSumRouterIO extends Bundle with ClusterConfig {
  val dataPath = new PSumRouterDataIO(pSumPortNum, psDataWidth)
  val ctrlPath = new CommonClusterCtrlIO[UInt, UInt](UInt(2.W), UInt(2.W))
}

class RouterDataIO extends Bundle with ClusterConfig {
  val iRIO: Vec[CommonRouterDataIO] = Vec(iactRouterNum, new CommonRouterDataIO(iactPortNum, iactAddrWidth, iactDataWidth))
  val wRIO: Vec[CommonRouterDataIO] = Vec(weightRouterNum, new CommonRouterDataIO(weightPortNum, weightAddrWidth, weightDataWidth))
  val pSRIO: Vec[PSumRouterDataIO] = Vec(pSumRouterNum, new PSumRouterDataIO(pSumPortNum, psDataWidth))
}

class RouterCtrlIO extends Bundle with ClusterConfig {
  val iRIO: Vec[CommonClusterCtrlIO[UInt, UInt]] = Vec(iactRouterNum, new CommonClusterCtrlIO[UInt, UInt](UInt(2.W), UInt(2.W)))
  val wRIO: Vec[CommonClusterCtrlIO[Bool, Bool]] = Vec(weightRouterNum, new CommonClusterCtrlIO[Bool, Bool](Bool(), Bool()))
  val pSRIO: Vec[CommonClusterCtrlIO[UInt, UInt]] = Vec(pSumRouterNum, new CommonClusterCtrlIO[UInt, UInt](UInt(2.W), UInt(2.W)))
}

class PSumRouterDataIO(portNum: Int, dataWidth: Int) extends Bundle {
  val inIOs: Vec[DecoupledIO[UInt]] = Vec(portNum, Flipped(Decoupled(UInt(dataWidth.W)))) // input bits and valid
  val outIOs: Vec[DecoupledIO[UInt]] = Vec(portNum, Decoupled(UInt(dataWidth.W))) // output bits and valid
}

class CommonRouterDataIO(portNum: Int, addrWidth: Int, dataWidth: Int) extends Bundle {
  val inIOs: Vec[CSCStreamIO] = Vec(portNum, Flipped(new CSCStreamIO(addrWidth, dataWidth)))
  val outIOs: Vec[CSCStreamIO] = Vec(portNum, new CSCStreamIO(addrWidth, dataWidth))
}

class RouterClusterCtrlIO extends Bundle { // output only
  // TODO: check whether each router needs its own config signals
  // uni-cast, horizontal, vertical, broad-cast
  // iactCtrlSel.inDataSel: 0 for GLB Cluster, 1 for north, 2 for south, 3 for horizontal
  // iactCtrlSel.outDataSel 0 for PE Cluster, 1 for north, 2 for south, 3 for horizontal
  val iactCtrlSel: CommonClusterCtrlIO[UInt, UInt] = new CommonClusterCtrlIO[UInt, UInt](UInt(2.W), UInt(2.W))
  // Weight Models: broad-cast, multi-cast, uni-cast, but the first two seems the same inner weight cluster
  // weightCtrlSel.inDataSel: true for broad-cast and multi-cast, false for uni-cast
  // weightCtrlSel.outDataSel: 0, send the data to PE Cluster; 1, send it to its neighboring WeightRouter and PE Cluster
  val weightCtrlSel: CommonClusterCtrlIO[Bool, Bool] = new CommonClusterCtrlIO[Bool, Bool](Bool(), Bool())
  // pSumCtrlSel.inDataSel: 0 for PE Cluster, 1 for GLB Cluster, 2 for vertical
  // pSumCtrlSel.outDataSel: 0 for PE Cluster, 1 for GLB Cluster, 2 for vertical
  val pSumCtrlSel: CommonClusterCtrlIO[UInt, UInt] = Flipped(new CommonClusterCtrlIO[UInt, UInt](UInt(2.W), UInt(2.W)))
}

class PEClusterIO extends Bundle with ClusterConfig {
  val dataPath = new PEClusterDataIO
  val ctrlPath: PEClusterCtrlIO = Flipped(new PEClusterCtrlIO) // input
}

class PEClusterDataIO extends Bundle with ClusterConfig {
  val iactIO: Vec[CSCStreamIO] = Vec(iactRouterNum, Flipped(new CSCStreamIO(iactAddrWidth, iactDataWidth))) // input only
  val weightIO: Vec[CSCStreamIO] = Vec(weightRouterNum, Flipped(new CSCStreamIO(weightAddrWidth, weightDataWidth)))
  val pSumIO = new PSumRouterDataIO(pSumRouterNum, psDataWidth) // input and output
  val routerInPSumToPEIO: Vec[DecoupledIO[UInt]] = Vec(pSumRouterNum, Flipped(Decoupled(UInt(psDataWidth.W)))) // input only
}

class PEClusterCtrlIO extends Bundle { // output only
  // iactCtrlSel.inDataSel: true for broad-cast, false for others
  // iactCtrlSel.outDataSel: the value indicates the index of router
  val iactCtrlSel = new CommonClusterCtrlIO[Bool, UInt](Bool(), UInt(2.W))
  // val weightCtrlSel = new CommonClusterCtrlIO[Bool, Bool](Bool(), Bool()) // do not need this
  // pSumCtrlSel.inDataSel: true, then receive data from PSumRouter, false then receive data from its southern PE Array
  // pSumCtrlSel.outDataSel: unused
  val pSumCtrlSel = new CommonClusterCtrlIO[Bool, Bool](Bool(), Bool())
  pSumCtrlSel.outDataSel := DontCare
}

class CommonClusterCtrlIO[T1<: Data, T2<: Data](dataType1: T1, dataType2: T2) extends Bundle {
  val inDataSel: T1 = Input(dataType1)
  val outDataSel: T2 = Input(dataType2)
}

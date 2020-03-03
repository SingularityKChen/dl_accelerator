package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe.CSCStreamIO

class ClusterGroupIO extends Bundle {
  val ctrlPath: ClusterGroupCtrlIO = Flipped(new ClusterGroupCtrlIO)
  val dataPath = new ClusterDataIO
}

class ClusterDataIO extends Bundle with ClusterConfig {
  val glbDataPath = new GLBClusterDataIO // through top to GLB
  val cgDataPath = new RouterDataIO // communicate with other cluster groups
  // routerPSumToPEIO for translating partial sum from each head in PE array to each tail of its northern PE Array
  val routerPSumToPEIOs = new PSumRouterDataIO(pSumRouterNum, psDataWidth) // input and output
}

class ClusterGroupCtrlIO extends Bundle {
  val peClusterCtrl: CommonClusterCtrlBoolUIntIO = Flipped(new CommonClusterCtrlBoolUIntIO) // input select signals to PE Cluster
  val routerClusterCtrl: RouterClusterCtrlIO = Flipped(new RouterClusterCtrlIO) // input select signals to Router Cluster
}

class RouterClusterIO extends Bundle {
  val ctrlPath: RouterCtrlIO = Flipped(new RouterCtrlIO) // input
  val dataPath = new RouterClusterDataIO
}

class RouterClusterDataIO extends Bundle with ClusterConfig {
  val routerData = new RouterDataIO
  val routerOutPSumToPEIO: Vec[DecoupledIO[UInt]] = Vec(pSumRouterNum, Decoupled(UInt(psDataWidth.W))) // output only
}

class CommonRouterBoolIO(val portNum: Int, val adrWidth: Int, val dataWidth: Int) extends Bundle {
  val dataPath = new CommonRouterDataIO(portNum, adrWidth, dataWidth)
  val ctrlPath: CommonClusterCtrlTwoBoolIO = Flipped(new CommonClusterCtrlTwoBoolIO)
}

class CommonRouterUIntIO(val portNum: Int, val adrWidth: Int, val dataWidth: Int) extends Bundle {
  val dataPath = new CommonRouterDataIO(portNum, adrWidth, dataWidth)
  val ctrlPath: CommonClusterCtrlTwoUIntIO = Flipped(new CommonClusterCtrlTwoUIntIO)
}

class PSumRouterIO extends Bundle with ClusterConfig {
  val dataPath = new PSumRouterDataIO(pSumPortNum, psDataWidth)
  val ctrlPath: CommonClusterCtrlTwoUIntIO = Flipped(new CommonClusterCtrlTwoUIntIO)
}

class RouterDataIO extends Bundle with ClusterConfig {
  val iRIO: Vec[CommonRouterDataIO] = Vec(inActRouterNum, new CommonRouterDataIO(inActPortNum, inActAdrWidth, inActDataWidth))
  val wRIO: Vec[CommonRouterDataIO] = Vec(weightRouterNum, new CommonRouterDataIO(weightPortNum, weightAdrWidth, weightDataWidth))
  val pSumRIO: Vec[PSumRouterDataIO] = Vec(pSumRouterNum, new PSumRouterDataIO(pSumPortNum, psDataWidth))
}

class RouterCtrlIO extends Bundle with ClusterConfig {
  val iRIO: Vec[CommonClusterCtrlTwoUIntIO] = Vec(inActRouterNum, new CommonClusterCtrlTwoUIntIO)
  val wRIO: Vec[CommonClusterCtrlTwoBoolIO] = Vec(weightRouterNum, new CommonClusterCtrlTwoBoolIO)
  val pSumRIO: Vec[CommonClusterCtrlTwoUIntIO] = Vec(pSumRouterNum, new CommonClusterCtrlTwoUIntIO)
}

class PSumRouterDataIO(val portNum: Int, val dataWidth: Int) extends Bundle {
  val inIOs: Vec[DecoupledIO[UInt]] = Vec(portNum, Flipped(Decoupled(UInt(dataWidth.W)))) // input bits and valid
  val outIOs: Vec[DecoupledIO[UInt]] = Vec(portNum, Decoupled(UInt(dataWidth.W))) // output bits and valid
}

class CommonRouterDataIO(val portNum: Int, val adrWidth: Int, val dataWidth: Int) extends Bundle {
  val inIOs: Vec[CSCStreamIO] = Vec(portNum, Flipped(new CSCStreamIO(adrWidth, dataWidth)))
  val outIOs: Vec[CSCStreamIO] = Vec(portNum, new CSCStreamIO(adrWidth, dataWidth))
}

class RouterClusterCtrlIO extends Bundle { // output only
  // TODO: check whether each router needs its own config signals
  // uni-cast, horizontal, vertical, broad-cast
  // inActCtrlSel.inDataSel: 0 for GLB Cluster, 1 for north, 2 for south, 3 for horizontal
  // inActCtrlSel.outDataSel 0 for PE Cluster, 1 for north, 2 for south, 3 for horizontal
  val inActCtrlSel = new CommonClusterCtrlTwoUIntIO
  // Weight Models: broad-cast, multi-cast, uni-cast, but the first two seems the same inner weight cluster
  // weightCtrlSel.inDataSel: true for broad-cast and multi-cast, false for uni-cast
  // weightCtrlSel.outDataSel: 0, send the data to PE Cluster; 1, send it to its neighboring WeightRouter and PE Cluster
  val weightCtrlSel = new CommonClusterCtrlTwoBoolIO
  // pSumCtrlSel.inDataSel: 0 for PE Cluster, 1 for GLB Cluster, 2 for vertical
  // pSumCtrlSel.outDataSel: 0 for PE Cluster, 1 for GLB Cluster, 2 for vertical
  val pSumCtrlSel = new CommonClusterCtrlTwoUIntIO
}

class PEClusterIO extends Bundle with ClusterConfig {
  val dataPath = new PEClusterDataIO
  val ctrlPath: PEClusterCtrlIO = Flipped(new PEClusterCtrlIO) // input
}

class PEClusterDataIO extends Bundle with ClusterConfig {
  val inActIO: Vec[CSCStreamIO] = Vec(inActRouterNum, Flipped(new CSCStreamIO(inActAdrWidth, inActDataWidth))) // input only
  val weightIO: Vec[CSCStreamIO] = Vec(weightRouterNum, Flipped(new CSCStreamIO(weightAdrWidth, weightDataWidth)))
  val pSumIO = new PSumRouterDataIO(pSumRouterNum, psDataWidth) // input and output
  val routerInPSumToPEIO: Vec[DecoupledIO[UInt]] = Vec(pSumRouterNum, Flipped(DecoupledIO(UInt(psDataWidth.W)))) // input only
}

class PEClusterCtrlIO extends Bundle { // output only
  // inActCtrlSel.inDataSel: true for broad-cast, false for others
  // inActCtrlSel.outDataSel: the value indicates the index of router
  val inActCtrlSel = new CommonClusterCtrlBoolUIntIO
  // val weightCtrlSel = new CommonClusterCtrlIO[Bool, Bool](Bool(), Bool()) // do not need this
  // pSumCtrlSel.inDataSel: true, then receive data from PSumRouter, false then receive data from its southern PE Array
  // pSumCtrlSel.outDataSel: unused
  val pSumCtrlSel = new CommonClusterCtrlTwoBoolIO
  val doEn: Bool = Output(Bool())
  val configIOs: Vec[UInt] = Output(Vec(6, UInt(3.W))) // that's GNMFCS
}

class CommonClusterCtrlTwoUIntIO extends Bundle {
  val inDataSel: UInt = Output(UInt(2.W))
  val outDataSel: UInt = Output(UInt(2.W))
}

class CommonClusterCtrlBoolUIntIO extends Bundle {
  val inDataSel: UInt = Output(Bool())
  val outDataSel: UInt = Output(UInt(2.W))
}

class CommonClusterCtrlTwoBoolIO extends Bundle {
  val inDataSel: Bool = Output(Bool())
  val outDataSel: Bool = Output(Bool())
}

trait BusySignal extends Bundle {
  val busy: Bool = Output(Bool())
}
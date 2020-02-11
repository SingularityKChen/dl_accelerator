package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe.CSCStreamIO

class ClusterGroupConfigIO extends Bundle {
  val peClusterCtrl = new ClusterCommonCtrlIO[Bool, UInt](Bool(), UInt(2.W))
  val routerClusterCtrl: RouterClusterCtrlIO = Flipped(new RouterClusterCtrlIO)
}

class RouterClusterCtrlIO extends Bundle { // TODO: check whether each router needs its own config signals
  val iactCtrlSel = new ClusterCommonCtrlIO[UInt, UInt](UInt(2.W), UInt(2.W))
  val weightCtrlSel = new ClusterCommonCtrlIO[Bool, Bool](Bool(), Bool())
  val pSumCtrlSel = new ClusterCommonCtrlIO[UInt, UInt](UInt(2.W), UInt(2.W))
}

class PEAndPSumCluster extends Bundle with ClusterConfig {
  val outIOs = new PSumClusterIO(pSumRouterNum, psDataWidth)
  val inIOs: PSumClusterIO = Flipped(new PSumClusterIO(pSumRouterNum * 2, psDataWidth))
}

class IactRouterIO extends Bundle with ClusterConfig {
  val outIOs = new IactClusterIO(UInt(2.W), UInt(2.W), iactPortNum, iactAddrWidth, iactDataWidth)
  val inIOs: IactClusterIO[UInt, UInt] = Flipped(new IactClusterIO(UInt(2.W), UInt(2.W), iactPortNum, iactAddrWidth, iactDataWidth))
}

class WeightRouterIO extends Bundle with ClusterConfig {
  val outIOs = new WeightClusterIO(weightPortNum, weightAddrWidth, weightDataWidth)
  val inIOs: WeightClusterIO = Flipped(new WeightClusterIO(weightPortNum, weightAddrWidth, weightDataWidth)) // input bits and valid, routerMode
}

class PSumRouterIO extends Bundle with ClusterConfig {
  val outIOs = new PSumClusterIO(pSumPortNum, psDataWidth)
  val inIOs: PSumClusterIO = Flipped(new PSumClusterIO(pSumPortNum, psDataWidth))
}

class IactClusterIO[T1<: Data, T2<:Data](dataType1: T1, dataType2: T2, portNum: Int, addrWidth: Int, dataWidth: Int) extends Bundle {
  val dataPath: Vec[CSCStreamIO] = Vec(portNum, new CSCStreamIO(addrWidth, dataWidth)) // output bits and valid
  val ctrlPath = new ClusterCommonCtrlIO[T1, T2](dataType1, dataType2)
  // uni-cast, horizontal, vertical, broad-cast
  // ctrlPath.inDataSel:
  //   in Router:UInt 0  for GLB Cluster, 1 for north, 2 for south, 3 for horizontal
  //   in PE Cluster:Bool true for broad-cast, false for others
  // ctrlPath.outDataSel:
  //   in Router:UInt 0 for PE Cluster, 1 for north, 2 for south, 3 for horizontal
  //   in PE Cluster:UInt the value indicates the index of router
}

class WeightClusterIO(portNum: Int, addrWidth: Int, dataWidth: Int) extends Bundle {
  val dataPath: Vec[CSCStreamIO] = Vec(portNum, new CSCStreamIO(addrWidth, dataWidth)) // output bits and valid
  val ctrlPath = new ClusterCommonCtrlIO[Bool, Bool](Bool(), Bool())
  // broad-cast, multi-cast, uni-cast, but the first two seems the same inner weight cluster
  // ctrlPath.inDataSel: true for broad-cast and multi-cast, false for uni-cast
  // ctrlPath.outDataSel: 0, send the data to PE Cluster; 1, send it to its neighboring WeightRouter and PE Cluster
}

class PSumClusterIO(portNum: Int, dataWidth: Int) extends Bundle {
  val dataPath: Vec[DecoupledIO[UInt]] = Vec(portNum, Decoupled(UInt(dataWidth.W))) // output bits and valid
  val ctrlPath = new ClusterCommonCtrlIO[UInt, UInt](UInt(2.W), UInt(2.W))
  // ctrlPath.inDataSel: 0 for PE Cluster, 1 for GLB Cluster, 2 for vertical
  // ctrlPath.outDataSel: 0 for PE Cluster, 1 for GLB Cluster, 2 for vertical
}

class ClusterCommonCtrlIO[T1<: Data, T2<: Data](dataType1: T1, dataType2: T2) extends Bundle {
  val inDataSel: T1 = Output(dataType1)
  val outDataSel: T2 = Output(dataType2)
}

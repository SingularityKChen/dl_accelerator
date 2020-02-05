package dla.cluster

import chisel3._
import chisel3.util._

class ClusterGroupConfigIO extends Bundle {
  val peClusterCtrl = new ClusterCommonCtrlIO[Bool, UInt](Bool(), UInt(2.W))
  //val routerClusterCtrl =
}

class PEAndPSumCluster extends Bundle with ClusterConfig {
  val outIOs = new PSumClusterIO(pSumRouterNum, psDataWidth)
  val inIOs: PSumClusterIO = Flipped(new PSumClusterIO(pSumRouterNum * 2, psDataWidth))
}

class IactRouterIO extends Bundle with ClusterConfig {
  val outIOs = new IactClusterIO(UInt(2.W), UInt(2.W), iactPortNum, iactAddrWidth, iactDataWidth, commonLenWidth, commonLenWidth)
  val inIOs: IactClusterIO[UInt, UInt] = Flipped(new IactClusterIO(UInt(2.W), UInt(2.W), iactPortNum, iactAddrWidth, iactDataWidth, commonLenWidth, commonLenWidth))
}

class WeightRouterIO extends Bundle with ClusterConfig {
  val outIOs = new WeightClusterIO(weightPortNum, weightAddrWidth, weightDataWidth, commonLenWidth, weightDataLenWidth)
  val inIOs: WeightClusterIO = Flipped(new WeightClusterIO(weightPortNum, weightAddrWidth, weightDataWidth, commonLenWidth, weightDataLenWidth)) // output bits and valid, routerMode
}

class PSumRouterIO extends Bundle with ClusterConfig {
  val outIOs = new PSumClusterIO(pSumPortNum, psDataWidth)
  val inIOs: PSumClusterIO = Flipped(new PSumClusterIO(pSumPortNum, psDataWidth))
}

class IactClusterIO[T1, T2](dataType1: T1, dataType2: T2, portNum: Int, addrWidth: Int, dataWidth: Int, addrLenWidth: Int, dataLenWidth: Int) extends Bundle {
  val dataPath: Vec[ClusterAddrWithDataCommonIO] = Vec(portNum, new ClusterAddrWithDataCommonIO(addrWidth, dataWidth, addrLenWidth, dataLenWidth)) // output bits and valid
  val ctrlPath = new ClusterCommonCtrlIO[T1, T2](dataType1, dataType2)
  // uni-cast, horizontal, vertical, broad-cast
  // ctrlPath.inDataSel:
  //   in Router:UInt 0  for GLB Cluster, 1 for north, 2 for south, 3 for horizontal
  //   in PE Cluster:Bool true for broad-cast, false for others
  // ctrlPath.outDataSel:
  //   in Router:UInt 0 for PE Cluster, 1 for north, 2 for south, 3 for horizontal
  //   in PE Cluster:UInt the value indicates the index of router
}

class WeightClusterIO(portNum: Int, addrWidth: Int, dataWidth: Int, addrLenWidth: Int, dataLenWidth: Int) extends Bundle {
  val dataPath: Vec[ClusterAddrWithDataCommonIO] = Vec(portNum, new ClusterAddrWithDataCommonIO(addrWidth, dataWidth, addrLenWidth, dataLenWidth)) // output bits and valid
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

class ClusterCommonCtrlIO[T1, T2](dataType1: T1, dataType2: T2) extends Bundle {
  val inDataSel: T1 = Output(dataType1)
  val outDataSel: T2 = Output(dataType2)
}

class ClusterAddrWithDataCommonIO(addrWidth: Int, dataWidth: Int, addrLenWidth: Int, dataLenWidth: Int) extends Bundle {
  val addrIOs = new ClusterDataWithLenCommonIO(addrWidth, addrLenWidth) // output bits and valid
  val dataIOs = new ClusterDataWithLenCommonIO(dataWidth, dataLenWidth)
}

class ClusterDataWithLenCommonIO(dataWidth: Int, dataLenWidth: Int) extends Bundle {
  val dataIO: DecoupledIO[UInt] = Decoupled(UInt(dataWidth.W)) // output bits and valid
  val dataLenIO: UInt = Input(UInt(dataLenWidth.W))
}

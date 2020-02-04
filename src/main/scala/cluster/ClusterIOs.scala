package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe._

class PEAndRouterIO extends Bundle with ClusterConfig {
  val iactCluster: IactClusterIO = Flipped(new IactClusterIO(iactRouterNum, iactAddrWidth, iactDataWidth, commonLenWidth, commonLenWidth))
  val weightCluster: WeightClusterIO = Flipped(new WeightClusterIO(weightRouterNum, weightAddrWidth, weightDataWidth, commonLenWidth, weightDataLenWidth))
  val pSUmCluster: PEAndPSumCluster = Flipped(new PEAndPSumCluster)
}

class PEAndPSumCluster extends Bundle with ClusterConfig {
  val outIOs = new PSumClusterIO(pSumRouterNum, psDataWidth)
  val inIOs: PSumClusterIO = Flipped(new PSumClusterIO(pSumRouterNum * 2, psDataWidth))
}

class IactRouterIO extends Bundle with ClusterConfig {
  val outIOs = new IactClusterIO(iactPortNum, iactAddrWidth, iactDataWidth, commonLenWidth, commonLenWidth)
  val inIOs: IactClusterIO = Flipped(new IactClusterIO(iactPortNum, iactAddrWidth, iactDataWidth, commonLenWidth, commonLenWidth))
}

class WeightRouterIO extends Bundle with ClusterConfig {
  val outIOs = new WeightClusterIO(weightPortNum, weightAddrWidth, weightDataWidth, commonLenWidth, weightDataLenWidth)
  val inIOs: WeightClusterIO = Flipped(new WeightClusterIO(weightPortNum, weightAddrWidth, weightDataWidth, commonLenWidth, weightDataLenWidth)) // output bits and valid, routerMode
}

class PSumRouterIO extends Bundle with ClusterConfig {
  val outIOs = new PSumClusterIO(pSumPortNum, psDataWidth)
  val inIOs: PSumClusterIO = Flipped(new PSumClusterIO(pSumPortNum, psDataWidth))
}

class IactClusterIO(portNum: Int, addrWidth: Int, dataWidth: Int, addrLenWidth: Int, dataLenWidth: Int) extends Bundle {
  val dataPath: Vec[ClusterAddrWithDataCommonIO] = Vec(portNum, new ClusterAddrWithDataCommonIO(addrWidth, dataWidth, addrLenWidth, dataLenWidth)) // output bits and valid
  val ctrlPath = new IactClusterCtrlIO
}

class WeightClusterIO(portNum: Int, addrWidth: Int, dataWidth: Int, addrLenWidth: Int, dataLenWidth: Int) extends Bundle {
  val dataPath: Vec[ClusterAddrWithDataCommonIO] = Vec(portNum, new ClusterAddrWithDataCommonIO(addrWidth, dataWidth, addrLenWidth, dataLenWidth)) // output bits and valid
  val ctrlPath = new WeightClusterCtrlIO
}

class PSumClusterIO(portNum: Int, dataWidth: Int) extends Bundle {
  val dataPath: Vec[DecoupledIO[UInt]] = Vec(portNum, Decoupled(UInt(dataWidth.W))) // output bits and valid
  val ctrlPath = new PSumClusterCtrlIO
}

class PSumClusterCtrlIO extends Bundle with ClusterConfig {
  val inDataSel: UInt = Output(UInt(2.W)) // 0 for PE Cluster, 1 for GLB Cluster, 2 for vertical
  val outDataSel: UInt = Output(UInt(2.W)) // 0 for PE Cluster, 1 for GLB Cluster, 2 for vertical
}

class WeightClusterCtrlIO extends Bundle {
  // broad-cast, multi-cast, uni-cast, but the first two seems the same inner weight cluster
  val inDataSel: Bool = Output(Bool()) // true for broad-cast and multi-cast, false for uni-cast
  val outDataSel: Bool = Output(Bool()) // 0, send the data to PE Cluster; 1, send it to its neighboring WeightRouter and PE Cluster
}

class IactClusterCtrlIO extends Bundle with ClusterConfig {
  // uni-cast, horizontal, vertical, broad-cast
  val inDataSel: UInt = Output(UInt(2.W)) // 0  for GLB Cluster, 1 for north, 2 for south, 3 for horizontal
  val outDataSel: UInt = Output(UInt(2.W)) // 0 for PE Cluster, 1 for north, 2 for south, 3 for horizontal
}

class ClusterAddrWithDataCommonIO(addrWidth: Int, dataWidth: Int, addrLenWidth: Int, dataLenWidth: Int) extends Bundle {
  val addrIOs = new ClusterDataWithLenCommonIO(addrWidth, addrLenWidth) // output bits and valid
  val dataIOs = new ClusterDataWithLenCommonIO(dataWidth, dataLenWidth)
}

class ClusterDataWithLenCommonIO(dataWidth: Int, dataLenWidth: Int) extends Bundle {
  val dataIO: DecoupledIO[UInt] = Decoupled(UInt(dataWidth.W)) // output bits and valid
  val dataLenIO: UInt = Input(UInt(dataLenWidth.W))
}

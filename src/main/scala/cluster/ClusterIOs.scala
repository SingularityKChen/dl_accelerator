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
  //val ctrlPath
}

class PSumClusterIO(portNum: Int, dataWidth: Int) extends Bundle {
  val dataPath: Vec[DecoupledIO[UInt]] = Vec(portNum, Decoupled(UInt(dataWidth.W))) // output bits and valid
  //val ctrlPath =
}

class IactClusterCtrlIO extends Bundle {
  val routingMode: UInt = Output(UInt(2.W)) // unicast, horizontal, vertical, broadcast
}

class ClusterAddrWithDataCommonIO(addrWidth: Int, dataWidth: Int, addrLenWidth: Int, dataLenWidth: Int) extends Bundle {
  val addrIOs = new ClusterDataWithLenCommonIO(addrWidth, addrLenWidth) // output bits and valid
  val dataIOs = new ClusterDataWithLenCommonIO(dataWidth, dataLenWidth)
}

class ClusterDataWithLenCommonIO(dataWidth: Int, dataLenWidth: Int) extends Bundle {
  val dataIO: DecoupledIO[UInt] = Decoupled(UInt(dataWidth.W)) // output bits and valid
  val dataLenIO: UInt = Input(UInt(dataLenWidth.W))
}

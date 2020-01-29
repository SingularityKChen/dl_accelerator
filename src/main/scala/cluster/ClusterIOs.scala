package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe._

class PEAndRouterIO extends Bundle with ClusterConfig {
  val iactCluster: Vec[IactClusterIO] = Vec(iactRouterNum, new IactClusterIO(iactRouterNum, iactAddrWidth, iactDataWidth, commonLenWidth, commonLenWidth))
  val weightCluster: Vec[WeightClusterIO] = Vec(weightRouterNum, new WeightClusterIO(weightRouterNum, weightAddrWidth, weightDataWidth, commonLenWidth, weightDataLenWidth))
  val pSUmCluster: Vec[PSumClusterIO] = Vec(pSumRouterNum, new PSumClusterIO(pSumRouterNum, psDataWidth))
}

class IactClusterIO(portNum: Int, addrWidth: Int, dataWidth: Int, addrLenWidth: Int, dataLenWidth: Int) extends Bundle {
  val dataPath: ClusterAddrWithDataCommonIO = Flipped(new ClusterAddrWithDataCommonIO(portNum, addrWidth, dataWidth)) // input bits and valid
  val ctrlPath = new IactClusterCtrlIO(portNum, addrLenWidth, dataLenWidth)
}

class WeightClusterIO(portNum: Int, addrWidth: Int, dataWidth: Int, addrLenWidth: Int, dataLenWidth: Int) extends Bundle {
  val dataPath: ClusterAddrWithDataCommonIO = Flipped(new ClusterAddrWithDataCommonIO(portNum, addrWidth, dataWidth)) // input bits and valid
  val ctrlPath = new ClusterCtrlCommonIO(portNum, addrLenWidth, dataLenWidth)
}

class PSumClusterIO(portNum: Int, dataWidth: Int) extends Bundle {
  val dataPath: Vec[DecoupledIO[UInt]] = Flipped(Vec(portNum, Decoupled(UInt(dataWidth.W)))) // input bits and valid
  //val ctrlPath =
}

class IactClusterCtrlIO(portNum: Int, addrLenWidth: Int, dataLenWidth: Int) extends ClusterCtrlCommonIO(portNum, addrLenWidth, dataLenWidth) {
  val routingMode: UInt = Input(UInt(2.W)) // unicast, horizontal, vertical, broadcast
}

class ClusterAddrWithDataCommonIO(portNum: Int, addrWidth: Int, dataWidth: Int) extends Bundle {
  val addrIO: Vec[DecoupledIO[UInt]] = Vec(portNum, Decoupled(UInt(dataWidth.W))) // output bits and valid
  val dataIO: Vec[DecoupledIO[UInt]] = Vec(portNum, Decoupled(UInt(dataWidth.W)))
}

class ClusterCtrlCommonIO(portNum: Int, addrLenWidth: Int, dataLenWidth: Int) extends Bundle {
  val addrLenIO: Vec[UInt] = Vec(portNum, Input(UInt(addrLenWidth.W)))
  val dataLenIO: Vec[UInt] = Vec(portNum, Input(UInt(dataLenWidth.W)))
}

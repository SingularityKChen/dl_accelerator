package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe._

class PEAndRouterIO extends Bundle with ClusterConfig {
  val iactCluster = Vec(iactRouterNum, new IactClusterIO(iactPortNum, iactAddrWidth, iactDataWidth))
  val weightCluster = Vec(weightRouterNum, new WeightClusterIO(weightPortNum, weightAddrWidth, weightDataWidth))
  val pSUmCluster = Vec(pSumRouterNum, new PSumClusterIO(pSumPortNum, psDataWidth))
}

class IactClusterIO(portNum: Int, addrWidth: Int, dataWidth: Int) extends Bundle {
  val dataPath = new IactClusterDataIO(portNum, addrWidth, dataWidth)
  //val ctrlPath =
}

class WeightClusterIO(portNum: Int, addrWidth: Int, dataWidth: Int) extends Bundle {
  val dataPath = new ClusterAddrWithDataCommonIO(portNum, addrWidth, dataWidth)
  //val ctrlPath =
}

class PSumClusterIO(portNum: Int, dataWidth: Int) extends Bundle {
  val dataPath: Vec[DecoupledIO[UInt]] = Vec(portNum, Decoupled(UInt(dataWidth.W)))
  //val ctrlPath =
}

class IactClusterDataIO(portNum: Int, addrWidth: Int, dataWidth: Int) extends ClusterAddrWithDataCommonIO(portNum, addrWidth, dataWidth) {
  val routingMode: UInt = Input(UInt(2.W)) // unicast, horizontal, vertical, broadcast
}

class ClusterAddrWithDataCommonIO(portNum: Int, addrWidth: Int, dataWidth: Int) extends Bundle {
  val addrIO: Vec[DecoupledIO[UInt]] = Vec(portNum, Decoupled(UInt(dataWidth.W)))
  val dataIO: Vec[DecoupledIO[UInt]] = Vec(portNum, Decoupled(UInt(dataWidth.W)))
}


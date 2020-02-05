package dla.cluster

import chisel3._
import chisel3.util._

class RouterCluster(debug: Boolean) extends Module with ClusterConfig {
  val io = IO(new Bundle {
    val iRIO: Vec[IactRouterIO] = Vec(iactRouterNum, new IactRouterIO)
    val wRIO: Vec[WeightRouterIO] = Vec(weightRouterNum, new WeightRouterIO)
    val pSRIO: Vec[PSumRouterIO] = Vec(pSumRouterNum, new PSumRouterIO)
    val routerPSumToPEIO: Vec[DecoupledIO[UInt]] = Vec(pSumRouterNum, Output(Decoupled(UInt(psDataWidth.W))))
  })
  val iRouters: Vec[IactRouterIO] = Vec(iactRouterNum, Module(new IactRouter).io)
  val wRouters: Vec[WeightRouterIO] = Vec(weightRouterNum, Module(new WeightRouter).io)
  val pSRouters: Vec[PSumRouterIO] = Vec(pSumRouterNum, Module(new PSumRouter).io)
  io.iRIO.zip(iRouters).foreach({case (x, y) => x <> y})
  io.wRIO.zip(wRouters).foreach({case (x, y) => x <> y})
  io.pSRIO.zip(pSRouters).foreach({case (x, y) => x <> y})
  io.routerPSumToPEIO.zip(pSRouters).foreach({ case (x, y) => x <> y.outIOs.dataPath.head})
}

class IactRouter extends Module with ClusterConfig {
  val io = new IactRouterIO
  val inSelWire: UInt = Wire(UInt(2.W)) // 0  for GLB Cluster, 1 for north, 2 for south, 3 for horizontal
  // router model: 0: uni-cast, 1: horizontal, 2: vertical, 3: broadcast
  val outSelWire: UInt = Wire(UInt(2.W)) // 0 for PE Cluster, 1 for north, 2 for south, 3 for horizontal
  val inDataWire: ClusterAddrWithDataCommonIO = Wire(new ClusterAddrWithDataCommonIO(iactAddrWidth, iactDataWidth, commonLenWidth, commonLenWidth))
  val outDataWire: ClusterAddrWithDataCommonIO = Wire(new ClusterAddrWithDataCommonIO(iactAddrWidth, iactDataWidth, commonLenWidth, commonLenWidth))
  inDataWire <> outDataWire
  inDataWire <> MuxLookup(inSelWire, 0.U, io.inIOs.dataPath.zipWithIndex.map({
    case (o, i) =>
      i.asUInt -> o
  }))
  switch (outSelWire) {
    is (0.U) { // uni-cast
      io.outIOs.dataPath(0) <> outDataWire // 0 to PE array
      io.outIOs.dataPath.takeRight(3).foreach(_ <> DontCare)
    }
    is (1.U) { // horizontal
      io.outIOs.dataPath(0) <> outDataWire
      io.outIOs.dataPath(1) <> DontCare // not send this time
      io.outIOs.dataPath(2) <> DontCare // not send this time
      io.outIOs.dataPath(3) <> outDataWire
    }
    is (2.U) { // vertical
      io.outIOs.dataPath.take(3).foreach(_ <> outDataWire)
      io.outIOs.dataPath(3) <> DontCare// not send this time
    }
    is (3.U) { // broadcast
      io.outIOs.dataPath.foreach(_ <> outDataWire)
    }
  }
  // control path
  inSelWire := io.outIOs.ctrlPath.inDataSel
  outSelWire := io.outIOs.ctrlPath.outDataSel
}

class WeightRouter extends Module with ClusterConfig {
  val io = new WeightRouterIO
  // inSelWire: 0, receive the data come from GLB Cluster; 1, receive it come from its neighborhood WeightRouter
  val inSelWire: Bool = Wire(Bool())
  // outSelWire: 0, send the data to PE Cluster; 1, send it to its neighborhood WeightRouter and PE Cluster
  val outSelWire: Bool = Wire(Bool())
  val inDataWire: ClusterAddrWithDataCommonIO = Wire(new ClusterAddrWithDataCommonIO(weightAddrWidth, weightDataWidth, commonLenWidth, weightDataLenWidth))
  val outDataWire: ClusterAddrWithDataCommonIO = Wire(new ClusterAddrWithDataCommonIO(weightAddrWidth, weightDataWidth, commonLenWidth, weightDataLenWidth))
  inDataWire <> outDataWire
  inDataWire <> Mux(inSelWire, io.inIOs.dataPath(1), io.inIOs.dataPath.head)
  io.outIOs.dataPath.head <> outDataWire
  io.outIOs.dataPath(1) <> Mux(outSelWire, outDataWire, DontCare)
  io.outIOs.ctrlPath <> DontCare
  // control path
  inSelWire := io.inIOs.ctrlPath.inDataSel
  outSelWire := io.inIOs.ctrlPath.outDataSel
}

class PSumRouter extends Module with ClusterConfig {
  val io = new PSumRouterIO
  val inSelWire: UInt = Wire(UInt(2.W)) // 0 for PE Cluster, 1 for GLB Cluster, 2 for vertical
  val outSelWire: UInt = Wire(UInt(2.W)) // 0 for PE Cluster, 1 for GLB Cluster, 2 for vertical
  val inDataWire: DecoupledIO[UInt] = Wire(Decoupled(UInt(psDataWidth.W)))
  val outDataWire: DecoupledIO[UInt] = Wire(Decoupled(UInt(psDataWidth.W)))
  inDataWire <> outDataWire
  inDataWire <> MuxLookup(inSelWire, 0.U, io.inIOs.dataPath.zipWithIndex.map({
    case (value, i) =>
      i.asUInt -> value
  }))
  io.outIOs.dataPath.zipWithIndex.foreach({ case (value, i) => value <> Mux(outSelWire === i.asUInt, outDataWire, DontCare)})
  // control path
  inSelWire := io.inIOs.ctrlPath.inDataSel
  outSelWire := io.inIOs.ctrlPath.outDataSel
}

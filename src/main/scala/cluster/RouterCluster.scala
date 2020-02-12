package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe.CSCStreamIO

class RouterCluster(debug: Boolean) extends Module with ClusterConfig {
  val io = new RouterClusterIO
  val iRouters: Vec[CommonRouterIO[UInt, UInt]] = Vec(iactRouterNum, Module(new IactRouter).io)
  val wRouters: Vec[CommonRouterIO[Bool, Bool]] = Vec(weightRouterNum, Module(new WeightRouter).io)
  val pSRouters: Vec[PSumRouterIO] = Vec(pSumRouterNum, Module(new PSumRouter).io)
  io.dataPath.routerData.iRIO.zip(iRouters).foreach({case (x, y) => x <> y.dataPath})
  io.dataPath.routerData.wRIO.zip(wRouters).foreach({case (x, y) => x <> y.dataPath})
  io.dataPath.routerData.pSRIO.zip(pSRouters).foreach({case (x, y) => x <> y.dataPath})
  io.ctrlPath.iRIO.zip(iRouters).foreach({case (x, y) => x <> y.ctrlPath})
  io.ctrlPath.wRIO.zip(wRouters).foreach({case (x, y) => x <> y.ctrlPath})
  io.ctrlPath.pSRIO.zip(pSRouters).foreach({case (x, y) => x <> y.ctrlPath})
  io.dataPath.routerOutPSumToPEIO.zip(pSRouters).foreach({ case (x, y) => x <> y.dataPath.outIOs.head})
}

class IactRouter extends Module with ClusterConfig {
  val io = new CommonRouterIO[UInt, UInt](UInt(2.W), UInt(2.W), iactPortNum, iactAddrWidth, iactDataWidth)
  val inSelWire: UInt = Wire(UInt(2.W)) // 0  for GLB Cluster, 1 for north, 2 for south, 3 for horizontal
  // router model: 0: uni-cast, 1: horizontal, 2: vertical, 3: broadcast
  val outSelWire: UInt = Wire(UInt(2.W)) // 0 for PE Cluster, 1 for north, 2 for south, 3 for horizontal
  val inDataWire: CSCStreamIO = Wire(new CSCStreamIO(iactAddrWidth, iactDataWidth))
  val outDataWire: CSCStreamIO = Wire(new CSCStreamIO(iactAddrWidth, iactDataWidth))
  inDataWire <> outDataWire
  inDataWire <> MuxLookup(inSelWire, 0.U, io.dataPath.inIOs.zipWithIndex.map({
    case (o, i) =>
      i.asUInt -> o
  }))
  switch (outSelWire) {
    is (0.U) { // uni-cast
      io.dataPath.outIOs(0) <> outDataWire // 0 to PE array
      io.dataPath.outIOs.takeRight(3).foreach(_ <> DontCare)
    }
    is (1.U) { // horizontal
      io.dataPath.outIOs(0) <> outDataWire
      io.dataPath.outIOs(1) <> DontCare // not send this time
      io.dataPath.outIOs(2) <> DontCare // not send this time
      io.dataPath.outIOs(3) <> outDataWire
    }
    is (2.U) { // vertical
      io.dataPath.outIOs.take(3).foreach(_ <> outDataWire)
      io.dataPath.outIOs(3) <> DontCare// not send this time
    }
    is (3.U) { // broad-cast
      io.dataPath.outIOs.foreach(_ <> outDataWire)
    }
  }
  // control path
  inSelWire := io.ctrlPath.inDataSel
  outSelWire := io.ctrlPath.outDataSel
}

class WeightRouter extends Module with ClusterConfig {
  val io = new CommonRouterIO[Bool, Bool](Bool(), Bool(), weightPortNum, weightAddrWidth, weightDataWidth)
  // inSelWire: 0, receive the data come from GLB Cluster; 1, receive it come from its neighborhood WeightRouter
  val inSelWire: Bool = Wire(Bool())
  // outSelWire: 0, send the data to PE Cluster; 1, send it to its neighborhood WeightRouter and PE Cluster
  val outSelWire: Bool = Wire(Bool())
  val inDataWire: CSCStreamIO = Wire(new CSCStreamIO(weightAddrWidth, weightDataWidth))
  val outDataWire: CSCStreamIO = Wire(new CSCStreamIO(weightAddrWidth, weightDataWidth))
  inDataWire <> outDataWire
  inDataWire <> Mux(inSelWire, io.dataPath.inIOs(1), io.dataPath.inIOs.head)
  io.dataPath.outIOs.head <> outDataWire
  io.dataPath.outIOs(1) <> Mux(outSelWire, outDataWire, DontCare)
  // control path
  inSelWire := io.ctrlPath.inDataSel
  outSelWire := io.ctrlPath.outDataSel
}

class PSumRouter extends Module with ClusterConfig {
  val io = new PSumRouterIO
  val inSelWire: UInt = Wire(UInt(2.W)) // 0 for PE Cluster, 1 for GLB Cluster, 2 for vertical
  val outSelWire: UInt = Wire(UInt(2.W)) // 0 for PE Cluster, 1 for GLB Cluster, 2 for vertical
  val inDataWire: DecoupledIO[UInt] = Wire(Decoupled(UInt(psDataWidth.W)))
  val outDataWire: DecoupledIO[UInt] = Wire(Decoupled(UInt(psDataWidth.W)))
  inDataWire <> outDataWire
  inDataWire <> MuxLookup(inSelWire, 0.U, io.dataPath.inIOs.zipWithIndex.map({
    case (value, i) =>
      i.asUInt -> value
  }))
  io.dataPath.outIOs.zipWithIndex.foreach({ case (value, i) => value <> Mux(outSelWire === i.asUInt, outDataWire, DontCare)})
  // control path
  inSelWire := io.ctrlPath.inDataSel
  outSelWire := io.ctrlPath.outDataSel
}

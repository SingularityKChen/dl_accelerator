package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe.CSCStreamIO

class RouterCluster(debug: Boolean) extends Module with ClusterConfig {
  val io: RouterClusterIO = IO(new RouterClusterIO)
  private val iRouters = Seq.fill(inActRouterNum){ Module(new InActRouter).io }
  private val wRouters = Seq.fill(weightRouterNum){ Module(new WeightRouter).io }
  private val pSRouters = Seq.fill(pSumRouterNum){ Module(new PSumRouter).io }
  io.dataPath.routerData.iRIO.zip(iRouters).foreach({case (x, y) => x <> y.dataPath})
  io.dataPath.routerData.wRIO.zip(wRouters).foreach({case (x, y) => x <> y.dataPath})
  io.dataPath.routerData.pSumRIO.zip(pSRouters).foreach({case (x, y) => x <> y.dataPath})
  io.ctrlPath.iRIO.zip(iRouters).foreach({case (x, y) => x <> y.ctrlPath})
  io.ctrlPath.wRIO.zip(wRouters).foreach({case (x, y) => x <> y.ctrlPath})
  io.ctrlPath.pSumRIO.zip(pSRouters).foreach({case (x, y) => x <> y.ctrlPath})
  io.dataPath.routerData.wRIO.zipWithIndex.foreach({ case (o, i) =>
    o.outIOs.head.suggestName(s"weightRouter${i}ToPECluster")
    o.outIOs.last.suggestName(s"weightRouter${i}ToNeighbor")
  })
  pSRouters.foreach(_.pSumLoadEn := io.pSumLoadEn)
}

class InActRouter extends CSCRouter with ClusterConfig {
  val io: CommonRouterUIntIO = IO(new CommonRouterUIntIO(inActPortNum, inActAdrWidth, inActDataWidth))
  // io.dataPath.inIOs(0) : inActRouterFromGLB
  // io.dataPath.inIOs(1) : inActRouterFromNorth
  // io.dataPath.inIOs(2) : inActRouterFromSouth
  // io.dataPath.inIOs(3) : inActRouterFromHorizontal
  // io.dataPath.outIOs(0): inActRouterToPEArray
  // io.dataPath.outIOs(1): inActRouterToNorth
  // io.dataPath.outIOs(2): inActRouterToSouth
  // io.dataPath.outIOs(3): inActRouterToHorizontal
  private val inSelWire: UInt = Wire(UInt(2.W)) // 0  for GLB Cluster, 1 for north, 2 for south, 3 for horizontal
  // outSelWire: 0: uni-cast, 1: horizontal, 2: vertical, 3: broadcast
  inSelWire.suggestName("inActRouterInSelWire")
  private val outSelWire: UInt = Wire(UInt(2.W))
  outSelWire.suggestName("inActRouterOutSelWire")
  private val internalDataWire: CSCStreamIO = Wire(new CSCStreamIO(inActAdrWidth, inActDataWidth))
  internalDataWire.suggestName("inActInternalDataWire")
  private val inSelEqWires = Seq.fill(io.dataPath.inIOs.length){Wire(Bool())}
  inSelEqWires.zipWithIndex.foreach({ case (bool, i) =>
    bool.suggestName(s"inActInSelEq${i}Wire")
    bool := inSelWire === i.asUInt
  })
  private val outSelEqWires = Seq.fill(io.dataPath.outIOs.length){Wire(Bool())}
  outSelEqWires.zipWithIndex.foreach({ case (bool, i) =>
    bool.suggestName(s"inActOutSelEq${i}Wire")
    bool := inSelWire === i.asUInt
  })
  when (inSelEqWires.head) { // from GLB
    internalDataWire <> io.dataPath.inIOs(0)
    io.dataPath.inIOs.takeRight(3).foreach({x =>
      disableAdrDataReady(x)
    })
  } .elsewhen (inSelEqWires(1)) { // from north
    disableAdrDataReady(io.dataPath.inIOs(0))
    internalDataWire <> io.dataPath.inIOs(1)
    io.dataPath.inIOs.takeRight(2).foreach({x =>
      disableAdrDataReady(x)
    })
  } .elsewhen (inSelEqWires(2)) { // from south
    io.dataPath.inIOs.take(2).foreach({x =>
      disableAdrDataReady(x)
    })
    internalDataWire <> io.dataPath.inIOs(2)
    disableAdrDataReady(io.dataPath.inIOs(3))
  } .otherwise { // from horizontal neighborhood
    io.dataPath.inIOs.take(3).foreach({x =>
      disableAdrDataReady(x)
    })
    internalDataWire <> io.dataPath.inIOs(3)
  }
  when (outSelEqWires.head) { // uni-cast
    io.dataPath.outIOs(0) <> internalDataWire // 0 to PE array
    io.dataPath.outIOs.takeRight(3).foreach(_ <> DontCare)
  } .elsewhen (outSelEqWires(1)) { // horizontal
    connectAllExceptReady(io.dataPath.outIOs(0), internalDataWire)
    io.dataPath.outIOs(1) <> DontCare // not send this time
    io.dataPath.outIOs(2) <> DontCare // not send this time
    connectAllExceptReady(io.dataPath.outIOs(3), internalDataWire)
    internalDataWire.adrIOs.data.ready := io.dataPath.outIOs(0).adrIOs.data.ready && io.dataPath.outIOs(3).adrIOs.data.ready
    internalDataWire.dataIOs.data.ready := io.dataPath.outIOs(0).dataIOs.data.ready && io.dataPath.outIOs(3).dataIOs.data.ready
  } .elsewhen (outSelEqWires(2)) { // vertical
    io.dataPath.outIOs.take(3).foreach(_ <> internalDataWire)
    io.dataPath.outIOs(3) <> DontCare// not send this time
  } .otherwise { // broad-cast
    io.dataPath.outIOs.foreach(_ <> internalDataWire)
  }
  // control path
  inSelWire := io.ctrlPath.inDataSel
  outSelWire := io.ctrlPath.outDataSel
}

class WeightRouter extends CSCRouter with ClusterConfig {
  val io: CommonRouterBoolIO = IO(new CommonRouterBoolIO(weightPortNum, weightAdrWidth, weightDataWidth))
  //io.dataPath.inIOs(0) : weightRouterFromGLB
  //io.dataPath.inIOs(1) : weightRouterFromHorizontal
  //io.dataPath.outIOs(0): weightRouterToPEArray
  //io.dataPath.outIOs(1): weightRouterToHorizontal
  // inSelWire: 0, receive the data come from GLB Cluster; 1, receive it come from its neighborhood WeightRouter
  private val inSelWire: Bool = Wire(Bool())
  inSelWire.suggestName("weightRouterInSelWire")
  // outSelWire: 0, send the data to PE Cluster; 1, send it to its neighborhood WeightRouter and PE Cluster
  private val outSelWire: Bool = Wire(Bool())
  outSelWire.suggestName("weightRouterOutSelWire")
  private val internalDataWire: CSCStreamIO = Wire(new CSCStreamIO(weightAdrWidth, weightDataWidth))
  internalDataWire.suggestName("weightInternalDataWire")
  when (inSelWire) {
    internalDataWire <> io.dataPath.inIOs(1)
    disableAdrDataReady(io.dataPath.inIOs.head)
  } .otherwise {
    internalDataWire <> io.dataPath.inIOs.head
    disableAdrDataReady(io.dataPath.inIOs(1))
  }
  io.dataPath.outIOs.head <> internalDataWire
  when (outSelWire) {
    io.dataPath.outIOs(1) <> internalDataWire
  } .otherwise {
    io.dataPath.outIOs(1) <> DontCare
  }
  // control path
  inSelWire := io.ctrlPath.inDataSel
  outSelWire := io.ctrlPath.outDataSel
}

class PSumRouter extends Module with ClusterConfig {
  val io: PSumRouterIO = IO(new PSumRouterIO)
  //io.dataPath.inIOs(0) : pSumRouterFromPEArray
  //io.dataPath.inIOs(1) : pSumRouterFromGLB
  //io.dataPath.inIOs(2) : pSumRouterFromNorthern
  //io.dataPath.outIOs(0): pSumRouterToPEArray
  //io.dataPath.outIOs(1): pSumRouterToGLB
  //io.dataPath.outIOs(2): pSumRouterToSouthern
  private val inSelWire: UInt = Wire(UInt(2.W)) // 0 for PE Cluster, 1 for GLB Cluster, 2 for vertical
  inSelWire.suggestName("pSumRouterInSelWire")
  private val outSelWire: UInt = Wire(UInt(2.W)) // 0 for PE Cluster, 1 for GLB Cluster, 2 for vertical
  outSelWire.suggestName("pSumRouterOutSelWire")
  private val pSumLoadEnReg = RegInit(false.B)
  private val internalDataWire: DecoupledIO[UInt] = Wire(Decoupled(UInt(psDataWidth.W)))
  internalDataWire.suggestName("pSumInternalDataWire")
  private val inSelEqWires = Seq.fill(io.dataPath.inIOs.length){Wire(Bool())}
  inSelEqWires.zipWithIndex.foreach({ case (bool, i) =>
    bool.suggestName(s"pSumInSelEq${i}Wire")
    bool := inSelWire === i.asUInt
  })
  private val outSelEqWires = Seq.fill(io.dataPath.outIOs.length){Wire(Bool())}
  outSelEqWires.zipWithIndex.foreach({ case (bool, i) =>
    bool.suggestName(s"pSumOutSelEq${i}Wire")
    bool := inSelWire === i.asUInt
  })
  when (inSelEqWires.head) {
    internalDataWire <> io.dataPath.inIOs(0)
    io.dataPath.inIOs(1).ready := false.B
    io.dataPath.inIOs(2).ready := false.B
  } .elsewhen (inSelEqWires(1)) {
    internalDataWire <> io.dataPath.inIOs(1)
    io.dataPath.inIOs(0).ready := false.B
    io.dataPath.inIOs(2).ready := false.B
  } .otherwise {
    internalDataWire <> io.dataPath.inIOs(2)
    io.dataPath.inIOs(0).ready := false.B
    io.dataPath.inIOs(1).ready := false.B
  }
  when (outSelEqWires.head) {
    // then it will need write data back
    io.dataPath.outIOs(0).bits := internalDataWire.bits
    io.dataPath.outIOs(0).valid := internalDataWire.valid && pSumLoadEnReg
    internalDataWire.ready := io.dataPath.outIOs(0).ready && pSumLoadEnReg
    io.dataPath.outIOs(1) <> DontCare
    io.dataPath.outIOs(2) <> DontCare
  } .elsewhen (outSelEqWires(1)) {
    io.dataPath.outIOs(0) <> DontCare
    io.dataPath.outIOs(1) <> internalDataWire
    io.dataPath.outIOs(2) <> DontCare
  } .otherwise {
    io.dataPath.outIOs(0) <> DontCare
    io.dataPath.outIOs(1) <> DontCare
    io.dataPath.outIOs(2) <> internalDataWire
  }
  // control path
  inSelWire := io.ctrlPath.inDataSel
  outSelWire := io.ctrlPath.outDataSel
  pSumLoadEnReg := io.pSumLoadEn
}

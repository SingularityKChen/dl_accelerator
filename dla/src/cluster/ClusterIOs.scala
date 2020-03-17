package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe.CSCStreamIO

class ClusterGroupIO extends Bundle {
  val ctrlPath: ClusterGroupCtrlIO = Flipped(new ClusterGroupCtrlIO) // now, input them
  val dataPath = new ClusterGroupDataIO
}

class ClusterGroupDataIO extends Bundle with ClusterConfig {
  val glbDataPath = new GLBClusterDataIO // through top to GLB
  val cgDataPath = new RouterDataIO // communicate with other cluster groups
  // pSumDataVerticalIOs.outIOs for translating partial sum from each head in PE array to each tail of its northern PE Array
  // pSumDataVerticalIOs.inIOs, on the other hand, receive partial sum from the head of its southern PE Array
  val pSumDataVerticalIOs = new PSumRouterDataIO(pSumRouterNum, psDataWidth) // input and output
}

class ClusterGroupCtrlIO extends Bundle {
  val peClusterCtrl = new Bundle {
    val inActSel = new CommonClusterCtrlBoolUIntIO
    val pSumInSel: Bool = Output(Bool())
  } // output select signals to PE Cluster
  val routerClusterCtrl: RouterClusterCtrlIO = new RouterClusterCtrlIO // output select signals to Router Cluster
  val readOutPSum: Bool = Output(Bool()) // true then to read out partial sums from GLB
}

class RouterClusterIO extends Bundle {
  val ctrlPath: RouterCtrlIO = Flipped(new RouterCtrlIO) // input
  val dataPath = new RouterClusterDataIO
}

class RouterClusterDataIO extends Bundle with ClusterConfig {
  val routerData = new RouterDataIO
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
  val ctrlPath: CommonClusterCtrlTwoBoolIO = Flipped(new CommonClusterCtrlTwoBoolIO)
  //  see commends in class RouterClusterCtrlIO
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
  // uni-cast, horizontal, vertical, broad-cast
  // inActCtrlSel.inDataSel: 0 for GLB Cluster, 1 for north, 2 for south, 3 for horizontal
  // inActCtrlSel.outDataSel 0 for PE Cluster, 1 for north, 2 for south, 3 for horizontal
  val inActCtrlSel = new CommonClusterCtrlTwoUIntIO
  // Weight Models: broad-cast, multi-cast, uni-cast, but the first two seems the same inner weight cluster
  // weightCtrlSel.inDataSel: true for broad-cast and multi-cast, false for uni-cast
  // weightCtrlSel.outDataSel: 0, send the data to PE Cluster; 1, send it to its neighboring WeightRouter and PE Cluster
  val weightCtrlSel = new CommonClusterCtrlTwoBoolIO
  // inData from PECluster will connect directly to outData to GLBCluster
  // pSumCtrlSel.inDataSel: true for GLB Cluster, false for vertical
  // pSumCtrlSel.outDataSel: true for PE Cluster, false for vertical
  val pSumCtrlSel = new CommonClusterCtrlTwoBoolIO
}

class PEClusterIO extends Bundle with ClusterConfig {
  val dataPath = new PEClusterDataIO
  val ctrlPath = new PEClusterCtrlIO
  val debugIO = new PEClusterDebugIO
}

class PEClusterDataIO extends Bundle with ClusterConfig {
  val inActIO: Vec[CSCStreamIO] = Vec(inActRouterNum, Flipped(new CSCStreamIO(inActAdrWidth, inActDataWidth))) // input only
  val weightIO: Vec[CSCStreamIO] = Vec(weightRouterNum, Flipped(new CSCStreamIO(weightAdrWidth, weightDataWidth)))
  val pSumIO = new PSumRouterDataIO(pSumRouterNum, psDataWidth) // input and output
  // pSumDataFromSouthernIO: the dataIO from southern PEArray
  val pSumDataFromSouthernIO: Vec[DecoupledIO[UInt]] = Vec(pSumRouterNum, Flipped(DecoupledIO(UInt(psDataWidth.W)))) // input only
}

class PEClusterCtrlIO extends Bundle with HasPSumLoadEnIO {
  // inActCtrlSel.inDataSel: true for broad-cast, false for others
  // inActCtrlSel.outDataSel: the value indicates the index of router
  val inActCtrlSel: CommonClusterCtrlBoolUIntIO = Flipped(new CommonClusterCtrlBoolUIntIO)
  // pSumCtrlSel.inDataSel: true, then receive data from PSumRouter, false then receive data from its southern PE Array
  // pSumCtrlSel.outDataSel: unused
  val pSumCtrlSel: CommonClusterCtrlTwoBoolIO = Flipped(new CommonClusterCtrlTwoBoolIO)
  val doEn: Bool = Input(Bool()) // load inAct and weight
  val allPSumAddFin: Bool = Output(Bool()) // true when all columns of PEs have finished accumulating PSum
  val allCalFin: Bool = Output(Bool()) // true when all pe finish computation
}

trait HasConfigIOs extends Bundle {
  val configIOs: Vec[UInt] = Output(Vec(6, UInt(3.W))) // that's GNMFCS
}

trait HasPSumLoadEnIO extends Bundle {
  val pSumLoadEn: Bool = Input(Bool()) // load accumulate pSum
}

class CommonClusterCtrlTwoUIntIO extends Bundle {
  val inDataSel: UInt = Output(UInt(2.W))
  val outDataSel: UInt = Output(UInt(2.W))
}

class CommonClusterCtrlBoolUIntIO extends Bundle {
  val inDataSel: Bool = Output(Bool())
  val outDataSel: UInt = Output(UInt(2.W))
}

class CommonClusterCtrlTwoBoolIO extends Bundle {
  val inDataSel: Bool = Output(Bool())
  val outDataSel: Bool = Output(Bool())
}

trait HasBusySignal extends Bundle {
  val busy: Bool = Output(Bool())
}

abstract class HasConnectAllExpRdModule extends Module {
  protected def connectAllExceptReady(slaverIO: CSCStreamIO, masterIO: CSCStreamIO): Unit ={
    slaverIO.dataIOs.data.bits := masterIO.dataIOs.data.bits
    slaverIO.dataIOs.data.valid := masterIO.dataIOs.data.valid
    slaverIO.adrIOs.data.bits := masterIO.adrIOs.data.bits
    slaverIO.adrIOs.data.valid := masterIO.adrIOs.data.valid
  }
}

abstract class CSCRouter extends HasConnectAllExpRdModule {
  protected def disableAdrDataReady(disabledIO: CSCStreamIO): Unit = {
    disabledIO.adrIOs.data.ready := false.B
    disabledIO.dataIOs.data.ready := false.B
  }
  protected def disableAdrDataValid(disabledIO: CSCStreamIO): Unit = {
    disabledIO.adrIOs.data.valid := false.B
    disabledIO.dataIOs.data.valid := false.B
    disabledIO.adrIOs.data.bits := DontCare
    disabledIO.dataIOs.data.bits := DontCare
  }
}

package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe._

class PECluster(val peColNum: Int, val peRowNum: Int, debug: Boolean) extends Module with ClusterConfig {
  val io = new PEAndRouterIO
  val peRow =  Vec(peColNum, Module(new ProcessingElement(debug = debug)).io)
  val peArray = Vec(peRowNum, peRow)
  val muxInPSumWire: Vec[DecoupledIO[UInt]] = Vec(peColNum, Wire(Decoupled(UInt(psDataWidth.W))))
  val muxIactDataWire: ClusterAddrWithDataCommonIO = Wire(new ClusterAddrWithDataCommonIO(iactAddrWidth, iactDataWidth, commonLenWidth, commonLenWidth))
  val iactRoutingMode: UInt = Wire(UInt(2.W))
  val muxIactCtrlWire: UInt = Wire(UInt(log2Ceil(iactRouterNum).W))
  val muxInPSumCtrlWire: Bool = Wire(Bool()) // TODO: whether each column needs one select signal?
  muxIactDataWire := MuxLookup(muxIactCtrlWire, 0.U, io.iactCluster.dataPath.zipWithIndex.map({
    case (x,y) =>
      y.asUInt -> x
  }))

  def iactWeightConnection(peIO: DataAddrStreamIO, connectIO: ClusterAddrWithDataCommonIO): Any = {
    Seq(peIO.addrIOs, peIO.dataIOs).zip(Seq(connectIO.addrIOs, connectIO.dataIOs)).foreach({
      case (x, y) =>
        x.streamLen := y.dataLenIO
        x.writeInDataIO <> y.dataIO
    })
  }

  for (i <- 0 until peColNum) {
    // we regard the neighborhood as two input of a Mux
    muxInPSumWire(i) <> Mux(muxInPSumCtrlWire, io.pSUmCluster.inIOs.dataPath(i*2), io.pSUmCluster.inIOs.dataPath(i*2+1))
    // connect output partial sum produced by the PE at the head of each column to one output partial sum top IO
    peArray.head(i).dataStream.opsIO <> io.pSUmCluster.outIOs.dataPath(i)
    // connect input partial sum from top IO to the PE at the tail of each column with the signal after Mux
    peArray.last(i).dataStream.ipsIO <> muxInPSumWire(i)
    for (j <- 0 until peRowNum) {
      iactWeightConnection(peArray(j)(i).dataStream.weightIOs, io.weightCluster.dataPath(j))
      iactWeightConnection(peArray(j)(i).dataStream.iactIOs, muxIactDataWire)
    }
  }
  iactRoutingMode := io.iactCluster.ctrlPath.routingMode
  // iact mux control logic here
  muxIactCtrlWire := iactRoutingMode // FIXME
}

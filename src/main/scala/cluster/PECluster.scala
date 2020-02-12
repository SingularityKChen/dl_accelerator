package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe._

class PECluster(debug: Boolean) extends Module with ClusterConfig {
  val diagNum: Int = peColNum + peRowNum - 1
  val io = new PEClusterIO
  // io.ctrlPath.iactCtrlSel.inDataSel indicates whether the input activations should be broad-cast;
  //   true then broad-cast, and read the index of router that should be broad-casted; false then only get the
  //   corresponding index of input activations router;
  // io.ctrlPath.iactCtrlSel.outDataSel should be assigned to the index of router port when broad-cast;
  val peRow: Vec[ProcessingElementIO] =  Vec(peColNum, Module(new ProcessingElement(debug = debug)).io)
  val peArray: Vec[Vec[ProcessingElementIO]] = Vec(peRowNum, peRow)
  val muxInPSumWire: Vec[DecoupledIO[UInt]] = Vec(peColNum, Wire(Decoupled(UInt(psDataWidth.W))))
  val muxIactDataWire: Vec[Vec[CSCStreamIO]] = Vec(peColNum, Vec(peRowNum,Wire(new CSCStreamIO(iactAddrWidth, iactDataWidth))))
  // iactRoutingMode: whether the input activations should be broad-cast;
  // true then all PEs' data receive from the same router, that's outDataSel's value;
  val iactRoutingMode: Bool = Wire(Bool())
  // iactMuxDiagIdxWire: correspond to each diagonal, to choose data read in;
  // 0-2 to iactCluster dataPath's responding index, 3 means wait for a while
  val iactMuxDiagIdxWire: Vec[UInt] = Vec(diagNum, Wire(UInt(2.W)))
  val iactBroadCastIdxWire: UInt = Wire(UInt(2.W))
  // iactFormerOrLater: correspond to each router data in, to see which part of PEs can read in data
  val iactFormerOrLater: Vec[Bool] = Vec(iactRouterNum, Wire(Bool())) // true for former, false for later readF
  // muxInPSumCtrlWire: true, then ; false, then
  val muxInPSumCtrlWire: Bool = Wire(Bool()) // TODO: whether each column needs one select signal?
  def iactWeightConnection(peIO: CSCStreamIO, connectIO: CSCStreamIO): Any = {
    Seq(peIO.addrIOs, peIO.dataIOs).zip(Seq(connectIO.addrIOs, connectIO.dataIOs)).foreach({
      case (x, y) =>
        x.data <> y.data
    })
  }
  for (i <- 0 until peColNum) {
    // connect output partial sum produced by the PE at the head of each column to one output partial sum top IO
    peArray.head(i).dataStream.opsIO <> io.dataPath.pSumIO.outIOs(i)
    // connect input partial sum from top IO to the PE at the tail of each column with the signal after Mux
    peArray.last(i).dataStream.ipsIO <> muxInPSumWire(i)
    muxInPSumWire(i) <> Mux(muxInPSumCtrlWire, io.dataPath.pSumIO.inIOs(i), io.dataPath.routerInPSumToPEIO(i))
    for (j <- 0 until peRowNum) {
      iactWeightConnection(peArray(j)(i).dataStream.weightIOs, io.dataPath.weightIO(j))
      iactWeightConnection(peArray(j)(i).dataStream.iactIOs, muxIactDataWire(j)(i))
      muxIactDataWire(j)(i) <> Mux(iactMuxDiagIdxWire(j + i) =/= 3.U, MuxLookup(iactMuxDiagIdxWire(j + i), 0.U, io.dataPath.iactIO.zipWithIndex.map({
        case (x,y) =>
          y.asUInt -> x
      })), DontCare) // TODO: check whether the valid equals to false when muxIdxWire equals to 3.U
    }
  }
  for (idx <- 0 until iactRouterNum) {
    iactMuxDiagIdxWire(idx) := Mux(!iactRoutingMode, Mux(iactFormerOrLater(idx), idx.U, 3.U), iactBroadCastIdxWire) // former PEs
    iactMuxDiagIdxWire(idx + iactRouterNum) := Mux(!iactRoutingMode, Mux(!iactFormerOrLater(idx), idx.U, 3.U), iactBroadCastIdxWire) // later PEs
  }
  // connections of control IOs
  muxInPSumCtrlWire := io.ctrlPath.pSumCtrlSel.inDataSel
  iactRoutingMode := io.ctrlPath.iactCtrlSel.inDataSel // true for broad-cast
  iactBroadCastIdxWire := io.ctrlPath.iactCtrlSel.outDataSel
}

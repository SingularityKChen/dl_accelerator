package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe._

class PECluster(debug: Boolean) extends Module with ClusterConfig {
  val diagnNum: Int = peColNum + peRowNum - 1
  val io: PEClusterIO = IO(new PEClusterIO)
  // io.ctrlPath.inActCtrlSel.inDataSel indicates whether the input activations should be broad-cast;
  //   true then broad-cast, and read the index of router that should be broad-casted; false then only get the
  //   corresponding index of input activations router;
  // io.ctrlPath.inActCtrlSel.outDataSel should be assigned to the index of router port when broad-cast;
  private val peRow: Vec[ProcessingElementIO] =  Vec(peColNum, Module(new ProcessingElement(debug = debug)).io)
  private val peArray: Vec[Vec[ProcessingElementIO]] = Vec(peRowNum, peRow)
  private val muxInPSumWire: Vec[DecoupledIO[UInt]] = Vec(peColNum, Wire(Decoupled(UInt(psDataWidth.W))))
  private val muxInActDataWire: Vec[Vec[CSCStreamIO]] = Vec(peColNum, Vec(peRowNum,Wire(new CSCStreamIO(inActAdrWidth, inActDataWidth))))
  // inActRoutingMode: whether the input activations should be broad-cast;
  // true then all PEs' data receive from the same router whose index equals to outDataSel's value;
  private val inActRoutingMode: Bool = Wire(Bool())
  // inActMuxDiagnIdxWire: correspond to each diagonal, to choose data read in;
  // 0-2 to inActCluster dataPath's responding index, 3 means wait for a while
  private val inActMuxDiagnIdxWire: Vec[UInt] = Vec(diagnNum, Wire(UInt(2.W)))
  private val inActBroadCastIdxWire: UInt = Wire(UInt(2.W))
  // inActFormerOrLater: correspond to each router data in, to see which part of PEs can read in data
  private val inActFormerOrLater: Vec[Bool] = Vec(inActRouterNum, Wire(Bool())) // true for former, false for later readF
  // muxInPSumCtrlWire: true, then ; false, then
  private val muxInPSumCtrlWire: Bool = Wire(Bool()) // TODO: whether each column needs one select signal?
  private def inActWeightConnection(peIO: CSCStreamIO, connectIO: CSCStreamIO): Any = {
    Seq(peIO.adrIOs, peIO.dataIOs).zip(Seq(connectIO.adrIOs, connectIO.dataIOs)).foreach({
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
      inActWeightConnection(peArray(j)(i).dataStream.weightIOs, io.dataPath.weightIO(j))
      inActWeightConnection(peArray(j)(i).dataStream.inActIOs, muxInActDataWire(j)(i))
      muxInActDataWire(j)(i) <> Mux(inActMuxDiagnIdxWire(j + i) =/= 3.U, MuxLookup(inActMuxDiagnIdxWire(j + i), 0.U, io.dataPath.inActIO.zipWithIndex.map({
        case (x,y) =>
          y.asUInt -> x
      })), DontCare) // TODO: check whether the valid equals to false when muxIdxWire equals to 3.U
    }
  }
  for (idx <- 0 until inActRouterNum) {
    inActMuxDiagnIdxWire(idx) := Mux(!inActRoutingMode, Mux(inActFormerOrLater(idx), idx.U, 3.U), inActBroadCastIdxWire) // former PEs
    inActMuxDiagnIdxWire(idx + inActRouterNum) := Mux(!inActRoutingMode, Mux(!inActFormerOrLater(idx), idx.U, 3.U), inActBroadCastIdxWire) // later PEs
  }
  // connections of control IOs
  muxInPSumCtrlWire := io.ctrlPath.pSumCtrlSel.inDataSel
  inActRoutingMode := io.ctrlPath.inActCtrlSel.inDataSel // true for broad-cast
  inActBroadCastIdxWire := io.ctrlPath.inActCtrlSel.outDataSel
}

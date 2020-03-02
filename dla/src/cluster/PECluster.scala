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
  private val peOneRow = VecInit(Seq.fill(peColNum){Module(new ProcessingElement(debug = debug)).io})
  private val peArray = VecInit(Seq.fill(peRowNum){peOneRow})
  peArray.foreach(_.foreach(_.debugIO <> DontCare))
  private val muxInPSumWire = Wire(Vec(peColNum, Decoupled(UInt(psDataWidth.W))))
  private val muxInActDataWire = Wire(Vec(peRowNum, Vec(peColNum, new CSCStreamIO(inActAdrWidth, inActDataWidth))))
  // inActRoutingMode: whether the input activations should be broad-cast;
  // true then all PEs' data receive from the same router whose index equals to outDataSel's value;
  private val inActRoutingMode = Wire(Bool())
  // inActMuxDiagnIdxWire: correspond to each diagonal, to choose data read in;
  // 0-2 to inActCluster dataPath's responding index, 3 means wait for a while
  private val inActBroadCastIdxWire = Wire(UInt(2.W))
  // inActFormerOrLater: correspond to each router data in, to see which part of PEs can read in data
  private val inActFormerOrLater = Wire(Vec(inActRouterNum, Bool())) // true for former, false for later readF
  // muxInPSumSelWire: true, then ; false, then
  private val muxInPSumSelWire = Wire(Vec(peColNum , Bool()))
  private val configRegs = VecInit(Seq.fill(6){RegInit(0.U)}) // to store GNMFCS
  configRegs <> io.ctrlPath.configIOs
  private val f2Inc = Wire(Bool())
  f2Inc := configRegs(3) =/= io.ctrlPath.configIOs(3) // if neq, then f2 increases
  private val thePELoadWires = Wire(Vec(peRowNum, Vec(peColNum, Bool())))
  private val inActAdrWEWires = Wire(Vec(peRowNum, Vec(peColNum, Bool()))) // inAct adress writeEn wires
  private val inActDataWEWires = Wire(Vec(peRowNum, Vec(peColNum, Bool()))) // inAct data writeEn wires
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
    muxInPSumWire(i) <> Mux(muxInPSumSelWire(i), io.dataPath.pSumIO.inIOs(i), io.dataPath.routerInPSumToPEIO(i))
    for (j <- 0 until peRowNum) {
      peArray(j)(i).dataStream.inActIOs.adrIOs.data.ready := inActAdrWEWires(j)(i) && muxInActDataWire(j)(i).adrIOs.data.ready
      peArray(j)(i).dataStream.inActIOs.dataIOs.data.ready := muxInActDataWire(j)(i).dataIOs.data.ready && inActDataWEWires(j)(i)
      inActWeightConnection(peArray(j)(i).dataStream.weightIOs, io.dataPath.weightIO(j))
      inActWeightConnection(peArray(j)(i).dataStream.inActIOs, muxInActDataWire(j)(i))
    }
  }
  muxInPSumSelWire.foreach(x => x := io.ctrlPath.pSumCtrlSel.inDataSel) // TODO: check
  inActRoutingMode := io.ctrlPath.inActCtrlSel.inDataSel // true for broad-cast
  inActBroadCastIdxWire := io.ctrlPath.inActCtrlSel.outDataSel
  // state machine of signal PE
  private val onePEIdle :: onePETrans :: onePEDoing :: onePEWaitPS :: Nil = Enum(4)
  private val thePEStateRegs = VecInit(Seq.fill(peRowNum){VecInit(Seq.fill(peColNum){RegInit(onePEIdle)})})
  for (i <- thePEStateRegs.indices) {
    for (j <- thePEStateRegs.head.indices) {
      thePELoadWires(i)(j) := thePEStateRegs(i)(j) === onePETrans
      peArray(i)(j).topCtrl.doLoadEn := thePELoadWires(i)(j)
      switch (thePEStateRegs(i)(j)) {
        is (onePEIdle) {
          when (io.ctrlPath.doEn) {
            thePEStateRegs(i)(j) := onePETrans
          }
        }
        is (onePETrans) {
          when (peArray(i)(j).topCtrl.writeFinish) {
            thePEStateRegs(i)(j) := onePEDoing
          }
        }
        is (onePEDoing) {
          when (peArray(i)(j).topCtrl.calFinish) {
            thePEStateRegs(i)(j) := onePEWaitPS
          }
        }
      }
    }
  }
  // state machine of inAct in the PE Cluster
  private val inActZero :: inActOne :: inActTwo :: Nil = Enum(3)
  // inActZero: none new
  // inActOne: one new row of computation
  // inActTwo: two new rows of computation
  private val inActStateReg = RegInit(inActZero)
  private val inActDataIOStateRegs = VecInit(Seq.fill(inActRouterNum){RegInit(inActZero)}) // three inIOs
  private val inActDataIOZeroWires = Wire(Vec(inActRouterNum, Bool()))
  private val inActDataIOOneWires = Wire(Vec(inActRouterNum, Bool()))
  private val inActDataIOTwoWires = Wire(Vec(inActRouterNum, Bool()))
  inActDataIOZeroWires.zip(inActDataIOStateRegs).foreach({ case (bool, int) => bool := int === inActZero})
  inActDataIOOneWires.zip(inActDataIOStateRegs).foreach({ case (bool, int) => bool := int === inActOne})
  inActDataIOTwoWires.zip(inActDataIOStateRegs).foreach({ case (bool, int) => bool := int === inActTwo})
  private def threeState(stateReg: UInt, changeWire0: Bool, changeWire1: Bool, changeWire2: Bool): Unit = {
    switch (stateReg) {
      is (inActZero) {
        when (changeWire0) {
          stateReg := inActOne
        }
      }
      is (inActOne) {
        when (changeWire1) {
          stateReg := inActTwo
        }
      }
      is (inActTwo) {
        when (changeWire2) {
          stateReg := inActZero
        }
      }
    }
  }
  threeState(inActStateReg, changeWire0 = f2Inc, changeWire1 = f2Inc, changeWire2 = f2Inc)
  require(peColNum == 4 && peRowNum == 3, "you need to change the following dataPath connections for non default value")
  private val inActWriteDoneRegVec = VecInit(Seq.fill(2){VecInit(Seq.fill(peRowNum){VecInit(Seq.fill(peColNum){RegInit(false.B)})})})
  private val inActWriteDoneWireVec = VecInit(Seq.fill(peRowNum){VecInit(Seq.fill(peColNum){Wire(Bool())})})
  // connections of enWires and inActData path
  for (i <- thePEStateRegs.indices) {
    for (j <- thePEStateRegs.head.indices) {
      inActWriteDoneRegVec(0)(i)(j) := Mux(peArray(i)(j).padWF.inActWriteFin.adrWriteFin, !inActWriteDoneRegVec(0)(i)(j), inActWriteDoneRegVec(0)(i)(j))
      inActWriteDoneRegVec(1)(i)(j) := Mux(peArray(i)(j).padWF.inActWriteFin.dataWriteFin, !inActWriteDoneRegVec(1)(i)(j), inActWriteDoneRegVec(1)(i)(j))
      inActWriteDoneWireVec(i)(j) := inActWriteDoneRegVec(0)(i)(j) && inActWriteDoneRegVec(1)(i)(j)
      def colZeroConnection[DataType<: Data](wireVec: Vec[Vec[DataType]], connectedWireVec: Vec[DataType], default: Data): Unit = {
        wireVec(i)(j) <> MuxLookup(inActStateReg, default, Array(
          0.U -> connectedWireVec((i + j) % 3),
          1.U -> connectedWireVec((i + j + 1) % 3),
          2.U -> connectedWireVec((i + j + 2) % 3)
        ))
      }
      def colOneConnection[DataType<: Data](wireVec: Vec[Vec[DataType]], connectedWireVec: Vec[DataType], default: Data): Unit = {
        wireVec(i)(j) <> MuxLookup(inActStateReg, default, Array(
          0.U -> connectedWireVec((i + j) % 3),
          1.U -> connectedWireVec((i + j + 1) % 3),
          2.U -> connectedWireVec((i + j) % 3)
        ))
      }
      def colTwoConnection[DataType<: Data](wireVec: Vec[Vec[DataType]], connectedWireVec: Vec[DataType], default: Data): Unit = {
        wireVec(i)(j) <> MuxLookup(inActStateReg, default, Array(
          0.U -> connectedWireVec((i + j) % 3),
          1.U -> connectedWireVec((i + j + 2) % 3),
          2.U -> connectedWireVec((i + j) % 3)
        ))
      }
      if (i == 0) {
        colZeroConnection[CSCStreamIO](muxInActDataWire, io.dataPath.inActIO, 0.U)
        if (j == 3) {
          colZeroConnection[Bool](inActAdrWEWires, inActDataIOOneWires, false.B)
        } else {
          colZeroConnection[Bool](inActAdrWEWires, inActDataIOZeroWires, false.B)
        }
      }
      if (i == 1) {
        colOneConnection[CSCStreamIO](muxInActDataWire, io.dataPath.inActIO, 0.U)
        if (j == 0) {
          colOneConnection[Bool](inActAdrWEWires, inActDataIOZeroWires, false.B)
        } else if (j == 1) {
          inActAdrWEWires(i)(j) <> MuxLookup(inActStateReg, false.B, Array(
            0.U -> inActDataIOZeroWires((i + j) % 3),
            1.U -> inActDataIOZeroWires((i + j + 1) % 3),
            2.U -> inActDataIOOneWires((i + j) % 3)
          ))
        } else {
          colOneConnection[Bool](inActAdrWEWires, inActDataIOOneWires, false.B)
        }
      }
      if (i == 2) {
        colTwoConnection[CSCStreamIO](muxInActDataWire, io.dataPath.inActIO, 0.U)
        if (j == 0) {
          inActAdrWEWires(i)(j) <> MuxLookup(inActStateReg, false.B, Array(
            0.U -> inActDataIOZeroWires((i + j) % 3),
            1.U -> inActDataIOOneWires((i + j + 2) % 3),
            2.U -> inActDataIOOneWires((i + j) % 3)
          ))
        } else if (j == 3) {
          inActAdrWEWires(i)(j) <> MuxLookup(inActStateReg, false.B, Array(
            0.U -> inActDataIOOneWires((i + j) % 3),
            1.U -> inActDataIOTwoWires((i + j + 2) % 3),
            2.U -> inActDataIOTwoWires((i + j) % 3)
          ))
        } else {
          colTwoConnection[Bool](inActAdrWEWires, inActDataIOOneWires, false.B)
        }
      }
    }
  }
  private val inActDataStateJumpWires = Wire(Vec(inActRouterNum, Vec(3, Bool())))
  // connections of enWire
  inActDataStateJumpWires(0)(0) := MuxLookup(inActStateReg, false.B, Array(
    0.U -> inActWriteDoneWireVec(0)(0),
    1.U -> (inActWriteDoneWireVec(0)(2) && inActWriteDoneWireVec(1)(1)),
    3.U -> inActWriteDoneWireVec(0)(1)
  ))
  inActDataStateJumpWires(0)(1) := MuxLookup(inActStateReg, false.B, Array(
    0.U -> (inActWriteDoneWireVec(0)(3) && inActWriteDoneWireVec(1)(2) && inActWriteDoneWireVec(2)(1)),
    1.U -> inActWriteDoneWireVec(2)(2),
    2.U -> (inActWriteDoneWireVec(1)(2) && inActWriteDoneWireVec(2)(1))
  ))
  inActDataStateJumpWires(0)(2) := true.B
  inActDataStateJumpWires(1)(0) := MuxLookup(inActStateReg, false.B, Array(
    0.U -> (inActWriteDoneWireVec(0)(1) && inActWriteDoneWireVec(1)(0)),
    1.U -> inActWriteDoneWireVec(0)(0),
    2.U -> inActWriteDoneWireVec(1)(0)
  ))
  inActDataStateJumpWires(1)(1) := MuxLookup(inActStateReg, false.B, Array(
    0.U -> (inActWriteDoneWireVec(1)(3) && inActWriteDoneWireVec(2)(2)),
    1.U -> (inActWriteDoneWireVec(0)(2) && inActWriteDoneWireVec(1)(2) && inActWriteDoneWireVec(0)(3)),
    2.U -> (inActWriteDoneWireVec(1)(3) && inActWriteDoneWireVec(2)(2))
  ))
  inActDataStateJumpWires(1)(2) := MuxLookup(inActStateReg, false.B, Array(
    0.U -> true.B,
    1.U -> inActWriteDoneWireVec(2)(3),
    2.U -> true.B
  ))
  inActDataStateJumpWires(2)(0) := MuxLookup(inActStateReg, false.B, Array(
    0.U -> (inActWriteDoneWireVec(0)(2) && inActWriteDoneWireVec(1)(1) && inActWriteDoneWireVec(2)(0)),
    1.U -> (inActWriteDoneWireVec(0)(1) && inActWriteDoneWireVec(1)(0)),
    2.U -> inActWriteDoneWireVec(0)(0)
  ))
  inActDataStateJumpWires(2)(1) := MuxLookup(inActStateReg, false.B, Array(
    0.U -> inActWriteDoneWireVec(2)(3),
    1.U -> (inActWriteDoneWireVec(1)(2) && inActWriteDoneWireVec(2)(1)),
    2.U -> (inActWriteDoneWireVec(0)(3) && inActWriteDoneWireVec(1)(1) && inActWriteDoneWireVec(2)(0))
  ))
  inActDataStateJumpWires(2)(2) := MuxLookup(inActStateReg, false.B, Array(
    0.U -> true.B,
    1.U -> true.B,
    2.U -> inActWriteDoneWireVec(2)(3)
  ))
  // state machine of three inActDataIOs
  for (i <- inActDataIOStateRegs.indices) {
    switch(inActDataIOStateRegs(i)) {
      is (inActZero) {
        when (inActDataStateJumpWires(i)(0)) {
          inActDataIOStateRegs(i) := inActOne
        }
      }
      is (inActOne) {
        when (inActDataStateJumpWires(i)(1)) {
          inActWriteDoneRegVec(i) := inActTwo
        }
      }
      is (inActTwo) {
        when (inActDataStateJumpWires(i)(2)) {
          inActWriteDoneRegVec(i) := inActZero
        }
      }
    }
  }
}

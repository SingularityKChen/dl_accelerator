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
  private val peArray = Seq.fill(peRowNum, peColNum){Module(new ProcessingElement(debug = debug)).io}
  peArray.zipWithIndex.foreach({case (pe, idx) =>
    pe.zipWithIndex.foreach({ case (o, i) => o.suggestName(s"pe($idx)($i)")
    })})
  private val muxInPSumWire = Wire(Vec(peColNum, Decoupled(UInt(psDataWidth.W))))
  muxInPSumWire.suggestName("muxInPsumWire")
  private val muxInActDataWire = Wire(Vec(peRowNum, Vec(peColNum, new CSCStreamIO(inActAdrWidth, inActDataWidth))))
  muxInActDataWire.suggestName("muxInActDataWire")
  // inActRoutingMode: whether the input activations should be broad-cast;
  // true then all PEs' data receive from the same router whose index equals to outDataSel's value;
  private val inActRoutingMode = Wire(Bool())
  // inActMuxDiagnIdxWire: correspond to each diagonal, to choose data read in;
  // 0-2 to inActCluster dataPath's responding index, 3 means wait for a while
  private val inActBroadCastIdxWire = Wire(UInt(2.W))
  // muxInPSumSelWire: true, then ; false, then
  private val muxInPSumSelWire = Wire(Vec(peColNum , Bool()))
  muxInPSumSelWire.suggestName("muxInPSumSelWire")
  private val configRegs = VecInit(Seq.fill(6){RegInit(0.U)}) // to store GNMFCS
  configRegs <> io.ctrlPath.configIOs
  private val f2Inc = Wire(Bool())
  f2Inc := configRegs(3) =/= io.ctrlPath.configIOs(3) // if neq, then f2 increases
  private val thePELoadWires = Seq.fill(peRowNum, peColNum){Wire(Bool())}
  thePELoadWires.zipWithIndex.foreach({ case (bools, i) =>
    bools.zipWithIndex.foreach({ case (bool, j) => bool.suggestName(s"thePELoadWires($i)($j)")})
  })
  private val inActWriteEnWires = Seq.fill(peRowNum, peColNum){Wire(Bool())} // inAct address and data writeEn wires
  thePELoadWires.zipWithIndex.foreach({ case (bools, i) =>
    bools.zipWithIndex.foreach({ case (bool, j) => bool.suggestName(s"thePELoadWires($i)($j)")})
  })
  private def inActWeightConnection(peIO: CSCStreamIO, connectIO: CSCStreamIO): Any = {
    Seq(peIO.adrIOs, peIO.dataIOs).zip(Seq(connectIO.adrIOs, connectIO.dataIOs)).foreach({
      case (x, y) =>
        x.data <> y.data
    })
  }
  for (i <- 0 until peColNum) {
    // connect output partial sum produced by the PE at the head of each column to one output partial sum top IO
    io.dataPath.pSumIO.outIOs(i) <> peArray.head(i).dataStream.opsIO
    peArray.head(i).dataStream.ipsIO <> peArray(1)(i).dataStream.opsIO
    peArray(1)(i).dataStream.ipsIO <> peArray.last(i).dataStream.opsIO
    // connect input partial sum from top IO to the PE at the tail of each column with the signal after Mux
    peArray.last(i).dataStream.ipsIO <> muxInPSumWire(i)
    when (muxInPSumSelWire(i)) {
      muxInPSumWire(i) <> io.dataPath.pSumIO.inIOs(i)
      io.dataPath.routerInPSumToPEIO(i).ready := false.B
    } .otherwise {
      muxInPSumWire(i) <> io.dataPath.routerInPSumToPEIO(i)
      io.dataPath.pSumIO.inIOs(i).ready := false.B
    }
    for (j <- 0 until peRowNum) {
      peArray(j)(i).dataStream.inActIOs.adrIOs.data.valid := inActWriteEnWires(j)(i) && muxInActDataWire(j)(i).adrIOs.data.valid
      peArray(j)(i).dataStream.inActIOs.dataIOs.data.valid := inActWriteEnWires(j)(i) && muxInActDataWire(j)(i).dataIOs.data.valid
      inActWeightConnection(peArray(j)(i).dataStream.weightIOs, io.dataPath.weightIO(j))
      inActWeightConnection(peArray(j)(i).dataStream.inActIOs, muxInActDataWire(j)(i))
    }
  }
  muxInPSumSelWire.foreach(x => x := io.ctrlPath.pSumCtrlSel.inDataSel) // TODO: check
  inActRoutingMode := io.ctrlPath.inActCtrlSel.inDataSel // true for broad-cast
  inActBroadCastIdxWire := io.ctrlPath.inActCtrlSel.outDataSel
  // state machine of signal PE
  private val onePEIdle :: onePETrans :: onePEDoing :: onePEWaitPS :: Nil = Enum(4)
  private val thePEStateRegs = Seq.fill(peRowNum, peColNum){RegInit(onePEIdle)}
  thePEStateRegs.zipWithIndex.foreach({ case (ints, i) =>
    ints.zipWithIndex.foreach({ case (int, j) => int.suggestName(s"thePEStateRegs($i)($j)")
    })})
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
  inActStateReg.suggestName("inActStateReg")
  private val inActDataIOStateRegs = Seq.fill(inActRouterNum){RegInit(inActZero)} // three inIOs
  inActDataIOStateRegs.zipWithIndex.foreach({ case (int, i) => int.suggestName(s"inActDataIOStateRegs[$i]")})
  private val inActDataIOZeroWires = WireInit(VecInit(Seq.fill(inActRouterNum){false.B}))
  inActDataIOZeroWires.zipWithIndex.foreach({ case (bool, i) => bool.suggestName(s"inActDataIOZeroWires[$i]")})
  private val inActDataIOOneWires = WireInit(VecInit(Seq.fill(inActRouterNum){false.B}))
  inActDataIOOneWires.zipWithIndex.foreach({ case (bool, i) => bool.suggestName(s"inActDataIOOneWires[$i]")})
  private val inActDataIOTwoWires = WireInit(VecInit(Seq.fill(inActRouterNum){false.B}))
  inActDataIOTwoWires.zipWithIndex.foreach({ case (bool, i) => bool.suggestName(s"inActDataIOTwoWires[$i]")})
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
  private val inActWriteDoneRegVec = Seq.fill(2, peRowNum, peColNum){RegInit(false.B)}
  inActWriteDoneRegVec.zipWithIndex.foreach({ case (seq, i) => seq.zipWithIndex.foreach({ case (bools, j) =>
    bools.zipWithIndex.foreach({ case (bool, k) => bool.suggestName(s"inActWriteDoneRegVec($i)($j)($k)")})
  })})
  private val inActWriteDoneWireVec = Seq.fill(peRowNum, peColNum){Wire(Bool())}
  inActWriteDoneWireVec.zipWithIndex.foreach({ case (bools, i) =>
    bools.zipWithIndex.foreach({ case (bool, j) => bool.suggestName(s"inActWriteDoneWireVec($i)($j)")
    })})
  private val inActStateEqWires = Seq.fill(3){Wire(Bool())}
  inActStateEqWires.zipWithIndex.foreach({ case (bool, i) =>
    bool.suggestName(s"inActStateEqWire${i}Wire")
  })
  inActStateEqWires.head := inActStateReg === inActZero
  inActStateEqWires(1) := inActStateReg === inActOne
  inActStateEqWires(2) := inActStateReg === inActTwo
  // connections of enWires and inActData path
  for (i <- thePEStateRegs.indices) {
    for (j <- thePEStateRegs.head.indices) {
      inActWriteDoneRegVec.head(i)(j) := Mux(peArray(i)(j).padWF.inActWriteFin.adrWriteFin, !inActWriteDoneRegVec.head(i)(j), inActWriteDoneRegVec.head(i)(j))
      inActWriteDoneRegVec(1)(i)(j) := Mux(peArray(i)(j).padWF.inActWriteFin.dataWriteFin, !inActWriteDoneRegVec(1)(i)(j), inActWriteDoneRegVec(1)(i)(j))
      inActWriteDoneWireVec(i)(j) := inActWriteDoneRegVec.head(i)(j) && inActWriteDoneRegVec(1)(i)(j)
      def iPlusJ[DataType<: Data](wireVec: Seq[Seq[DataType]], connectedWireVec: Vec[DataType], oneOrVec: Boolean): Unit = {
        if (oneOrVec) {
          wireVec(i)(j) := connectedWireVec((i + j) % 3)
        } else {
          wireVec(i)(j) <> connectedWireVec((i + j) % 3)
        }
      }
      def iPlusJPlusOne[DataType<: Data](wireVec: Seq[Seq[DataType]], connectedWireVec: Vec[DataType], oneOrVec: Boolean): Unit = {
        if (oneOrVec) {
          wireVec(i)(j) := connectedWireVec((i + j + 1) % 3)
        } else {
          wireVec(i)(j) <> connectedWireVec((i + j + 1) % 3)
        }
      }
      def iPlusJPlusTwo[DataType<: Data](wireVec: Seq[Seq[DataType]], connectedWireVec: Vec[DataType], oneOrVec: Boolean): Unit = {
        if (oneOrVec) {
          wireVec(i)(j) := connectedWireVec((i + j + 2) % 3)
        } else {
          wireVec(i)(j) <> connectedWireVec((i + j + 2) % 3)
        }
      }
      def colZeroConnection[DataType<: Data](wireVec: Seq[Seq[DataType]], connectedWireVec: Vec[DataType], oneOrVec: Boolean): Unit = {
        when (inActStateEqWires.head) {
          iPlusJ[DataType](wireVec = wireVec, connectedWireVec = connectedWireVec, oneOrVec = oneOrVec)
        } .elsewhen (inActStateEqWires(1)) {
          iPlusJPlusOne[DataType](wireVec = wireVec, connectedWireVec = connectedWireVec, oneOrVec = oneOrVec)
        } otherwise {
          iPlusJPlusTwo[DataType](wireVec = wireVec, connectedWireVec = connectedWireVec, oneOrVec = oneOrVec)
        }
      }
      def colOneConnection[DataType<: Data](wireVec: Seq[Seq[DataType]], connectedWireVec: Vec[DataType], oneOrVec: Boolean): Unit = {
        when (inActStateEqWires.head) {
          iPlusJ[DataType](wireVec = wireVec, connectedWireVec = connectedWireVec, oneOrVec = oneOrVec)
        } .elsewhen (inActStateEqWires(1)) {
          iPlusJPlusOne[DataType](wireVec = wireVec, connectedWireVec = connectedWireVec, oneOrVec = oneOrVec)
        } otherwise {
          iPlusJ[DataType](wireVec = wireVec, connectedWireVec = connectedWireVec, oneOrVec = oneOrVec)
        }
      }
      def colTwoConnection[DataType <: Data](wireVec: Seq[Seq[DataType]], connectedWireVec: Vec[DataType], oneOrVec: Boolean): Unit = {
        when (inActStateEqWires.head) {
          iPlusJ[DataType](wireVec = wireVec, connectedWireVec = connectedWireVec, oneOrVec = oneOrVec)
        } .elsewhen (inActStateEqWires(1)) {
          iPlusJPlusTwo[DataType](wireVec = wireVec, connectedWireVec = connectedWireVec, oneOrVec = oneOrVec)
        } otherwise {
          iPlusJ[DataType](wireVec = wireVec, connectedWireVec = connectedWireVec, oneOrVec = oneOrVec)
        }
      }
      if (i == 0) {
        colZeroConnection[CSCStreamIO](muxInActDataWire, io.dataPath.inActIO, oneOrVec = false)
        if (j == 3) {
          colZeroConnection[Bool](inActWriteEnWires, inActDataIOOneWires, oneOrVec = true)
        } else {
          colZeroConnection[Bool](inActWriteEnWires, inActDataIOZeroWires, oneOrVec = true)
        }
      }
      if (i == 1) {
        colOneConnection[CSCStreamIO](muxInActDataWire, io.dataPath.inActIO, oneOrVec = false)
        if (j == 0) {
          colOneConnection[Bool](inActWriteEnWires, inActDataIOZeroWires, oneOrVec = true)
        } else if (j == 1) {
          inActWriteEnWires(i)(j) := MuxLookup(inActStateReg, false.B, Array(
            inActZero -> inActDataIOZeroWires((i + j) % 3),
            inActOne -> inActDataIOZeroWires((i + j + 1) % 3),
            inActTwo -> inActDataIOOneWires((i + j) % 3)
          ))
        } else {
          colOneConnection[Bool](inActWriteEnWires, inActDataIOOneWires, oneOrVec = true)
        }
      }
      if (i == 2) {
        colTwoConnection[CSCStreamIO](muxInActDataWire, io.dataPath.inActIO, oneOrVec = false)
        if (j == 0) {
          inActWriteEnWires(i)(j) := MuxLookup(inActStateReg, false.B, Array(
            inActZero -> inActDataIOZeroWires((i + j) % 3),
            inActOne -> inActDataIOOneWires((i + j + 2) % 3),
            inActTwo -> inActDataIOOneWires((i + j) % 3)
          ))
        } else if (j == 3) {
          inActWriteEnWires(i)(j) := MuxLookup(inActStateReg, false.B, Array(
            inActZero -> inActDataIOOneWires((i + j) % 3),
            inActOne -> inActDataIOTwoWires((i + j + 2) % 3),
            inActTwo -> inActDataIOTwoWires((i + j) % 3)
          ))
        } else {
          colTwoConnection[Bool](inActWriteEnWires, inActDataIOOneWires, oneOrVec = true)
        }
      }
    }
  }
  private val inActDataStateJumpWires = Seq.fill(inActRouterNum, 3){Wire(Bool())}
  inActDataStateJumpWires.zipWithIndex.foreach({ case (bools, i) =>
    bools.zipWithIndex.foreach({ case (bool, j) => bool.suggestName(s"inActDataStateJumpWires($i)($j)")})
  })
  // connections of enWire
  inActDataStateJumpWires.head.head := MuxLookup(inActStateReg, false.B, Array(
    inActZero -> inActWriteDoneWireVec.head.head,
    inActOne -> (inActWriteDoneWireVec.head(2) && inActWriteDoneWireVec(1)(1)),
    inActTwo -> inActWriteDoneWireVec.head(1)
  ))
  inActDataStateJumpWires.head(1) := MuxLookup(inActStateReg, false.B, Array(
    inActZero -> (inActWriteDoneWireVec.head(3) && inActWriteDoneWireVec(1)(2) && inActWriteDoneWireVec(2)(1)),
    inActOne -> inActWriteDoneWireVec(2)(2),
    inActTwo -> (inActWriteDoneWireVec(1)(2) && inActWriteDoneWireVec(2)(1))
  ))
  inActDataStateJumpWires.head(2) := true.B
  inActDataStateJumpWires(1).head := MuxLookup(inActStateReg, false.B, Array(
    inActZero -> (inActWriteDoneWireVec.head(1) && inActWriteDoneWireVec(1).head),
    inActOne -> inActWriteDoneWireVec.head.head,
    inActTwo -> inActWriteDoneWireVec(1).head
  ))
  inActDataStateJumpWires(1)(1) := MuxLookup(inActStateReg, false.B, Array(
    inActZero -> (inActWriteDoneWireVec(1)(3) && inActWriteDoneWireVec(2)(2)),
    inActOne -> (inActWriteDoneWireVec.head(2) && inActWriteDoneWireVec(1)(2) && inActWriteDoneWireVec.head(3)),
    inActTwo -> (inActWriteDoneWireVec(1)(3) && inActWriteDoneWireVec(2)(2))
  ))
  inActDataStateJumpWires(1)(2) := MuxLookup(inActStateReg, false.B, Array(
    inActZero -> true.B,
    inActOne -> inActWriteDoneWireVec(2)(3),
    inActTwo -> true.B
  ))
  inActDataStateJumpWires(2).head := MuxLookup(inActStateReg, false.B, Array(
    inActZero -> (inActWriteDoneWireVec.head(2) && inActWriteDoneWireVec(1)(1) && inActWriteDoneWireVec(2).head),
    inActOne -> (inActWriteDoneWireVec.head(1) && inActWriteDoneWireVec(1).head),
    inActTwo -> inActWriteDoneWireVec.head.head
  ))
  inActDataStateJumpWires(2)(1) := MuxLookup(inActStateReg, false.B, Array(
    inActZero -> inActWriteDoneWireVec(2)(3),
    inActOne -> (inActWriteDoneWireVec(1)(2) && inActWriteDoneWireVec(2)(1)),
    inActTwo -> (inActWriteDoneWireVec.head(3) && inActWriteDoneWireVec(1)(1) && inActWriteDoneWireVec(2).head)
  ))
  inActDataStateJumpWires(2)(2) := MuxLookup(inActStateReg, false.B, Array(
    inActZero -> true.B,
    inActOne -> true.B,
    inActTwo -> inActWriteDoneWireVec(2)(3)
  ))
  // state machine of three inActDataIOs
  for (i <- inActDataIOStateRegs.indices) {
    switch(inActDataIOStateRegs(i)) {
      is (inActZero) {
        when (inActDataStateJumpWires(i).head) {
          inActDataIOStateRegs(i) := inActOne
        }
      }
      is (inActOne) {
        when (inActDataStateJumpWires(i)(1)) {
          inActDataIOStateRegs(i) := inActTwo
        }
      }
      is (inActTwo) {
        when (inActDataStateJumpWires(i)(2)) {
          inActDataIOStateRegs(i) := inActZero
        }
      }
    }
  }
  // pSumControl
  peArray.foreach(_.foreach({ x => // FIXME
    x.topCtrl.pSumEnqOrProduct.bits := true.B
    x.topCtrl.pSumEnqOrProduct.valid := true.B
  }))
}

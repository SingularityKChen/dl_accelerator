package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe._

class PECluster(debug: Boolean) extends Module with ClusterConfig {
  val diagnNum: Int = peColNum + peRowNum - 1
  val io: PEClusterIO = IO(new PEClusterIO)
  // io.dataPath.pSumDataFromSouthernIO: pSum from southern PE Array
  // io.dataPath.pSumDataFromSouthernIO: data from pSumRouter
  // io.ctrlPath.inActCtrlSel.inDataSel indicates whether the input activations should be broad-cast;
  //   true then broad-cast, and read the index of router that should be broad-casted; false then only get the
  //   corresponding index of input activations router;
  // io.ctrlPath.inActCtrlSel.outDataSel should be assigned to the index of router port when broad-cast;
  private val peArray = Seq.fill(peRowNum, peColNum){Module(new ProcessingElement(debug = debug)).io}
  peArray.zipWithIndex.foreach({case (pe, idx) =>
    pe.zipWithIndex.foreach({ case (o, i) => o.suggestName(s"pe($idx)($i)")
    })})
  private val peClusterInAct = Module(new PEClusterInAct).io
  peClusterInAct.suggestName("peClusterInAct")
  // connections of peClusterInAct
  peClusterInAct.topToInAct.configF2Inc := io.ctrlPath.configF2Inc
  peClusterInAct.topToInAct.inActCtrlSel <> io.ctrlPath.inActCtrlSel
  peClusterInAct.inActWriteFinVec.zip(peArray).foreach({ case (os, peCol) =>
    os.zip(peCol).foreach({ case (o, onePE) =>
      o := onePE.padWF.inActWriteFin
    })})
  peClusterInAct.inActToArrayData.inActIO <> io.dataPath.inActIO
  // connections of peClusterPSum
  private val peClusterPSumCtrl = Module(new PEClusterPSumController).io
  peClusterPSumCtrl.suggestName("peClusterPSumCtrl")
  peClusterPSumCtrl.doEn := io.ctrlPath.doEn
  private val muxInPSumDataWire = Wire(Vec(peColNum, Decoupled(UInt(psDataWidth.W))))
  muxInPSumDataWire.suggestName("muxInPSumDataWire")
  private val muxInPSumSelWire = Seq.fill(peColNum){Wire(Bool())}
  muxInPSumSelWire.zipWithIndex.foreach({ case (bool, i) => bool.suggestName(s"muxInPSumSelWire$i")})
  private def inActWeightConnection(peIO: CSCStreamIO, connectIO: CSCStreamIO): Any = {
    Seq(peIO.adrIOs, peIO.dataIOs).zip(Seq(connectIO.adrIOs, connectIO.dataIOs)).foreach({
      case (x, y) =>
        x.data <> y.data
    })
  }
  muxInPSumSelWire.foreach(x => x := io.ctrlPath.pSumCtrlSel.inDataSel) // TODO: check
  for (j <- 0 until peColNum) {
    // connect output partial sum produced by the PE at the head of each column to one output partial sum top IO
    io.dataPath.pSumIO.outIOs(j) <> peArray.head(j).dataStream.opsIO
    peArray.head(j).dataStream.ipsIO <> peArray(1)(j).dataStream.opsIO
    peArray(1)(j).dataStream.ipsIO <> peArray.last(j).dataStream.opsIO
    // connect input partial sum from top IO to the PE at the tail of each column with the signal after Mux
    peArray.last(j).dataStream.ipsIO <> muxInPSumDataWire(j)
    when (muxInPSumSelWire(j)) {
      muxInPSumDataWire(j) <> io.dataPath.pSumIO.inIOs(j) // from router
      io.dataPath.pSumDataFromSouthernIO(j).ready := false.B
    } .otherwise {
      muxInPSumDataWire(j) <> io.dataPath.pSumDataFromSouthernIO(j) // from southern PEArray
      io.dataPath.pSumIO.inIOs(j).ready := false.B
    }
    for (i <- 0 until peRowNum) {
      inActWeightConnection(peArray(i)(j).dataStream.weightIOs, io.dataPath.weightIO(i))
      inActWeightConnection(peArray(i)(j).dataStream.inActIOs, peClusterInAct.inActToArrayData.muxInActData(i)(j))
      peArray(i)(j).topCtrl.doLoadEn := io.ctrlPath.doEn
      // pSumControl
      peArray(i)(j).topCtrl.pSumEnqEn := peClusterPSumCtrl.pSumEnqEn(i)(j)
      peClusterPSumCtrl.pSumWF(i)(j) := peArray(i)(j).padWF.pSumWriteFin
    }
  }
}

class PEClusterPSumController extends Module with ClusterConfig {
  val io: PEClusterPSumCtrlIO = IO(new PEClusterPSumCtrlIO)
  // the machine state of each PSum's column
  // pSumZero, one, two enable each row's Enq signal,
  // i.e., when pSumZero, then current column, row zero pSumEnqEn assign to true.B, and row one's opsIO.ready === true.B
  // Note: we assume when doLoadEn, the PSum from the head of each column has been read. TODO: check
  require(peRowNum == 3, "peRowNum should be 3 or you need change the state machine")
  private val pSumIdle :: pSumZero :: pSumOne :: pSumTwo :: Nil = Enum(4)
  private val pSumColStateRegs = Seq.fill(peColNum){RegInit(pSumIdle)}
  pSumColStateRegs.zipWithIndex.foreach({ case (pSumStateReg, colIdx) =>
    pSumStateReg.suggestName(s"pSumCol${colIdx}StateReg")
    io.pSumEnqEn(0)(colIdx) := pSumStateReg === pSumZero
    io.pSumEnqEn(1)(colIdx) := pSumStateReg === pSumOne
    io.pSumEnqEn(2)(colIdx) := pSumStateReg === pSumTwo
    switch (pSumStateReg) {
      is (pSumIdle) {
        when (io.doEn) {
          pSumStateReg := pSumZero
        }
      }
      is (pSumZero) {
        when (io.pSumWF(0)(colIdx)) {
          pSumStateReg := pSumOne
        }
      }
      is (pSumOne) {
        when (io.pSumWF(1)(colIdx)) {
          pSumStateReg := pSumTwo
        }
      }
      is (pSumTwo) {
        when (io.pSumWF(2)(colIdx)) {
          pSumStateReg := pSumIdle
        }
      }
    }
  })
}

class PEClusterInAct extends Module with ClusterConfig {
  val io: PEClusterInActIO = IO(new PEClusterInActIO)
  private val dataPart = Module(new PEClusterInActDataConnections).io
  private val ctrlPart = Module(new PEClusterInActController).io// connections of enWires
  io.inActWriteFinVec <> ctrlPart.inActWriteFinVec
  ctrlPart.inActStateEq <> dataPart.inActStateEq
  ctrlPart.topToInAct <> io.topToInAct
  dataPart.inActToArrayData.inActIO <> io.inActToArrayData.inActIO
  for (i <- 0 until peRowNum) {
    for (j <- 0 until peColNum) {
      val theTopMux = io.inActToArrayData.muxInActData(i)(j)
      val theSubMux = dataPart.inActToArrayData.muxInActData(i)(j)
      theTopMux.adrIOs.data.valid := theSubMux.adrIOs.data.valid && ctrlPart.writeEn(i)(j)
      theTopMux.dataIOs.data.valid := theSubMux.dataIOs.data.valid && ctrlPart.writeEn(i)(j)
      theTopMux.adrIOs.data.bits := theSubMux.adrIOs.data.bits
      theTopMux.dataIOs.data.bits := theSubMux.dataIOs.data.bits
      theSubMux.adrIOs.data.ready := theTopMux.adrIOs.data.ready
      theSubMux.dataIOs.data.ready := theTopMux.dataIOs.data.ready
    }
  }
}

class PEClusterInActDataConnections extends HasConnectAllExpRdModule with ClusterConfig {
  val io: PEClusterInActDataIO = IO(new PEClusterInActDataIO)
  private val muxInActDataWire = Wire(Vec(peRowNum, Vec(peColNum, new CSCStreamIO(inActAdrWidth, inActDataWidth))))
  muxInActDataWire.suggestName("muxInActDataWire")
  // oneInActIOReadyWires: ready wires used for ready signal, adr/data (2), inIO number (3)
  private val oneInActIOReadyWires = Seq.fill(2, inActRouterNum) {Wire(Bool())}
  oneInActIOReadyWires.zipWithIndex.foreach({ case (bools, i) => bools.zipWithIndex.foreach({ case (bool, j) =>
    bool.suggestName(s"oneInAct${i}IOReadyWires$j")})
  })
  for (i <- 0 until peRowNum) {
    for (j <- 0 until peColNum) {
      val iPlusJMod = (i + j) % 3
      val iPlusJPlusOneMod = (i + j + 1) % 3
      val iPlusJPlusTwoMod = (i + j + 2) %3
      def colZeroConnectionCSC(wireVec: Seq[Seq[CSCStreamIO]], connectedWireVec: Vec[CSCStreamIO]): Unit = {
        when (io.inActStateEq.head) {
          connectAllExceptReady(slaverIO = wireVec(i)(j), masterIO = connectedWireVec(iPlusJMod))
        } .elsewhen (io.inActStateEq(1)) {
          connectAllExceptReady(slaverIO = wireVec(i)(j), masterIO = connectedWireVec(iPlusJPlusOneMod))
        } .otherwise {
          connectAllExceptReady(slaverIO = wireVec(i)(j), masterIO = connectedWireVec(iPlusJPlusTwoMod))
        }
      }
      def colOneConnectionCSC(wireVec: Seq[Seq[CSCStreamIO]], connectedWireVec: Vec[CSCStreamIO]): Unit = {
        when (io.inActStateEq.head) {
          connectAllExceptReady(slaverIO = wireVec(i)(j), masterIO = connectedWireVec(iPlusJMod))
        } .elsewhen (io.inActStateEq(1)) {
          connectAllExceptReady(slaverIO = wireVec(i)(j), masterIO = connectedWireVec(iPlusJPlusOneMod))
        } .otherwise {
          connectAllExceptReady(slaverIO = wireVec(i)(j), masterIO = connectedWireVec(iPlusJMod))
        }
      }
      def colTwoConnectionCSC(wireVec: Seq[Seq[CSCStreamIO]], connectedWireVec: Vec[CSCStreamIO]): Unit = {
        when (io.inActStateEq.head) {
          connectAllExceptReady(slaverIO = wireVec(i)(j), masterIO = connectedWireVec(iPlusJMod))
        } .elsewhen (io.inActStateEq(1)) {
          connectAllExceptReady(slaverIO = wireVec(i)(j), masterIO = connectedWireVec(iPlusJPlusTwoMod))
        } .otherwise {
          connectAllExceptReady(slaverIO = wireVec(i)(j), masterIO = connectedWireVec(iPlusJMod))
        }
      }
      if (i == 0) {
        colZeroConnectionCSC(muxInActDataWire, io.inActToArrayData.inActIO)
      }
      if (i == 1) {
        colOneConnectionCSC(muxInActDataWire, io.inActToArrayData.inActIO)
      }
      if (i == 2) {
        colTwoConnectionCSC(muxInActDataWire, io.inActToArrayData.inActIO)
      }
    }
  }
  // connect inActIO's address and data ready signals
  io.inActToArrayData.inActIO.map(x => x.adrIOs).zip(oneInActIOReadyWires.head).foreach({ case (o, bool) =>
    o.data.ready := bool
  })
  io.inActToArrayData.inActIO.map(x => x.dataIOs).zip(oneInActIOReadyWires(1)).foreach({ case (o, bool) =>
    o.data.ready := bool
  })
  // connections of ready wires
  private def reduceMuxInActAdr(indexSeq: Seq[(Int, Int)]): Bool = {
    indexSeq.map({case (a, b) => muxInActDataWire(a)(b)}).map(x => x.adrIOs.data.ready).reduce(_ && _)
  }
  private def reduceMuxInActData(indexSeq: Seq[(Int, Int)]): Bool = {
    indexSeq.map({case (a, b) => muxInActDataWire(a)(b)}).map(x => x.dataIOs.data.ready).reduce(_ && _)
  }
  private val reduceMuxInActAdr0110 = Wire(Bool())
  reduceMuxInActAdr0110 := reduceMuxInActAdr(Seq((0, 1), (1, 0)))
  private val reduceMuxInActData0110 = Wire(Bool())
  reduceMuxInActData0110 := reduceMuxInActData(Seq((0, 1), (1, 0)))
  private val reduceMuxInActAdr1322 = Wire(Bool())
  reduceMuxInActAdr1322 := reduceMuxInActAdr(Seq((1, 3), (2, 2)))
  private val reduceMuxInActData1322 = Wire(Bool())
  reduceMuxInActData1322 := reduceMuxInActData(Seq((1, 3), (2, 2)))
  when (io.inActStateEq.head) {
    oneInActIOReadyWires.head.head := muxInActDataWire(0)(0).adrIOs.data.ready ||
      reduceMuxInActAdr(Seq((0, 3), (1, 2), (2, 1)))
    oneInActIOReadyWires.head(1) := reduceMuxInActAdr0110 || reduceMuxInActAdr1322
    oneInActIOReadyWires.head(2) := reduceMuxInActAdr(Seq((0, 2), (1, 1), (2, 0))) ||
      muxInActDataWire(2)(3).adrIOs.data.ready
    oneInActIOReadyWires(1).head := muxInActDataWire(0)(0).dataIOs.data.ready ||
      reduceMuxInActData(Seq((0, 3), (1, 2), (2, 1)))
    oneInActIOReadyWires(1)(1) := reduceMuxInActData0110 || reduceMuxInActData1322
    oneInActIOReadyWires(1)(2) := reduceMuxInActData(Seq((0, 2), (1, 1), (2, 0))) ||
      muxInActDataWire(2)(3).dataIOs.data.ready
  } .elsewhen (io.inActStateEq(1)) {
    oneInActIOReadyWires.head.head := reduceMuxInActAdr(Seq((0, 2), (1, 1))) || muxInActDataWire(2)(2).adrIOs.data.ready
    oneInActIOReadyWires.head(1) := muxInActDataWire(0)(0).adrIOs.data.ready ||
      reduceMuxInActAdr(Seq((0, 3), (1, 2), (2, 0))) || muxInActDataWire(2)(3).adrIOs.data.ready
    oneInActIOReadyWires.head(2) := reduceMuxInActAdr0110 || reduceMuxInActAdr(Seq((1, 3), (2, 1)))
    oneInActIOReadyWires(1).head := reduceMuxInActData(Seq((0, 2), (1, 1))) || muxInActDataWire(2)(2).dataIOs.data.ready
    oneInActIOReadyWires(1)(1) := muxInActDataWire(0)(0).dataIOs.data.ready ||
      reduceMuxInActAdr(Seq((0, 3), (1, 2), (2, 0))) || muxInActDataWire(2)(3).dataIOs.data.ready
    oneInActIOReadyWires(1)(2) := reduceMuxInActData0110 || reduceMuxInActData(Seq((1, 3), (2, 1)))
  } .otherwise {
    oneInActIOReadyWires.head.head := muxInActDataWire(0)(1).adrIOs.data.ready || reduceMuxInActAdr(Seq((1, 2), (2, 1)))
    oneInActIOReadyWires.head(1) := reduceMuxInActAdr(Seq((0, 2), (1, 0))) || reduceMuxInActAdr1322
    oneInActIOReadyWires.head(2) := muxInActDataWire(0)(0).adrIOs.data.ready || reduceMuxInActAdr(Seq((1, 1), (2, 0))) ||
      muxInActDataWire(2)(3).adrIOs.data.ready
    oneInActIOReadyWires(1).head := muxInActDataWire(0)(1).dataIOs.data.ready || reduceMuxInActData(Seq((1, 2), (2, 1)))
    oneInActIOReadyWires(1)(1) := reduceMuxInActData(Seq((0, 2), (1, 0))) || reduceMuxInActData1322
    oneInActIOReadyWires(1)(2) := muxInActDataWire(0)(0).dataIOs.data.ready || reduceMuxInActData(Seq((1, 1), (2, 0))) ||
      muxInActDataWire(2)(3).dataIOs.data.ready
  }
  io.inActToArrayData.muxInActData.zip(muxInActDataWire).foreach({ case (os, os1) => os.zip(os1).foreach({ case (o, o1) =>
    connectAllExceptReady(o, o1)
    o1.adrIOs.data.ready := o.adrIOs.data.ready
    o1.dataIOs.data.ready := o.dataIOs.data.ready
  })})
}

class PEClusterInActController extends Module with ClusterConfig {
  val io: PEClusterInActCtrlIO = IO(new PEClusterInActCtrlIO)// state machine of inAct in the PE Cluster
  private val f2IncWire = Wire(Bool())
  f2IncWire := io.topToInAct.configF2Inc
  // inActRoutingMode: whether the input activations should be broad-cast;
  // true then all PEs' data receive from the same router whose index equals to outDataSel's value;
  private val inActRoutingMode = Wire(Bool()) // FIXME: unused
  inActRoutingMode.suggestName("inActRoutingMode")
  // inActMuxDiagnIdxWire: correspond to each diagonal, to choose data read in;
  // 0-2 to inActCluster dataPath's responding index, 3 means wait for a while
  private val inActBroadCastIdxWire = Wire(UInt(2.W))  // FIXME: unused
  inActBroadCastIdxWire.suggestName("inActBroadCastIdxWire")
  // muxInPSumSelWire: true, then from pSumRouter;
  //                   false, then from southern PEArray
  inActRoutingMode := io.topToInAct.inActCtrlSel.inDataSel // true for broad-cast
  inActBroadCastIdxWire := io.topToInAct.inActCtrlSel.outDataSel
  private val inActWriteEnWires = Seq.fill(peRowNum, peColNum){Wire(Bool())} // inAct address and data writeEn wires
  inActWriteEnWires.zipWithIndex.foreach({ case (bools, i) =>
    bools.zipWithIndex.foreach({ case (bool, j) => bool.suggestName(s"inActWriteEnWires($i)($j)")})
  })
  private val inActZero :: inActOne :: inActTwo :: Nil = Enum(3)
  // inActZero: none new
  // inActOne: one new row of computation
  // inActTwo: two new rows of computation
  private val inActStateReg = RegInit(inActZero)
  inActStateReg.suggestName("inActStateReg")
  private val inActDataIOStateRegs = Seq.fill(inActRouterNum){RegInit(inActZero)} // three inIOs
  inActDataIOStateRegs.zipWithIndex.foreach({ case (int, i) => int.suggestName(s"inActDataIOStateRegs[$i]")})
  private val inActDataIOZeroWires = Seq.fill(inActRouterNum){Wire(Bool())}
  inActDataIOZeroWires.zipWithIndex.foreach({ case (bool, i) => bool.suggestName(s"inActDataIOZeroWires[$i]")})
  private val inActDataIOOneWires = Seq.fill(inActRouterNum){Wire(Bool())}
  inActDataIOOneWires.zipWithIndex.foreach({ case (bool, i) => bool.suggestName(s"inActDataIOOneWires[$i]")})
  private val inActDataIOTwoWires = Seq.fill(inActRouterNum){Wire(Bool())}
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
  threeState(inActStateReg, changeWire0 = f2IncWire, changeWire1 = f2IncWire, changeWire2 = f2IncWire)
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
  // connections of enWires
  for (i <- 0 until peRowNum) {
    for (j <- 0 until peColNum) {
      inActWriteDoneRegVec.head(i)(j) := Mux(io.inActWriteFinVec(i)(j).adrWriteFin,
        !inActWriteDoneRegVec.head(i)(j), inActWriteDoneRegVec.head(i)(j))
      inActWriteDoneRegVec(1)(i)(j) := Mux(io.inActWriteFinVec(i)(j).dataWriteFin,
        !inActWriteDoneRegVec(1)(i)(j), inActWriteDoneRegVec(1)(i)(j))
      inActWriteDoneWireVec(i)(j) := inActWriteDoneRegVec.head(i)(j) && inActWriteDoneRegVec(1)(i)(j)
      val iPlusJMod = (i + j) % 3
      val iPlusJPlusOneMod = (i + j + 1) % 3
      val iPlusJPlusTwoMod = (i + j + 2) %3
      def colZeroConnectionBool(wireVec: Seq[Seq[Bool]], connectedWireVec: Seq[Bool]): Unit = {
        when (inActStateEqWires.head) {
          wireVec(i)(j) := connectedWireVec(iPlusJMod)
        } .elsewhen (inActStateEqWires(1)) {
          wireVec(i)(j) := connectedWireVec(iPlusJPlusOneMod)
        } .otherwise {
          wireVec(i)(j) := connectedWireVec(iPlusJPlusTwoMod)
        }
      }
      def colOneConnectionBool(wireVec: Seq[Seq[Bool]], connectedWireVec: Seq[Bool]): Unit = {
        when (inActStateEqWires.head) {
          wireVec(i)(j) := connectedWireVec(iPlusJMod)
        } .elsewhen (inActStateEqWires(1)) {
          wireVec(i)(j) := connectedWireVec(iPlusJPlusOneMod)
        } .otherwise {
          wireVec(i)(j) := connectedWireVec(iPlusJMod)
        }
      }
      def colTwoConnectionBool(wireVec: Seq[Seq[Bool]], connectedWireVec: Seq[Bool]): Unit = {
        when (inActStateEqWires.head) {
          wireVec(i)(j) := connectedWireVec(iPlusJMod)
        } .elsewhen (inActStateEqWires(1)) {
          wireVec(i)(j) := connectedWireVec(iPlusJPlusTwoMod)
        } .otherwise {
          wireVec(i)(j) := connectedWireVec(iPlusJMod)
        }
      }
      if (i == 0) {
        if (j == 3) {
          colZeroConnectionBool(inActWriteEnWires, inActDataIOOneWires)
        } else {
          colZeroConnectionBool(inActWriteEnWires, inActDataIOZeroWires)
        }
      }
      if (i == 1) {
        if (j == 0) {
          colOneConnectionBool(inActWriteEnWires, inActDataIOZeroWires)
        } else if (j == 1) {
          inActWriteEnWires(i)(j) := MuxLookup(inActStateReg, false.B, Array(
            inActZero -> inActDataIOZeroWires(iPlusJMod),
            inActOne -> inActDataIOZeroWires(iPlusJPlusOneMod),
            inActTwo -> inActDataIOOneWires(iPlusJMod)
          ))
        } else {
          colOneConnectionBool(inActWriteEnWires, inActDataIOOneWires)
        }
      }
      if (i == 2) {
        if (j == 0) {
          inActWriteEnWires(i)(j) := MuxLookup(inActStateReg, false.B, Array(
            inActZero -> inActDataIOZeroWires(iPlusJMod),
            inActOne -> inActDataIOOneWires(iPlusJPlusTwoMod),
            inActTwo -> inActDataIOOneWires(iPlusJMod)
          ))
        } else if (j == 3) {
          inActWriteEnWires(i)(j) := MuxLookup(inActStateReg, false.B, Array(
            inActZero -> inActDataIOOneWires(iPlusJMod),
            inActOne -> inActDataIOTwoWires(iPlusJPlusTwoMod),
            inActTwo -> inActDataIOTwoWires(iPlusJMod)
          ))
        } else {
          colTwoConnectionBool(inActWriteEnWires, inActDataIOOneWires)
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
  io.writeEn.zip(inActWriteEnWires).foreach({ case (outputs, enWires) =>
    outputs.zip(enWires).foreach({ case (output, enWires) => output := enWires
    })})
  io.inActStateEq.zip(inActStateEqWires).foreach({ case (bool, bool1) => bool := bool1})
}

class PEClusterTopToInActCtrlIO extends Bundle {
  val configF2Inc: Bool = Input(Bool())
  val inActCtrlSel: CommonClusterCtrlBoolUIntIO = Flipped(new CommonClusterCtrlBoolUIntIO)
}

class PEClusterInActToArrayDataIO extends Bundle with ClusterConfig {
  // output bits and valid
  val muxInActData: Vec[Vec[CSCStreamIO]] = Vec(peRowNum, Vec(peColNum, new CSCStreamIO(inActAdrWidth, inActDataWidth)))
  val inActIO: Vec[CSCStreamIO] = Vec(inActRouterNum, Flipped(new CSCStreamIO(inActAdrWidth, inActDataWidth))) // input only
}

class PEClusterInActIO extends Bundle with ClusterConfig {
  val topToInAct = new PEClusterTopToInActCtrlIO
  val inActWriteFinVec: Vec[Vec[CSCWriteFinIO]] = Vec(peRowNum, Vec(peColNum, Flipped(new CSCWriteFinIO))) // input
  val inActToArrayData = new PEClusterInActToArrayDataIO
}

class PEClusterInActDataIO extends Bundle with ClusterConfig {
  val inActToArrayData = new PEClusterInActToArrayDataIO
  val inActStateEq: Vec[Bool] = Vec(3, Input(Bool()))
}

class PEClusterInActCtrlIO extends Bundle with ClusterConfig {
  val topToInAct = new PEClusterTopToInActCtrlIO
  val writeEn: Vec[Vec[Bool]] = Vec(peRowNum, Vec(peColNum, Output(Bool())))
  val inActStateEq: Vec[Bool] = Vec(3, Output(Bool()))
  val inActWriteFinVec: Vec[Vec[CSCWriteFinIO]] = Vec(peRowNum, Vec(peColNum, Flipped(new CSCWriteFinIO))) // input
}

class PEClusterPSumCtrlIO extends Bundle with ClusterConfig {
  val pSumEnqEn: Vec[Vec[Bool]] = Vec(peRowNum, Vec(peColNum, Output(Bool())))
  val doEn: Bool = Input(Bool()) // doEn signal from top
  // pSumWF: receive write finish signals from each PE
  val pSumWF: Vec[Vec[Bool]] = Vec(peRowNum, Vec(peColNum, Input(Bool())))
}
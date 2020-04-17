package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe._

class PECluster(debug: Boolean) extends HasConnectAllExpRdModule with ClusterConfig {
  val io: PEClusterIO = IO(new PEClusterIO)
  // io.dataPath.pSumDataFromSouthernIO: pSum from southern PE Array
  // io.dataPath.pSumDataFromSouthernIO: data from pSumRouter
  // io.ctrlPath.inActCtrlSel.inDataSel indicates whether the input activations should be broad-cast;
  //   true then broad-cast, and read the index of router that should be broad-casted; false then only get the
  //   corresponding index of input activations router;
  // io.ctrlPath.inActCtrlSel.outDataSel should be assigned to the index of router port when broad-cast;
  private val peArray = Seq.fill(peRowNum, peColNum){Module(new ProcessingElement(debug = debug))}
  peArray.zipWithIndex.foreach({case (pe, row) =>
    pe.zipWithIndex.foreach({ case (o, col) => o.suggestName(s"pe($row)($col)")
    })})
  private val peArrayIO = peArray.map(x => x.map(y => y.io))
  private val peClusterInAct = Module(new PEClusterInAct(debug = debug))
  peClusterInAct.suggestName("peClusterInAct")
  private val peClusterInActIO = peClusterInAct.io
  // connections of peClusterInAct
  peClusterInActIO.inActCtrlSel <> io.ctrlPath.inActCtrlSel
  peClusterInActIO.inActWriteFinVec.zip(peArrayIO).foreach({ case (os, peCol) =>
    os.zip(peCol).foreach({ case (o, onePE) =>
      o := onePE.padWF.inActWriteFin
    })})
  peClusterInActIO.inActToArrayData.inActIO <> io.dataPath.inActIO
  private val oneColumnPSumAddFinRegVec = Seq.fill(peColNum){RegInit(false.B)}
  oneColumnPSumAddFinRegVec.zipWithIndex.foreach({ case (bool, i) => bool.suggestName(s"col${i}PSumAddFinReg")})
  private val allColPSumAddFin = oneColumnPSumAddFinRegVec.reduce(_ && _)
  private val onePECalFinReg = Seq.fill(peRowNum, peColNum){RegInit(false.B)}
  private val allCalFinWire = Wire(Bool())
  allCalFinWire := onePECalFinReg.map(_.reduce(_ && _)).reduce(_ && _)
  // connections of peClusterPSum
  private val muxInPSumDataWire = Wire(Vec(peColNum, Decoupled(UInt(psDataWidth.W))))
  muxInPSumDataWire.suggestName("muxInPSumDataWire")
  for (col <- 0 until peColNum) {
    // connect output partial sum produced by the PE at the head of each column to one output partial sum top IO
    io.dataPath.pSumIO.outIOs(col) <> peArrayIO.head(col).dataStream.opsIO
    for (row <- 1 until peRowNum) {
      peArrayIO(row - 1)(col).dataStream.ipsIO <> peArrayIO(row)(col).dataStream.opsIO
    }
    // connect input partial sum from top IO to the PE at the tail of each column with the signal after Mux
    peArrayIO.last(col).dataStream.ipsIO <> muxInPSumDataWire(col)
    // select ips of the tail of each column, true from router, false from southern PEArray
    when (io.ctrlPath.pSumCtrlSel.inDataSel) {
      muxInPSumDataWire(col) <> io.dataPath.pSumIO.inIOs(col) // from router
      io.dataPath.pSumDataFromSouthernIO(col).ready := false.B
    } .otherwise {
      muxInPSumDataWire(col) <> io.dataPath.pSumDataFromSouthernIO(col) // from southern PEArray
      io.dataPath.pSumIO.inIOs(col).ready := false.B
    }
    oneColumnPSumAddFinRegVec(col) := Mux(allColPSumAddFin, false.B,
      // only need to record each head of column as we begin add from the tail
      Mux(peArrayIO.head(col).padWF.pSumAddFin, true.B, oneColumnPSumAddFinRegVec(col))
    )
    for (row <- 0 until peRowNum) {
      connectAllExceptReady(peArrayIO(row)(col).dataStream.weightIOs, io.dataPath.weightIO(row))
      io.dataPath.weightIO(row).adrIOs.data.ready := peArrayIO(row).map(x =>
        x.dataStream.weightIOs.adrIOs.data.ready).reduce(_ && _)
      io.dataPath.weightIO(row).dataIOs.data.ready := peArrayIO(row).map(x =>
        x.dataStream.weightIOs.dataIOs.data.ready).reduce(_ && _)
      peArrayIO(row)(col).dataStream.inActIOs <> peClusterInActIO.inActToArrayData.muxInActData(row)(col)
      peArrayIO(row)(col).topCtrl.doLoadEn := io.ctrlPath.doEn
      // pSumControl
      peArrayIO(row)(col).topCtrl.pSumEnqEn := io.ctrlPath.pSumLoadEn
      onePECalFinReg(row)(col) := Mux(allCalFinWire, false.B,
        Mux(peArrayIO(row)(col).topCtrl.calFinish, true.B, onePECalFinReg(row)(col))
      )
    }
  }
  io.ctrlPath.allPSumAddFin := allColPSumAddFin
  io.ctrlPath.allCalFin := allCalFinWire
  if (debug) {
    for (row <- 0 until peRowNum) {
      for (col <- 0 until peColNum) {
        io.debugIO.eachPEInActValid.head(row)(col) :=
          peClusterInActIO.inActToArrayData.muxInActData(row)(col).adrIOs.data.valid
        io.debugIO.eachPEInActValid.last(row)(col) :=
          peClusterInActIO.inActToArrayData.muxInActData(row)(col).dataIOs.data.valid
        io.debugIO.inActWriteFinVec(row)(col) <> peArrayIO(row)(col).padWF.inActWriteFin
        io.debugIO.eachPETopDebug(row)(col) <> peArrayIO(row)(col).debugIO
      }
    }
    io.debugIO.inActDataIOState <> peClusterInActIO.debugIO.inActDataIOState
  } else {
    io.debugIO <> DontCare
  }
}

class PEClusterInAct(debug: Boolean) extends Module with ClusterConfig {
  val io: PEClusterInActIO = IO(new PEClusterInActIO)
  private val dataPart = Module(new PEClusterInActDataConnections).io
  private val ctrlPart = Module(new PEClusterInActController(debug)).io// connections of enWires
  io.inActWriteFinVec <> ctrlPart.inActWriteFinVec
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
  // inActRoutingMode: whether the input activations should be broad-cast;
  // true then all PEs' data receive from the same router whose index equals to outDataSel's value;
  private val inActRoutingMode = Wire(Bool())
  inActRoutingMode.suggestName("inActRoutingMode")
  private val inActBroadCastIdxWire = Wire(UInt(2.W))
  inActBroadCastIdxWire.suggestName("inActBroadCastIdxWire")
  inActRoutingMode := io.inActCtrlSel.inDataSel // true for broad-cast
  inActBroadCastIdxWire := io.inActCtrlSel.outDataSel
  when (inActRoutingMode) { // when need to broad-cast, then each port of inAct should connect to the same one
    dataPart.inActToArrayData.inActIO.foreach({x =>
      x.adrIOs.data.bits := MuxLookup(io.inActCtrlSel.outDataSel, 0.U, io.inActToArrayData.inActIO.zipWithIndex.map({
        case (o, i) => i.asUInt -> o.adrIOs.data.bits}))
      x.adrIOs.data.valid := MuxLookup(io.inActCtrlSel.outDataSel, false.B, io.inActToArrayData.inActIO.zipWithIndex.map({
        case (o, i) => i.asUInt -> o.adrIOs.data.valid}))
      x.dataIOs.data.bits := MuxLookup(io.inActCtrlSel.outDataSel, 0.U, io.inActToArrayData.inActIO.zipWithIndex.map({
        case (o, i) => i.asUInt -> o.dataIOs.data.bits}))
      x.dataIOs.data.valid := MuxLookup(io.inActCtrlSel.outDataSel, false.B, io.inActToArrayData.inActIO.zipWithIndex.map({
        case (o, i) => i.asUInt -> o.dataIOs.data.valid}))
    })
    io.inActToArrayData.inActIO.zipWithIndex.foreach({case (x, idx1) =>
      x.adrIOs.data.ready := MuxLookup(io.inActCtrlSel.outDataSel, false.B,
        Seq.fill(inActRouterNum){1}.zipWithIndex.map({ case (_, idx) =>
        if (idx1 == idx) idx.asUInt -> dataPart.inActToArrayData.inActIO.map(y => y.adrIOs.data.ready).reduce(_ && _)
        else idx.asUInt -> false.B
      }))
      x.dataIOs.data.ready := MuxLookup(io.inActCtrlSel.outDataSel, false.B,
        Seq.fill(inActRouterNum){1}.zipWithIndex.map({ case (_, idx) =>
        if (idx1 == idx) idx.asUInt -> dataPart.inActToArrayData.inActIO.map(y => y.dataIOs.data.ready).reduce(_ && _)
        else idx.asUInt -> false.B
      }))
    })
  } .otherwise {
    dataPart.inActToArrayData.inActIO <> io.inActToArrayData.inActIO
  }
  if (debug) {
    io.debugIO <> ctrlPart.debugIO
  } else {
    io.debugIO <> DontCare
  }
}

class PEClusterInActDataConnections extends HasConnectAllExpRdModule with ClusterConfig {
  val io: PEClusterInActDataIO = IO(new PEClusterInActDataIO)
  private val muxInActDataWire = Wire(Vec(peRowNum, Vec(peColNum, new CSCStreamIO(inActAdrWidth, inActDataWidth))))
  muxInActDataWire.suggestName("muxInActDataWire")
  // oneInActIOReadyWires: used for each inActIO's ready signal, adr/data (2), inIO number (inActRouterNum)
  private val oneInActIOReadyWires = Seq.fill(2, inActRouterNum) {Wire(Bool())}
  oneInActIOReadyWires.zipWithIndex.foreach({ case (bools, i) => bools.zipWithIndex.foreach({ case (bool, j) =>
    bool.suggestName(s"oneInAct${i}IOReadyWires$j")})
  })
  for (i <- 0 until peRowNum) {
    for (j <- 0 until peColNum) {
      val iPlusJMod = (i + j) % inActRouterNum
      connectAllExceptReady(slaverIO = muxInActDataWire(i)(j), masterIO = io.inActToArrayData.inActIO(iPlusJMod))
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
  for (k <- 0 until inActRouterNum) {
    var formerAdrWire: Seq[Bool] = Nil
    var laterAdrWire: Seq[Bool] = Nil
    var formerDataWire: Seq[Bool] = Nil
    var laterDataWire: Seq[Bool] = Nil
    for (i <- 0 until peRowNum) {
      for (j <- 0 until peColNum) {
        if (i + j == k) {
          formerAdrWire = formerAdrWire.:+(muxInActDataWire(i)(j).adrIOs.data.ready)
          formerDataWire = formerDataWire.:+(muxInActDataWire(i)(j).dataIOs.data.ready)
        }
        if (i + j == k + inActRouterNum) {
          laterAdrWire = laterAdrWire.:+(muxInActDataWire(i)(j).adrIOs.data.ready)
          laterDataWire = laterDataWire.:+(muxInActDataWire(i)(j).dataIOs.data.ready)
        }
      }
    }
    oneInActIOReadyWires.head(k) := formerAdrWire.reduce(_ && _) || laterAdrWire.reduce(_ && _)
    oneInActIOReadyWires.last(k) := formerDataWire.reduce(_ && _) || laterDataWire.reduce(_ && _)
  }
  io.inActToArrayData.muxInActData.zip(muxInActDataWire).foreach({ case (os, os1) => os.zip(os1).foreach({ case (o, o1) =>
    connectAllExceptReady(o, o1)
    o1.adrIOs.data.ready := o.adrIOs.data.ready
    o1.dataIOs.data.ready := o.dataIOs.data.ready
  })})
}

class PEClusterInActController(debug: Boolean) extends Module with ClusterConfig {
  val io: PEClusterInActCtrlIO = IO(new PEClusterInActCtrlIO)// state machine of inAct in the PE Cluster
  // inActWriteEnWires: inAct address and data writeEn wires, used to `and` with valid data
  private val inActWriteEnWires = Seq.fill(peRowNum, peColNum){Wire(Bool())}
  inActWriteEnWires.zipWithIndex.foreach({ case (bools, i) =>
    bools.zipWithIndex.foreach({ case (bool, j) => bool.suggestName(s"inActWriteEnWires($i)($j)")})
  })
  private val inActLoadFormer :: inActLoadLater :: Nil = Enum(2)
  // inActLoadFormer: inAct load data for former peArray
  // inActLoadLater: inAct load data for later peArray
  // TODO: use counter to get configurable load state
  private val inActDataIOStateRegs = Seq.fill(inActRouterNum){RegInit(inActLoadFormer)} // three inIOs
  inActDataIOStateRegs.zipWithIndex.foreach({ case (int, i) => int.suggestName(s"inActDataIOStateRegs[$i]")})
  private val inActDataIOZeroWires = Seq.fill(inActRouterNum){Wire(Bool())}
  inActDataIOZeroWires.zipWithIndex.foreach({ case (bool, i) => bool.suggestName(s"inActDataIOZeroWires[$i]")})
  private val inActDataIOOneWires = Seq.fill(inActRouterNum){Wire(Bool())}
  inActDataIOOneWires.zipWithIndex.foreach({ case (bool, i) => bool.suggestName(s"inActDataIOOneWires[$i]")})
  inActDataIOZeroWires.zip(inActDataIOStateRegs).foreach({ case (bool, int) => bool := int === inActLoadFormer})
  inActDataIOOneWires.zip(inActDataIOStateRegs).foreach({ case (bool, int) => bool := int === inActLoadLater})
  require(peColNum == 4 && peRowNum == 3, "you need to change the following dataPath connections for non default value")
  private val inActWriteDoneRegVec = Seq.fill(2, peRowNum, peColNum){RegInit(false.B)}
  // .head for address, .last for data
  inActWriteDoneRegVec.zipWithIndex.foreach({ case (seq, i) => seq.zipWithIndex.foreach({ case (bools, j) =>
    bools.zipWithIndex.foreach({ case (bool, k) => bool.suggestName(s"inActWriteDoneRegVec($i)($j)($k)")})
  })})
  private val inActWriteDoneWireVec = Seq.fill(peRowNum, peColNum){Wire(Bool())}
  // true then both inActAddress and inActData have finished write
  // used for jump inActDataIOStateRegs
  inActWriteDoneWireVec.zipWithIndex.foreach({ case (bools, i) =>
    bools.zipWithIndex.foreach({ case (bool, j) => bool.suggestName(s"inActWriteDoneWireVec($i)($j)")
    })})
  // connections of enWires and write finish wire
  for (i <- 0 until peRowNum) {
    for (j <- 0 until peColNum) {
      io.writeEn(i)(j) := inActWriteEnWires(i)(j)
      inActWriteDoneRegVec.head(i)(j) := Mux(io.inActWriteFinVec(i)(j).adrWriteFin,
        true.B, inActWriteDoneRegVec.head(i)(j))
      inActWriteDoneRegVec(1)(i)(j) := Mux(io.inActWriteFinVec(i)(j).dataWriteFin,
        true.B, inActWriteDoneRegVec(1)(i)(j))
      inActWriteDoneWireVec(i)(j) := inActWriteDoneRegVec.head(i)(j) && inActWriteDoneRegVec(1)(i)(j)
      val iPlusJMod = (i + j) % inActRouterNum
      if (i + j < inActRouterNum) {
        inActWriteEnWires(i)(j) := inActDataIOZeroWires(iPlusJMod)
      } else {
        inActWriteEnWires(i)(j) := inActDataIOOneWires(iPlusJMod)
      }
    }
  }
  private val inActDataStateJumpWires = Seq.fill(2, inActRouterNum){Wire(Bool())}
  // each inActIO has two Jump wires
  inActDataStateJumpWires.zipWithIndex.foreach({ case (bools, i) =>
    bools.zipWithIndex.foreach({ case (bool, j) => bool.suggestName(s"inActDataStateJumpWires($i)($j)")})
  })
  // connections of State JumpWire
  for (k <- 0 until inActRouterNum) {
    var wDoneJPZeroWires: Seq[Bool] = Nil
    var wDoneJPOneWires: Seq[Bool] = Nil
    for (i <- 0 until peRowNum) {
      for (j <- 0 until peColNum) {
        if (i + j == k) {
          wDoneJPZeroWires = wDoneJPZeroWires.+:(inActWriteDoneWireVec(i)(j)) // do now
          when (inActDataStateJumpWires.head(k)) {
            // only need one cycle for inActStateReg to jump, then can assign them to false
            inActWriteDoneRegVec.foreach(x => x(i)(j) := false.B)
          }
        }
        if (i + j == k + inActRouterNum) {
          wDoneJPOneWires = wDoneJPOneWires.+:(inActWriteDoneWireVec(i)(j)) // do later
          when (inActDataStateJumpWires.last(k)) {
            // only need one cycle for inActStateReg to jump, then can assign them to false
            inActWriteDoneRegVec.foreach(x => x(i)(j) := false.B)
          }
        }
      }
    }
    inActDataStateJumpWires.head(k) := wDoneJPZeroWires.reduce(_ && _)
    inActDataStateJumpWires.last(k) := wDoneJPOneWires.reduce(_ && _)
  }
  // state machine of three inActDataIOs
  for (i <- inActDataIOStateRegs.indices) {
    switch(inActDataIOStateRegs(i)) {
      is (inActLoadFormer) {
        when (inActDataStateJumpWires.head(i)) {
          inActDataIOStateRegs(i) := inActLoadLater
        }
      }
      is (inActLoadLater) {
        when (inActDataStateJumpWires.last(i)) {
          inActDataIOStateRegs(i) := inActLoadFormer
        }
      }
    }
  }
  if (debug) {
    io.debugIO.inActDataIOState <> inActDataIOStateRegs
  } else {
    io.debugIO <> DontCare
  }
}

class PEClusterInActToArrayDataIO extends Bundle with ClusterConfig {
  // output bits and valid, from inActCtrl to PEArray
  val muxInActData: Vec[Vec[CSCStreamIO]] = Vec(peRowNum, Vec(peColNum, new CSCStreamIO(inActAdrWidth, inActDataWidth)))
  // from top to inActCtrl
  val inActIO: Vec[CSCStreamIO] = Vec(inActRouterNum, Flipped(new CSCStreamIO(inActAdrWidth, inActDataWidth))) // input only
}

class PEClusterInActIO extends Bundle with HasInActWriteFinVecIO {
  val inActCtrlSel: CommonClusterCtrlBoolUIntIO = Flipped(new CommonClusterCtrlBoolUIntIO)
  val inActToArrayData = new PEClusterInActToArrayDataIO
  val debugIO = new Bundle with HasPEClusterInActControllerDebugIO
}

class PEClusterInActDataIO extends Bundle with ClusterConfig {
  val inActToArrayData = new PEClusterInActToArrayDataIO
}

class PEClusterInActCtrlIO extends Bundle with HasInActWriteFinVecIO {
  val writeEn: Vec[Vec[Bool]] = Output(Vec(peRowNum, Vec(peColNum, Bool())))
  val debugIO = new Bundle with HasPEClusterInActControllerDebugIO
}

trait HasInActWriteFinVecIO extends Bundle with ClusterConfig {
  val inActWriteFinVec: Vec[Vec[CSCWriteFinIO]] = Vec(peRowNum, Vec(peColNum, Flipped(new CSCWriteFinIO))) // input
}

trait HasPEClusterInActControllerDebugIO extends Bundle with ClusterConfig {
  val inActDataIOState: Vec[UInt] = Output(Vec(inActRouterNum, UInt(1.W)))
  //val inActDoneReg
}

class PEClusterDebugIO extends Bundle with ClusterConfig with HasPEClusterInActControllerDebugIO {
  val inActWriteFinVec: Vec[Vec[CSCWriteFinIO]] = Vec(peRowNum, Vec(peColNum, new CSCWriteFinIO)) // Output
  val eachPETopDebug: Vec[Vec[PETopDebugIO]] = Vec(peRowNum, Vec(peColNum, new PETopDebugIO))
  val eachPEInActValid: Vec[Vec[Vec[Bool]]] = Output(Vec(2, Vec(peRowNum, Vec(peColNum, Bool()))))
}

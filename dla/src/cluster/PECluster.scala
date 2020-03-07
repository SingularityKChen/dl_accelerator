package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe._

class PECluster(debug: Boolean) extends HasConnectAllExpRdModule with ClusterConfig {
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
      connectAllExceptReady(peArray(i)(j).dataStream.weightIOs, io.dataPath.weightIO(i))
      io.dataPath.weightIO(i).adrIOs.data.ready := peArray(i).map(x => x.dataStream.weightIOs.adrIOs.data.ready).reduce(_ && _)
      io.dataPath.weightIO(i).dataIOs.data.ready := peArray(i).map(x => x.dataStream.weightIOs.dataIOs.data.ready).reduce(_ && _)
      peArray(i)(j).dataStream.inActIOs <> peClusterInAct.inActToArrayData.muxInActData(i)(j)
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
          formerAdrWire = formerAdrWire.+:(muxInActDataWire(i)(j).adrIOs.data.ready)
          formerDataWire = formerDataWire.+:(muxInActDataWire(i)(j).dataIOs.data.ready)
        }
        if (i + j == k + inActRouterNum) {
          laterAdrWire = laterAdrWire.+:(muxInActDataWire(i)(j).adrIOs.data.ready)
          laterDataWire = laterDataWire.+:(muxInActDataWire(i)(j).dataIOs.data.ready)
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

class PEClusterInActController extends Module with ClusterConfig {
  val io: PEClusterInActCtrlIO = IO(new PEClusterInActCtrlIO)// state machine of inAct in the PE Cluster
  // inActRoutingMode: whether the input activations should be broad-cast;
  // true then all PEs' data receive from the same router whose index equals to outDataSel's value;
  private val inActRoutingMode = Wire(Bool()) // FIXME: unused
  inActRoutingMode.suggestName("inActRoutingMode")
  private val inActBroadCastIdxWire = Wire(UInt(2.W))  // FIXME: unused
  inActBroadCastIdxWire.suggestName("inActBroadCastIdxWire")
  inActRoutingMode := io.topToInAct.inActCtrlSel.inDataSel // true for broad-cast
  inActBroadCastIdxWire := io.topToInAct.inActCtrlSel.outDataSel
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
  // connections of enWires
  for (i <- 0 until peRowNum) {
    for (j <- 0 until peColNum) {
      io.writeEn(i)(j) := inActWriteEnWires(i)(j)
      inActWriteDoneRegVec.head(i)(j) := Mux(io.inActWriteFinVec(i)(j).adrWriteFin,
        !inActWriteDoneRegVec.head(i)(j), inActWriteDoneRegVec.head(i)(j))
      inActWriteDoneRegVec(1)(i)(j) := Mux(io.inActWriteFinVec(i)(j).dataWriteFin,
        !inActWriteDoneRegVec(1)(i)(j), inActWriteDoneRegVec(1)(i)(j))
      inActWriteDoneWireVec(i)(j) := inActWriteDoneRegVec.head(i)(j) && inActWriteDoneRegVec(1)(i)(j)
      when (inActWriteDoneWireVec(i)(j)) {
        // only need one cycle for inActStateReg to jump, then can assign them to false
        inActWriteDoneRegVec.foreach(x => x(i)(j) := false.B)
      }
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
  // connections of EnWire
  for (k <- 0 until inActRouterNum) {
    var wDoneJPZeroWires: Seq[Bool] = Nil
    var wDoneJPOneWires: Seq[Bool] = Nil
    for (i <- 0 until peRowNum) {
      for (j <- 0 until peColNum) {
        if (i + j == k) wDoneJPZeroWires = wDoneJPZeroWires.+:(inActWriteDoneWireVec(i)(j))
        if (i + j == k + inActRouterNum) wDoneJPOneWires = wDoneJPOneWires.+:(inActWriteDoneWireVec(i)(j))
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
}

class PEClusterTopToInActCtrlIO extends Bundle {
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
}

class PEClusterInActCtrlIO extends Bundle with ClusterConfig {
  val topToInAct = new PEClusterTopToInActCtrlIO
  val writeEn: Vec[Vec[Bool]] = Vec(peRowNum, Vec(peColNum, Output(Bool())))
  val inActWriteFinVec: Vec[Vec[CSCWriteFinIO]] = Vec(peRowNum, Vec(peColNum, Flipped(new CSCWriteFinIO))) // input
}

class PEClusterPSumCtrlIO extends Bundle with ClusterConfig {
  val pSumEnqEn: Vec[Vec[Bool]] = Vec(peRowNum, Vec(peColNum, Output(Bool())))
  val doEn: Bool = Input(Bool()) // doEn signal from top
  // pSumWF: receive write finish signals from each PE
  val pSumWF: Vec[Vec[Bool]] = Vec(peRowNum, Vec(peColNum, Input(Bool())))
}
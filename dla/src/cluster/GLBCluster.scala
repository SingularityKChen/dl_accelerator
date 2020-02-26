package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe.{CSCStreamIO, MCRENFConfig, StreamBitsIO}

class GLBCluster(debug: Boolean) extends Module with ClusterSRAMConfig {
  val io: GLBClusterIO = IO(new GLBClusterIO)
  private val iSRAMs = VecInit(Seq.fill(inActSRAMNum){Module(new InActSRAMBank(debug)).io})
  iSRAMs.suggestName("inActSRAMs")
  private val pSRAMs = VecInit(Seq.fill(pSumSRAMNum){Module(new PSumSRAMBank(pSumSRAMSize, psDataWidth, debug)).io})
  pSRAMs.suggestName("pSumSRAMs")
  private val oneSRAMIdle :: oneSRAMDoing :: oneSRAMWaiting :: Nil = Enum(3)
  private val theTopCtrls = Seq(io.ctrlPath.inActIO, io.ctrlPath.pSumIO)
  private val theSRAMsCtrl = Seq(iSRAMs.map(x => x.ctrlPath), pSRAMs.map(x => x.ctrlPath))
  private val theSRAMsNum = Seq(inActSRAMNum, pSumSRAMNum)
  private val theSRAMsEnWire = Wire(Vec(2, Bool()))
  theSRAMsEnWire.suggestName("theSRAMsEnWire")
  private val theSRAMsState = RegInit(VecInit(Seq.fill(2){oneSRAMIdle}))
  theSRAMsState.suggestName("theSRAMsState")
  private val theSRAMsDoneRegVec = Seq(RegInit(VecInit(Seq.fill(inActSRAMNum){false.B})), RegInit(VecInit(Seq.fill(pSumSRAMNum){false.B})))
  theSRAMsDoneRegVec.head.suggestName("inActSRAMDoneRegVec")
  theSRAMsDoneRegVec.last.suggestName("pSumSRAMDoneRegVec")
  private val theSRAMsAllDoneWire = Wire(Vec(2, Bool()))
  theSRAMsAllDoneWire.suggestName("theSRAMsAllDoneWire")
  private val theSRAMsDoingWire = Wire(Vec(2, Bool()))
  theSRAMsDoingWire.suggestName("theSRAMsDoingWire")
  private val pSumSRAMStrIdx = VecInit(Seq.fill(pSumSRAMNum){RegInit(0.U)})
  private def OneSRAMState(stateReg: UInt, enable: Bool, done: Bool): Unit = {
    switch (stateReg) {
      is (oneSRAMIdle) {
        when (enable) {
          stateReg := oneSRAMDoing
        }
      }
      is (oneSRAMDoing) {
        when (done) {
          stateReg := oneSRAMIdle
        } .elsewhen (!enable) {
          stateReg := oneSRAMWaiting
        } .otherwise {
          stateReg := oneSRAMDoing
        }
      }
      is (oneSRAMWaiting) { // waiting for enable signal, but keep the done information
        when (enable) {
          stateReg := oneSRAMDoing
        }
      }
    }
  }
  // connections of inAct and PSum
  for (i <- 0 until 2) {
    // connections of control path
    theTopCtrls(i).done := theSRAMsAllDoneWire(i)
    theTopCtrls(i).busy := theSRAMsDoingWire(i)
    theSRAMsEnWire(i) := theTopCtrls(i).doEn
    // inner logic
    theSRAMsAllDoneWire(i) := theSRAMsDoneRegVec(i).reduce(_ && _) // when they all become true, then inActSRAMs are all done
    theSRAMsDoingWire(i) := theSRAMsState(i) === oneSRAMDoing
    // inner connections
    for (j <- 0 until theSRAMsNum(i)) {
      theSRAMsCtrl(i)(j).doEn := theSRAMsDoingWire(i) && !theSRAMsDoneRegVec(i)(j)
      // if one received done signal, then its signal flips, from false to true, then disable its enable signal
      // if inAct state is idle, then assign done reg to false.
      theSRAMsDoneRegVec(i)(j) := Mux(!(theSRAMsState(i) === oneSRAMIdle), Mux(theSRAMsCtrl(i)(j).done, !theSRAMsDoneRegVec(i)(j), theSRAMsDoneRegVec(i)(j)), false.B)
      theSRAMsCtrl(i)(j).writeOrRead := theTopCtrls(i).writeOrRead
    }
    OneSRAMState(theSRAMsState(i), theSRAMsEnWire(i), theSRAMsAllDoneWire(i))
  }
  // connections of data path
  io.dataPath.weightIO.foreach(x => x.inIOs <> x.outIOs)
  io.dataPath.inActIO.zip(iSRAMs).foreach({ case (dataIO, sramIO) => dataIO <> sramIO.dataPath})
  io.dataPath.pSumIO.zip(pSRAMs).foreach({ case (dataIO, sramIO) => dataIO <> sramIO.dataPath})
  pSRAMs.zipWithIndex.foreach({ case (pSumIO, idx) =>
    pSumIO.dataPath <> io.dataPath.pSumIO(idx)
    pSumIO.ctrlPath.startIdx := pSumSRAMStrIdx(idx) // start index
  })
  // connections of debugIO
  if (debug) {
    io.debugIO.inActDebugIO.zip(iSRAMs).foreach({ case (topDebug, sram) => topDebug <> sram.debugIO})
    io.debugIO.pSumDebugIO.zip(pSRAMs).foreach({ case (topDebug, sram) => topDebug <> sram.debugIO})
    io.debugIO.theState <> theSRAMsState
    io.debugIO.allDone <> theSRAMsAllDoneWire
    io.debugIO.oneInActSRAMDone <> theSRAMsDoneRegVec.head
    io.debugIO.onePSumSRAMDone <> theSRAMsDoneRegVec(1)
  } else {
    io.debugIO <> DontCare
  }
  // TODO: pSum's start index
}

class PSumSRAMBank(private val theSRAMSize: Int, private val theDataWidth: Int, debug: Boolean) extends SRAMCommon(theSRAMSize, theDataWidth) with ClusterSRAMConfig with MCRENFConfig with GNMFCS2Config {
  private val oneSPadPSum: Int = M0*E*N0*F0 // when read counts this, then stop
  private val oneSRAMPSum: Int = oneSPadPSum * N2*M2*F2 // when write counts this, then finished
  require(oneSRAMPSum <= pSumSRAMSize, s"the size of PSum SRAM is $pSumSRAMSize, " +
    s"which should be larger than the config's requirement, which is $oneSRAMPSum")
  theSRAM.suggestName("onePSumSRAMBank")
  val io: PSumSRAMBankIO = IO(new PSumSRAMBankIO)
  // SRAM read write logic
  private val startIdx = Wire(UInt(log2Ceil(theSRAMSize).W)) // the first do address, from io
  private val doIdxWire: UInt = Wire(UInt(log2Ceil(theSRAMSize).W))
  // read or write one SPad size partial sums
  // doIdxCount means current number of PSums that has been done
  // when finish, doDone = true.B
  private val (doIdxCount, doDoneWire) = Counter(doIdxIncWire, oneSPadPSum)
  startIdx := io.ctrlPath.startIdx
  writeOrRead := io.ctrlPath.writeOrRead
  io.ctrlPath.done := doDoneReg
  doDoneReg := doDoneWire
  //doEnWire := !doDoneReg && io.ctrlPath.doEn
  doEnWire := io.ctrlPath.doEn // FIXME
  doIdxWire := startIdx + doIdxCount
  // write logic
  writeLogic(io.dataPath.inIOs, doIdxWire)
  // read logic
  readLogic(io.dataPath.outIOs, doIdxWire, doDoneWire)
  // debug io
  if (debug) {
    debugLogic(io.debugIO, doIdxWire)
    io.debugIO.idxCount := doIdxCount
    io.debugIO.currentData := DontCare
    io.debugIO.meetOneZero := DontCare
    io.debugIO.theState := DontCare
  } else {
    io.debugIO <> DontCare
  }
}

abstract class SRAMCommon(private val theSRAMSize: Int, private val theDataWidth: Int) extends Module {
  protected val theSRAM: SyncReadMem[UInt] = SyncReadMem(theSRAMSize, UInt(theDataWidth.W))
  // SRAM read write logic
  protected val writeOrRead: Bool = Wire(Bool()) // true then "do" means write, false then "do" means read
  // because it will not read and write at the same time
  // so we just need one pair of regs to record process
  protected val doEnWire: Bool = Wire(Bool())
  protected val doReadWire: Bool = Wire(Bool())
  protected val doWriteWire: Bool = Wire(Bool())
  protected val writeInData: UInt = Wire(UInt(theDataWidth.W))
  protected val readOutData: UInt = Wire(UInt(theDataWidth.W))
  // waitForRead: false, then ask for data;
  //              true, then data comes.
  protected val waitForRead: Bool = RegInit(false.B) // use for letting increase slower when read
  protected val doIdxIncWire: Bool = Wire(Bool()) // true, then the index increase
  protected val nextValid: Bool = Wire(Bool())
  protected val nextValidReg: Bool = RegInit(false.B) // one cycle after nextValid
  protected val doDoneReg: Bool = RegInit(false.B) // or combination loop detected
  doIdxIncWire := Mux(writeOrRead, doWriteWire, doReadWire && waitForRead)

  def readLogic(readOutIO: DecoupledIO[UInt], doIdx: UInt, doDoneWire: Bool): Any = {
    nextValid := Mux(writeOrRead, false.B, doEnWire && !waitForRead)
    nextValidReg := nextValid && readOutIO.ready
    // when write, waitForRead keeps be false;
    // when read, only enable signal becomes true, then waitForRead signal began to flip
    waitForRead := Mux(!doDoneWire, Mux(writeOrRead || !doEnWire, waitForRead, !waitForRead), false.B)
    readOutIO.valid := nextValidReg // reg next, so one cycle later, data will be read out with valid signal
    doReadWire := readOutIO.ready && nextValidReg
    readOutData := theSRAM.read(doIdx, doReadWire && !waitForRead)
    readOutIO.bits := Mux(waitForRead, readOutData, 0.U)
  }

  def writeLogic(writeInIO: DecoupledIO[UInt], doIdx: UInt): Any = {
    writeInData := writeInIO.bits
    doWriteWire := doEnWire && writeInIO.valid
    writeInIO.ready := doWriteWire
    when(doIdxIncWire) {
      theSRAM.write(doIdx, writeInData)
    }
  }

  def debugLogic(debugIO: SRAMCommonDebugIO, doIdx: UInt): Any = {
    debugIO.idx := doIdx
    debugIO.idxInc := doIdxIncWire
    debugIO.waitForRead := waitForRead
    debugIO.doReadWire := doReadWire
  }
}

class InActSRAMCommon(private val theSRAMSize: Int, private val theDataWidth: Int, debug: Boolean) extends SRAMCommon(theSRAMSize, theDataWidth) {
  theSRAM.suggestName("oneInActSRAM")
  val io: InACTSRAMCommonIO = IO(new InACTSRAMCommonIO(theSRAMSize, theDataWidth))
  private val doMeetZeroWire = Wire(Bool()) // current data equals to zero
  private val doMightFinishedWire = Wire(Bool())// that's the end of one SPad's data
  private val writeDoneWire = Wire(Bool())
  private val readDoneWire = Wire(Bool())
  private val doIdxReg: UInt = RegInit(0.U(log2Ceil(theSRAMSize).W))
  // SRAM read write logic
  // we assume that one write cycle won't start until read finishes, i.e., the index can be hold when calculating,
  // then continue reading
  private val currentData = Wire(UInt(theDataWidth.W))
  private val doDoneWire = Wire(Bool())
  // if meet two continuous zeros, then the group of data finishes
  private val meetTwoZerosWire = Wire(Bool())
  private val idxWrap = Wire(Bool())
  // TODO: add some logic to check whether one SPad of data is a zero matrix, true then jump
  writeOrRead := io.ctrlPath.writeOrRead
  io.ctrlPath.done := doDoneWire
  doEnWire := (!writeOrRead || !doDoneReg) && io.ctrlPath.doEn // FIXME
  currentData := Mux(writeOrRead, writeInData, readOutData)
  // index increase and wrap
  when (doIdxIncWire) {
    doIdxReg := doIdxReg + 1.U
  }
  when (idxWrap) {
    doIdxReg := 0.U
  }
  // write logic
  writeLogic(io.dataPath.inIOs.data, doIdxReg)
  // read logic
  readLogic(io.dataPath.outIOs.data, doIdxReg, doDoneWire)
  // do finish?
  private val noZero :: oneZero :: twoZeros :: Nil = Enum(3)
  private val zeroState = RegInit(noZero)
  private val meetZeroWire = Wire(Bool())
  meetZeroWire := currentData === 0.U
  doMeetZeroWire := Mux(writeOrRead, meetZeroWire, meetZeroWire && waitForRead)
  doMightFinishedWire := doMeetZeroWire && doEnWire // meets one zero, that's the end of one SPad
  switch(zeroState) {
    is (noZero) {
      doDoneReg := false.B
      when (doMightFinishedWire) {
        zeroState := oneZero
      }
    }
    is (oneZero) {
      when (doMightFinishedWire) {
        zeroState := twoZeros
      } .otherwise {
        zeroState := noZero
      }
    }
    is (twoZeros) {
      zeroState := noZero // we assume there will not be three continuous zeros
    }
  }
  meetTwoZerosWire := zeroState === twoZeros // that's two zeros
  readDoneWire := zeroState === oneZero // or meet one zero
  writeDoneWire := meetTwoZerosWire
  doDoneWire := Mux(writeOrRead, writeDoneWire, readDoneWire)
  doDoneReg := doDoneWire
  idxWrap := meetTwoZerosWire // wrap the index only after meeting two zeros
  // debug io
  if (debug) {
    debugLogic(io.debugIO, doIdxReg)
    io.debugIO.currentData := currentData
    io.debugIO.meetOneZero := readDoneWire
    io.debugIO.theState := zeroState
    io.debugIO.idxCount := DontCare
  } else {
    io.debugIO <> DontCare
  }
}

class InActSRAMBank(debug: Boolean) extends Module with ClusterSRAMConfig {
  val io: InActSRAMBankIO = IO(new InActSRAMBankIO)
  private val adrSRAM = Module(new InActSRAMCommon(inActAdrSRAMSize, inActAdrWidth, debug))
  adrSRAM.suggestName("adrSRAM")
  private val dataSRAM = Module(new InActSRAMCommon(inActDataSRAMSize, inActDataWidth, debug))
  dataSRAM.suggestName("dataSRAM")
  private val bothDoneWire = Wire(Bool())
  private val inActIdle :: inActDoing :: inActWaitAdr :: inActWaitData :: Nil = Enum(4)
  private val inActState = RegInit(inActIdle)
  private val currentDoingWire = Wire(Bool())
  private val adrDoneWire = Wire(Bool())
  private val dataDoneWire = Wire(Bool())
  currentDoingWire := inActState === inActDoing
  adrDoneWire := adrSRAM.io.ctrlPath.done
  dataDoneWire := dataSRAM.io.ctrlPath.done
  adrSRAM.io.ctrlPath.doEn := currentDoingWire || (inActState === inActWaitAdr)
  dataSRAM.io.ctrlPath.doEn := currentDoingWire || (inActState === inActWaitData)
  bothDoneWire := ((inActState === inActWaitData) && dataDoneWire) || ((inActState === inActWaitAdr) && adrDoneWire)
  switch(inActState) {
    is (inActIdle) {
      when (io.ctrlPath.doEn) {
        inActState := inActDoing
      }
    }
    is (inActDoing) {
      when (adrDoneWire) {
        inActState := inActWaitData
      } .elsewhen (dataDoneWire) {
        inActState := inActWaitAdr
      } .otherwise {
        inActState := inActDoing
      }
    }
    is (inActWaitData) {
      when (dataDoneWire) {
        inActState := inActIdle
      }
    }
    is (inActWaitAdr) {
      when (adrDoneWire) {
        inActState := inActIdle
      }
    }
  }
  // SRAM connections
  adrSRAM.io.dataPath.inIOs <> io.dataPath.inIOs.adrIOs
  adrSRAM.io.dataPath.outIOs <> io.dataPath.outIOs.adrIOs
  dataSRAM.io.dataPath.inIOs <> io.dataPath.inIOs.dataIOs
  dataSRAM.io.dataPath.outIOs <> io.dataPath.outIOs.dataIOs
  // control path
  io.ctrlPath.done := bothDoneWire
  Seq(adrSRAM.io.ctrlPath, dataSRAM.io.ctrlPath).foreach({ x =>
    x.writeOrRead := io.ctrlPath.writeOrRead
  })
  if (debug) {
    io.debugIO.theState := inActState
    Seq(io.debugIO.adrDebug, io.debugIO.dataDebug).zip(Seq(adrSRAM.io, dataSRAM.io)).foreach({case (topIO, sram) =>
      topIO.subDone := sram.ctrlPath.done
      topIO.commonDebug <> sram.debugIO
    })
  } else {
    io.debugIO <> DontCare
  }
}

class SRAMCommonCtrlIO extends Bundle {
  val writeOrRead: Bool = Input(Bool())
  val doEn: Bool = Input(Bool())
  val done: Bool = Output(Bool())
}

class SRAMCommonDebugIO(private val theSRAMSize: Int, private val theDataWidth: Int) extends Bundle {
  val idx: UInt = Output(UInt(log2Ceil(theSRAMSize).W))
  val idxInc: Bool = Output(Bool())
  val idxCount: UInt = Output(UInt(log2Ceil(theSRAMSize).W))
  val currentData: UInt = Output(UInt(theDataWidth.W))
  val meetOneZero: Bool = Output(Bool())
  val theState: UInt = Output(UInt(2.W))
  val waitForRead: Bool = Output(Bool())
  val doReadWire: Bool = Output(Bool())
}

class InActSRAMBankDebugSubIO(private val theSRAMSize: Int, private val theDataWidth: Int) extends Bundle {
  val commonDebug = new SRAMCommonDebugIO(theSRAMSize, theDataWidth)
  val subDone: Bool = Output(Bool())
}

trait WithStartIdxIO extends Bundle with ClusterSRAMConfig {
  val startIdx: UInt = Input(UInt(log2Ceil(pSumSRAMSize).W))
}

class InACTSRAMCommonIO(private val theSRAMSize: Int, private val theDataWidth: Int) extends Bundle {
  val dataPath = new StreamBitsInOutIO(theDataWidth)
  val ctrlPath = new SRAMCommonCtrlIO
  val debugIO = new SRAMCommonDebugIO(theSRAMSize, theDataWidth)
}

class InActSRAMBankDebugIO extends Bundle with ClusterSRAMConfig {
  val theState: UInt = Output(UInt(2.W))
  val adrDebug =  new InActSRAMBankDebugSubIO(inActAdrSRAMSize, inActAdrWidth)
  val dataDebug = new InActSRAMBankDebugSubIO(inActDataSRAMSize, inActDataWidth)
}

class InActSRAMBankIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new CSCStreamInOutIO(inActAdrWidth, inActDataWidth)
  val ctrlPath = new SRAMCommonCtrlIO
  val debugIO = new InActSRAMBankDebugIO
}

class StreamBitsInOutIO(private val theDataWidth: Int) extends Bundle {
  val inIOs: StreamBitsIO = Flipped(new StreamBitsIO(theDataWidth))
  val outIOs = new StreamBitsIO(theDataWidth)
}

class CSCStreamInOutIO(private val adrWidth: Int, private val dataWidth: Int) extends Bundle {
  val inIOs: CSCStreamIO = Flipped(new CSCStreamIO(adrWidth = adrWidth, dataWidth = dataWidth))
  val outIOs = new CSCStreamIO(adrWidth = adrWidth, dataWidth = dataWidth)
}

class PSumSRAMBankIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new PSumSRAMDataIO(psDataWidth)
  val ctrlPath = new SRAMCommonCtrlIO with WithStartIdxIO
  val debugIO = new SRAMCommonDebugIO(pSumSRAMSize, psDataWidth)
}

class PSumSRAMDataIO(private val dataWidth: Int) extends Bundle {
  val inIOs: DecoupledIO[UInt] = Flipped(Decoupled(UInt(dataWidth.W)))
  val outIOs: DecoupledIO[UInt] = Decoupled(UInt(dataWidth.W))
}

class GLBClusterIO extends Bundle {
  val dataPath = new GLBClusterDataIO
  val ctrlPath = new GLBClusterCtrlIO
  val debugIO = new GLBClusterDebugIO
}

class WeightGLBIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new CSCStreamInOutIO(weightAdrWidth, weightDataWidth)
}

class GLBClusterCtrlIO extends Bundle {
  val inActIO = new SRAMCommonCtrlIO with BusySignal
  val pSumIO =  new SRAMCommonCtrlIO with BusySignal
}

class GLBClusterDataIO extends Bundle with ClusterSRAMConfig {
  val inActIO: Vec[CSCStreamInOutIO] = Vec(inActSRAMNum, new CSCStreamInOutIO(inActAdrWidth, inActDataWidth))
  val weightIO: Vec[CSCStreamInOutIO] = Vec(weightRouterNum, new CSCStreamInOutIO(weightAdrWidth, weightDataWidth))
  val pSumIO: Vec[PSumSRAMDataIO] = Vec(pSumSRAMNum, new PSumSRAMDataIO(psDataWidth))
}

class GLBClusterDebugIO extends Bundle with ClusterSRAMConfig {
  val oneInActSRAMDone: Vec[Bool] = Output(Vec(inActSRAMNum, Bool()))
  val onePSumSRAMDone: Vec[Bool] = Output(Vec(pSumSRAMNum, Bool()))
  val allDone: Vec[Bool] = Output(Vec(2, Bool()))
  val theState: Vec[UInt] = Output(Vec(2, UInt(2.W)))
  val inActDebugIO: Vec[InActSRAMBankDebugIO] = Vec(inActSRAMNum, new InActSRAMBankDebugIO)
  val pSumDebugIO: Vec[SRAMCommonDebugIO] = Vec(pSumSRAMNum, new SRAMCommonDebugIO(pSumSRAMSize, psDataWidth))
}
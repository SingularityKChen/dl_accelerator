package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe.{CSCStreamIO, MCRENFConfig, StreamBitsIO}

class GLBCluster(debug: Boolean) extends Module with ClusterSRAMConfig {
  val io: GLBClusterIO = IO(new GLBClusterIO)
  private val iSRAMs = VecInit(Seq.fill(inActSRAMNum){Module(new InActSRAMBank(debug)).io})
  private val pSRAMs = VecInit(Seq.fill(pSumSRAMNum){Module(new PSumSRAMBank(pSumSRAMSize, psDataWidth, debug)).io})
  // connections of data path
  io.dataPath.weightIO.foreach(x => x.inIOs <> x.outIOs)
  io.dataPath.inActIO.zip(iSRAMs).foreach({ case (topIO, sramIO) => topIO <> sramIO.dataPath})
  io.dataPath.pSumIO.zip(pSRAMs).foreach({ case (topIO, sramIO) => topIO <> sramIO.dataPath})
  // connections of control path
}

class PSumSRAMBank(private val theSRAMSize: Int, private val theDataWidth: Int, debug: Boolean) extends SRAMCommon(theSRAMSize, theDataWidth) with ClusterSRAMConfig with MCRENFConfig with GNMFCS2Config {
  private val oneSPadPSum: Int = M0*E*N0*F0 // when read counts this, then stop
  private val oneSRAMPSum: Int = oneSPadPSum * N2*M2*F2 // when write counts this, then finished
  require(oneSRAMPSum <= pSumSRAMSize, s"the size of PSum SRAM is $pSumSRAMSize, " +
    s"which should be larger than the config's requirement, which is $oneSRAMPSum")
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
  readLogic(io.dataPath.outIOs, doIdxWire)
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
  protected val nextValidReg: Bool = RegNext(nextValid) // one cycle after nextValid
  protected val doDoneReg: Bool = RegInit(false.B) // or combinational loop detected
  doIdxIncWire := Mux(writeOrRead, doWriteWire, doReadWire && waitForRead)
  def readLogic(readOutIO: DecoupledIO[UInt], doIdx: UInt): Any = {
    nextValid := Mux(writeOrRead, false.B, doEnWire && !waitForRead)
    waitForRead := Mux(writeOrRead, waitForRead, !waitForRead)
    readOutIO.valid := nextValidReg // reg next, so one cycle later, data will be read out with valid signal
    doReadWire := readOutIO.ready && nextValidReg
    readOutData := theSRAM.read(doIdx, doReadWire && !waitForRead)
    readOutIO.bits := Mux(waitForRead, readOutData, 0.U)
  }

  def writeLogic(writeInIO: DecoupledIO[UInt], doIdx: UInt): Any = {
    writeInData := writeInIO.bits
    doWriteWire := doEnWire && writeInIO.valid
    writeInIO.ready := doWriteWire
    when (doIdxIncWire) {
      theSRAM.write(doIdx, writeInData)
    }
  }

  def debugLogic(debugIO: SRAMCommonDebugIO, doIdx: UInt): Any = {
    debugIO.idx := doIdx
    debugIO.idxInc := doIdxIncWire
  }
}

class InActSRAMCommon(private val theSRAMSize: Int, private val theDataWidth: Int, debug: Boolean) extends SRAMCommon(theSRAMSize, theDataWidth) {
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
  doDoneReg := doDoneWire
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
  readLogic(io.dataPath.outIOs.data, doIdxReg)
  // do finish?
  private val noZero :: oneZero :: twoZeros :: Nil = Enum(3)
  private val zeroState = RegInit(noZero)
  private val meetZeroWire = Wire(Bool())
  meetZeroWire := currentData === 0.U
  doMeetZeroWire := Mux(writeOrRead, meetZeroWire, meetZeroWire && waitForRead)
  doMightFinishedWire := doMeetZeroWire && doEnWire // meets one zero, that's the end of one SPad
  switch(zeroState) {
    is (noZero) {
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
      }
      when (dataDoneWire) {
        inActState := inActWaitAdr
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
      topIO.idx := sram.debugIO.idx
      topIO.idxInc := sram.debugIO.idxInc
      topIO.meetOneZero := sram.debugIO.meetOneZero
      topIO.currentData := sram.debugIO.currentData
      topIO.theState := sram.debugIO.theState
      topIO.idxCount := DontCare
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
}

trait SubModuleDebugDone extends Bundle {
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
  val adrDebug = new SRAMCommonDebugIO(inActAdrSRAMSize, inActAdrWidth) with SubModuleDebugDone
  val dataDebug = new SRAMCommonDebugIO(inActDataSRAMSize, inActDataWidth) with SubModuleDebugDone
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

class PSumSRAMDataIO(dataWidth: Int) extends Bundle {
  val inIOs: DecoupledIO[UInt] = Flipped(Decoupled(UInt(dataWidth.W)))
  val outIOs: DecoupledIO[UInt] = Decoupled(UInt(dataWidth.W))
}

class GLBClusterIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new GLBClusterDataIO
  val ctrlPath = new GLBClusterCtrlIO
}

class WeightGLBIO extends Bundle with ClusterSRAMConfig {
  val dataPath = new CSCStreamInOutIO(weightAdrWidth, weightDataWidth)
}

class GLBClusterCtrlIO extends Bundle {

}

class GLBClusterDataIO extends Bundle with ClusterSRAMConfig {
  val inActIO: Vec[CSCStreamInOutIO] = Vec(inActSRAMNum, new CSCStreamInOutIO(inActAdrWidth, inActDataWidth))
  val weightIO: Vec[CSCStreamInOutIO] = Vec(weightRouterNum, new CSCStreamInOutIO(weightAdrWidth, weightDataWidth))
  val pSumIO: Vec[PSumSRAMDataIO] = Vec(pSumSRAMNum, new PSumSRAMDataIO(psDataWidth))
}
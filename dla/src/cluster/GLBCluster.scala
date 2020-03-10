package dla.cluster

import chisel3._
import chisel3.util._
import dla.pe.{CSCStreamIO, MCRENFConfig, StreamBitsIO}

class GLBCluster(debug: Boolean) extends Module with ClusterSRAMConfig with GNMFCS2Config {
  val io: GLBClusterIO = IO(new GLBClusterIO)
  private val iSRAMs = Seq.fill(inActSRAMNum){Module(new InActSRAMBank(debug)).io}
  iSRAMs.zipWithIndex.foreach({ case (o, i) => o.suggestName(s"inActSRAM$i")})
  private val pSRAMs = Seq.fill(pSumSRAMNum){Module(new PSumSRAMBank(pSumSRAMSize, psDataWidth, debug)).io}
  pSRAMs.zipWithIndex.foreach({ case (o, i) => o.suggestName(s"pSumSRAM$i")})
  private val theTopCtrls = Seq(io.ctrlPath.inActIO, io.ctrlPath.pSumIO)
  private val theSRAMsCtrl = Seq(iSRAMs.map(x => x.ctrlPath), pSRAMs.map(x => x.ctrlPath))
  private val theSRAMsNum = Seq(inActSRAMNum, pSumSRAMNum)
  private val theSRAMsEnWire = Wire(Vec(2, Vec(2, Bool())))
  theSRAMsEnWire.suggestName("theSRAMsEnWire")
  // connections of inAct and PSum
  for (i <- 0 until 2) { // inAct and PSum
    // connections of control path
    for (k <- 0 until 2) { // read and write
      // inner connections
      for (j <- 0 until theSRAMsNum(i)) {
        val topCtrlSeq = Seq(theTopCtrls(i)(j).readIO, theTopCtrls(i)(j).writeIO)
        theSRAMsEnWire(i)(k) := topCtrlSeq(k).enable
        val sramCtrlSeq = Seq(theSRAMsCtrl(i)(j).readIO, theSRAMsCtrl(i)(j).writeIO)
        sramCtrlSeq(k).enable := topCtrlSeq(k).enable
        sramCtrlSeq(k).adr := topCtrlSeq(k).adr
        if (i == 1) {
          topCtrlSeq(k).done := DontCare
        } else {
          topCtrlSeq(k).done := sramCtrlSeq(k).done
        }
      }
    }
  }
  // connections of data path
  io.dataPath.weightIO.foreach(x => x.inIOs <> x.outIOs)
  io.dataPath.inActIO.zip(iSRAMs).foreach({ case (dataIO, sramIO) => dataIO <> sramIO.dataPath})
  io.dataPath.pSumIO.zip(pSRAMs).foreach({ case (dataIO, sramIO) =>
    dataIO <> sramIO.dataPath
  })
  pSRAMs.zipWithIndex.foreach({ case (pSumIO, idx) =>
    pSumIO.dataPath <> io.dataPath.pSumIO(idx)
  })
  // connections of debugIO
  if (debug) {
    io.debugIO.inActDebugIO.zip(iSRAMs).foreach({ case (topDebug, sram) => topDebug <> sram.debugIO})
    io.debugIO.pSumDebugIO.zip(pSRAMs).foreach({ case (topDebug, sram) => topDebug <> sram.debugIO})
    io.debugIO.theState.foreach(_ := DontCare)
    io.debugIO.oneInActSRAMDone.foreach(_ := DontCare)
    //io.debugIO.theState.head <> Mux(io.ctrlPath.inActIO.readIO.enable, theSRAMsStateRegVec.head.head, theSRAMsStateRegVec.head.last)
    //io.debugIO.theState.last <> Mux(io.ctrlPath.pSumIO.readIO.enable, theSRAMsStateRegVec.last.head, theSRAMsStateRegVec.last.last)
    //io.debugIO.oneInActSRAMDone <> Mux(io.ctrlPath.inActIO.readIO.enable, theSRAMsDoneRegVec.head.head, theSRAMsDoneRegVec.head.last)
    io.debugIO.onePSumSRAMDone <> theSRAMsCtrl(1).map(x => Mux(x.readIO.enable, x.readIO.done, x.writeIO.done)) // so that's the wire not reg done
  } else {
    io.debugIO <> DontCare
  }
}

class PSumSRAMBank(private val theSRAMSize: Int, private val theDataWidth: Int, debug: Boolean)
  extends SRAMCommon(theSRAMSize, theDataWidth) {
  theSRAM.suggestName("onePSumSRAMBank")
  val io: PSumSRAMBankIO = IO(new PSumSRAMBankIO)
  // write logic
  writeLogic(io.dataPath.inIOs, io.ctrlPath.writeIO.enable, io.ctrlPath.writeIO.adr)
  // read logic
  // only use ready to control the read progress, so ready signal need to keep at least two cycles
  readLogic(io.dataPath.outIOs, io.ctrlPath.readIO.enable, io.ctrlPath.readIO.adr, false.B)
  io.ctrlPath.writeIO.done := DontCare
  io.ctrlPath.readIO.done := DontCare
  // debug io
  if (debug) {
    debugLogic(io.debugIO, io.ctrlPath.readIO.enable)
  } else {
    io.debugIO <> DontCare
  }
}

abstract class SRAMCommon(private val theSRAMSize: Int, private val theDataWidth: Int) extends Module {
  protected val theSRAM: SyncReadMem[UInt] = SyncReadMem(theSRAMSize, UInt(theDataWidth.W), SyncReadMem.ReadFirst)
  // SRAM read write logic
  protected val writeInData: UInt = Wire(UInt(theDataWidth.W))
  protected val readOutData: UInt = Wire(UInt(theDataWidth.W))
  // waitForRead: false, then ask for data; true, then data comes.
  protected val waitForRead: Bool = RegInit(false.B) // use for letting increase slower when read
  protected val nextValid: Bool = Wire(Bool())
  protected val nextValidReg: Bool = RegInit(false.B) // one cycle after nextValid
  protected val doReadWire: Bool = Wire(Bool())
  protected val doWriteWire: Bool = Wire(Bool())
  def readLogic(readOutDataIO: DecoupledIO[UInt], enable: Bool, idx: UInt, doDoneWire: Bool): Any = {
    doReadWire := enable && readOutDataIO.ready
    nextValid := doReadWire && !waitForRead
    nextValidReg := nextValid
    // when read, only enable signal becomes true and ready, then waitForRead signal began to flip
    waitForRead := Mux(!doDoneWire && readOutDataIO.ready, Mux(doReadWire, !waitForRead, waitForRead), false.B)
    readOutDataIO.valid := nextValidReg // reg next, so one cycle later, data will be read out with valid signal
    readOutData := theSRAM.read(idx, nextValid)
    readOutDataIO.bits := readOutData
  }
  def writeLogic(writeInDataIO: DecoupledIO[UInt], enable: Bool, idx: UInt): Any = {
    doWriteWire := enable && writeInDataIO.valid
    writeInData := writeInDataIO.bits
    writeInDataIO.ready := doWriteWire
    when(doWriteWire) {
      theSRAM.write(idx, writeInData)
    }
  }
  def debugLogic(debugIO: SRAMCommonDebugIO, enable: Bool): Any = {
    debugIO.waitForRead := waitForRead
    debugIO.doReadWire := enable
  }
}

class InActSRAMCommon(private val theSRAMSize: Int, private val theDataWidth: Int, debug: Boolean)
  extends SRAMCommon(theSRAMSize, theDataWidth) with GNMFCS2Config {
  theSRAM.suggestName("oneInActSRAM")
  val io: InACTSRAMCommonIO = IO(new InACTSRAMCommonIO(theSRAMSize, theDataWidth))
  // use lookup table to store the start index of each stream
  private val adrLookUpTable = Mem(inActStreamNum, UInt(log2Ceil(theSRAMSize).W))
  private val noZero :: oneZero :: twoZeros :: Nil = Enum(3)
  private val zeroState = Seq.fill(2){RegInit(noZero)} // 0 for read, 1 for write
  private val meetZeroWire = Wire(Vec(2, Bool())) // 0 for read, 1 for write
  private val writeDoneWire = Wire(Bool())
  private val readDoneWire = Wire(Bool())
  private val writeIdxReg = RegInit(0.U(width = log2Ceil(theSRAMSize).W)) // as counter
  private val readIdxIncReg = RegInit(0.U(width = log2Ceil(theSRAMSize).W)) // as counter
  // lookup table
  private val lookupTableWriteIdx = RegInit(0.U(log2Ceil(inActStreamNum).W))
  private val readZeroIdx = io.ctrlPath.readIO.adr === 0.U // then stream number is zero
  private val readStartIdx = Mux(readZeroIdx, 0.U,
    RegEnable(adrLookUpTable.read(io.ctrlPath.readIO.adr), 0.U(log2Ceil(theSRAMSize).W), io.ctrlPath.readIO.enable)
  )
  when (RegNext(meetZeroWire.last)) {
    // when meet zero, then next cycle, after the writeIdx reg increase, we need to record the start index of new stream
    adrLookUpTable.write(lookupTableWriteIdx, writeIdxReg)
  }
  lookupTableWriteIdx := Mux(writeDoneWire, 0.U,
    Mux(meetZeroWire.last && doWriteWire, lookupTableWriteIdx + 1.U, lookupTableWriteIdx)
  )
  // SRAM read write logic
  // if meet two continuous zeros, then the group of data finishes, also write finish
  private val meetTwoZerosWire = Wire(Bool())
  // TODO: add some logic to check whether one SPad of data is a zero matrix, true then jump
  io.ctrlPath.readIO.done := readDoneWire
  io.ctrlPath.writeIO.done := writeDoneWire
  // write logic
  writeLogic(io.dataPath.inIOs.data, enable = io.ctrlPath.writeIO.enable,
    idx = writeIdxReg
  )
  // when data has been write in, then write index increase one
  writeIdxReg := Mux(writeDoneWire, 0.U, Mux(doWriteWire, writeIdxReg + 1.U, writeIdxReg))
  // read logic
  readLogic(io.dataPath.outIOs.data, enable = io.ctrlPath.readIO.enable,
    idx = readIdxIncReg + readStartIdx, doDoneWire = readDoneWire
  )
  // when data has been read out, then read index increase one
  readIdxIncReg := Mux(readDoneWire, 0.U, Mux(nextValidReg, readIdxIncReg + 1.U, readIdxIncReg))
  // do finish?
  meetZeroWire.head := readOutData === 0.U && waitForRead
  meetZeroWire.last := writeInData === 0.U
  for (i <- 0 until 2) {
    switch(zeroState(i)) {
      is (noZero) {
        when (meetZeroWire(i)) {
          zeroState(i) := oneZero
        }
      }
      is (oneZero) {
        when (meetZeroWire(i)) {
          zeroState(i) := twoZeros
        } .otherwise {
          zeroState(i) := noZero
        }
      }
      is (twoZeros) {
        zeroState(i) := noZero // we assume there will not be three continuous zeros
      }
    }
  }
  readDoneWire := zeroState.head === oneZero // or meet one zero
  writeDoneWire := meetTwoZerosWire
  meetTwoZerosWire := zeroState.last === twoZeros // that's two zeros, only write need
  // debug io
  if (debug) {
    debugLogic(io.debugIO, nextValidReg)
    io.debugIO.currentData := Mux(io.ctrlPath.readIO.enable, readOutData, writeInData)
    io.debugIO.meetOneZero := readDoneWire
    io.debugIO.theState := Mux(io.ctrlPath.readIO.enable, zeroState.head, zeroState.last)
    io.debugIO.idx := Mux(io.ctrlPath.readIO.enable, readIdxIncReg + readStartIdx, writeIdxReg)
    io.debugIO.indexInc := readIdxIncReg
  } else {
    io.debugIO <> DontCare
  }
}

class InActSRAMBank(debug: Boolean) extends Module with ClusterSRAMConfig {
  val io: InActSRAMBankIO = IO(new InActSRAMBankIO)
  private val adrSRAM = Module(new InActSRAMCommon(inActAdrSRAMSize, inActAdrWidth, debug)).io
  adrSRAM.suggestName("adrSRAM")
  private val dataSRAM = Module(new InActSRAMCommon(inActDataSRAMSize, inActDataWidth, debug)).io
  dataSRAM.suggestName("dataSRAM")
  private val bothDoneWire = Wire(Vec(2,Bool()))
  private val inActIdle :: inActDoing :: inActWaitAdr :: inActWaitData :: Nil = Enum(4)
  private val inActState = Seq.fill(2){RegInit(inActIdle)}
  private val currentDoingWire = Wire(Vec(2, Bool()))
  private val adrDoneWire = Wire(Vec(2, Bool()))
  private val dataDoneWire = Wire(Vec(2, Bool()))
  adrDoneWire.head := adrSRAM.ctrlPath.readIO.done
  dataDoneWire.head := dataSRAM.ctrlPath.readIO.done
  adrDoneWire.last := adrSRAM.ctrlPath.writeIO.done
  dataDoneWire.last := dataSRAM.ctrlPath.writeIO.done
  adrSRAM.ctrlPath.readIO.enable := currentDoingWire.head || (inActState.head === inActWaitAdr)
  dataSRAM.ctrlPath.readIO.enable := currentDoingWire.head || (inActState.head === inActWaitData)
  adrSRAM.ctrlPath.writeIO.enable := currentDoingWire.last || (inActState.last === inActWaitAdr)
  dataSRAM.ctrlPath.writeIO.enable := currentDoingWire.last || (inActState.last === inActWaitData)
  private val topCtrlSeq = Seq(io.ctrlPath.readIO, io.ctrlPath.writeIO)
  for (i <- 0 until 2) {
    currentDoingWire(i) := inActState(i) === inActDoing
    bothDoneWire(i) := ((inActState(i) === inActWaitData) && dataDoneWire(i)) ||
      ((inActState(i) === inActWaitAdr) && adrDoneWire(i))
    switch(inActState(i)) {
      is (inActIdle) {
        when (topCtrlSeq(i).enable) {
          inActState(i) := inActDoing
        }
      }
      is (inActDoing) {
        when (adrDoneWire(i) && dataDoneWire(i)) {
          inActState(i) := inActIdle
        } .elsewhen (adrDoneWire(i)) {
          inActState(i) := inActWaitData
        } .elsewhen (dataDoneWire(i)) {
          inActState(i) := inActWaitAdr
        } .otherwise {
          inActState(i) := inActDoing
        }
      }
      is (inActWaitData) {
        when (dataDoneWire(i)) {
          inActState(i) := inActIdle
        }
      }
      is (inActWaitAdr) {
        when (adrDoneWire(i)) {
          inActState(i) := inActIdle
        }
      }
    }
  }
  // SRAM connections
  adrSRAM.dataPath.inIOs <> io.dataPath.inIOs.adrIOs
  adrSRAM.dataPath.outIOs <> io.dataPath.outIOs.adrIOs
  dataSRAM.dataPath.inIOs <> io.dataPath.inIOs.dataIOs
  dataSRAM.dataPath.outIOs <> io.dataPath.outIOs.dataIOs
  // control path
  Seq(adrSRAM.ctrlPath, dataSRAM.ctrlPath).foreach({ x =>
    x.readIO.adr := io.ctrlPath.readIO.adr // address for lookup table
    x.writeIO.adr := io.ctrlPath.writeIO.adr
  })
  io.ctrlPath.readIO.done := bothDoneWire.head
  io.ctrlPath.writeIO.done := bothDoneWire.last
  if (debug) {
    io.debugIO.theState := Mux(io.ctrlPath.readIO.enable, inActState.head, inActState.last)
    Seq(io.debugIO.adrDebug, io.debugIO.dataDebug).zip(Seq(adrSRAM, dataSRAM)).foreach({case (topIO, sram) =>
      topIO.subDone := Mux(io.ctrlPath.readIO.enable, sram.ctrlPath.readIO.done, sram.ctrlPath.writeIO.done)
      topIO.commonDebug <> sram.debugIO
    })
  } else {
    io.debugIO <> DontCare
  }
}

class SRAMCommonCtrlIO(private val theMemSize: Int) extends Bundle {
  val writeIO = new MeMReWrIO(theMemSize)
  val readIO = new MeMReWrIO(theMemSize)
}

class MeMReWrIO(private val theMemSize: Int) extends Bundle {
  val enable: Bool = Input(Bool())
  val adr: UInt = Input(UInt(log2Ceil(theMemSize).W))
  val done: Bool = Output(Bool())
}

class SRAMCommonDebugIO(private val theSRAMSize: Int, private val theDataWidth: Int) extends Bundle {
  val waitForRead: Bool = Output(Bool())
  val doReadWire: Bool = Output(Bool())
}

trait InActSpecialDebugIO extends Bundle with ClusterSRAMConfig {
  val indexInc: UInt = Output(UInt(30.W))
  val idx: UInt = Output(UInt(log2Ceil(inActDataSRAMSize).W))
  val currentData: UInt = Output(UInt(inActDataWidth.W))
  val meetOneZero: Bool = Output(Bool())
  val theState: UInt = Output(UInt(2.W))
}

class InActSRAMBankDebugSubIO(private val theSRAMSize: Int, private val theDataWidth: Int) extends Bundle {
  val commonDebug = new SRAMCommonDebugIO(theSRAMSize, theDataWidth) with InActSpecialDebugIO
  val subDone: Bool = Output(Bool())
}

class InACTSRAMCommonIO(private val theSRAMSize: Int, private val theDataWidth: Int) extends Bundle {
  val dataPath = new StreamBitsInOutIO(theDataWidth)
  val ctrlPath = new SRAMCommonCtrlIO(theMemSize = theSRAMSize)
  val debugIO = new SRAMCommonDebugIO(theSRAMSize, theDataWidth) with InActSpecialDebugIO
}

class InActSRAMBankDebugIO extends Bundle with ClusterSRAMConfig {
  val theState: UInt = Output(UInt(2.W))
  val adrDebug =  new InActSRAMBankDebugSubIO(inActAdrSRAMSize, inActAdrWidth)
  val dataDebug = new InActSRAMBankDebugSubIO(inActDataSRAMSize, inActDataWidth)
}

class InActSRAMBankIO extends Bundle with ClusterSRAMConfig with GNMFCS2Config {
  val dataPath = new CSCStreamInOutIO(inActAdrWidth, inActDataWidth)
  val ctrlPath = new SRAMCommonCtrlIO(inActStreamNum) // address data width only need to index stream numbers
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
  val ctrlPath = new SRAMCommonCtrlIO(theMemSize = pSumSRAMSize)
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

class GLBClusterCtrlIO extends Bundle with GNMFCS1Config with ClusterSRAMConfig {
  val inActIO: Vec[SRAMCommonCtrlIO] = Vec(inActSRAMNum, new SRAMCommonCtrlIO(inActDataSRAMSize))
  val pSumIO: Vec[SRAMCommonCtrlIO] =  Vec(pSumSRAMNum, new SRAMCommonCtrlIO(pSumSRAMSize))
}

class GLBClusterDataIO extends Bundle with ClusterSRAMConfig {
  val inActIO: Vec[CSCStreamInOutIO] = Vec(inActSRAMNum, new CSCStreamInOutIO(inActAdrWidth, inActDataWidth))
  val weightIO: Vec[CSCStreamInOutIO] = Vec(weightRouterNum, new CSCStreamInOutIO(weightAdrWidth, weightDataWidth))
  val pSumIO: Vec[PSumSRAMDataIO] = Vec(pSumSRAMNum, new PSumSRAMDataIO(psDataWidth))
}

class GLBClusterDebugIO extends Bundle with ClusterSRAMConfig {
  val oneInActSRAMDone: Vec[Bool] = Output(Vec(inActSRAMNum, Bool()))
  val onePSumSRAMDone: Vec[Bool] = Output(Vec(pSumSRAMNum, Bool()))
  val theState: Vec[UInt] = Output(Vec(2, UInt(2.W)))
  val inActDebugIO: Vec[InActSRAMBankDebugIO] = Vec(inActSRAMNum, new InActSRAMBankDebugIO)
  val pSumDebugIO: Vec[SRAMCommonDebugIO] = Vec(pSumSRAMNum, new SRAMCommonDebugIO(pSumSRAMSize, psDataWidth))
}

package dla.pe

import chisel3._
import chisel3.util._

class ProcessingElement(debug: Boolean) extends Module with PESizeConfig {
  val io: ProcessingElementIO = IO(new ProcessingElementIO)
  private val peCtrl = Module(new ProcessingElementControl(debug = debug))
  peCtrl.suggestName("peCtrlModule")
  private val peCtrlIO = peCtrl.io
  private val pePad = Module(new ProcessingElementPad(debug = debug))
  pePad.suggestName("pePadModule")
  private val pePadIO = pePad.io
  if (fifoEn) {
    pePadIO.dataStream.inActIOs.adrIOs.data <> Queue(io.dataStream.inActIOs.adrIOs.data, fifoSize, flow = true, pipe = true)
    pePadIO.dataStream.inActIOs.dataIOs.data <> Queue(io.dataStream.inActIOs.dataIOs.data, fifoSize, flow = true, pipe = true)
    pePadIO.dataStream.weightIOs.adrIOs.data <> Queue(io.dataStream.weightIOs.adrIOs.data, fifoSize, flow = true, pipe = true)
    pePadIO.dataStream.weightIOs.dataIOs.data <> Queue(io.dataStream.weightIOs.dataIOs.data, fifoSize, flow = true, pipe = true)
  } else {
    pePadIO.dataStream.inActIOs <> io.dataStream.inActIOs
    pePadIO.dataStream.weightIOs <> io.dataStream.weightIOs
 }
  private val inActAndWeightWFIOs = Seq(pePadIO.padWF.inActWriteFin, pePadIO.padWF.weightWriteFin)
  private val inActAndWeightTopWFIOs = Seq(io.padWF.inActWriteFin, io.padWF.weightWriteFin)
  inActAndWeightWFIOs.zip(inActAndWeightTopWFIOs).foreach{case (x, y) => y <> x}
  io.padWF.pSumAddFin := pePadIO.padWF.pSumAddFin
  peCtrlIO.ctrlPad <> pePadIO.padCtrl
  io.topCtrl.pSumEnqEn <> peCtrlIO.ctrlTop.pSumEnqEn
  io.topCtrl.calFinish := peCtrlIO.ctrlTop.calFinish
  peCtrlIO.ctrlTop.doLoadEn := io.topCtrl.doLoadEn
  private val SPadWFSeq = Seq(inActAndWeightWFIOs.head.adrWriteFin, inActAndWeightWFIOs.head.dataWriteFin,
    inActAndWeightWFIOs.last.adrWriteFin, inActAndWeightWFIOs.last.dataWriteFin)
  private val writeFinishWire: Bool = Wire(Bool())
  writeFinishWire.suggestName("inActAndWeightWFWire")
  private val writeFinishRegVec = Seq.fill(SPadWFSeq.length){RegInit(false.B)}
  writeFinishRegVec.head.suggestName("inActAdrWFReg")
  writeFinishRegVec(1).suggestName("inActDataWFReg")
  writeFinishRegVec(2).suggestName("weightAdrWFReg")
  writeFinishRegVec.last.suggestName("weightDataWFReg")
  for (i <- SPadWFSeq.indices) {
    writeFinishRegVec(i) := Mux(io.topCtrl.calFinish, false.B, Mux(SPadWFSeq(i), true.B, writeFinishRegVec(i)))
  }
  writeFinishWire := writeFinishRegVec.reduce(_ && _) // when inAct and Weight Scratch Pads write finished
  io.topCtrl.writeFinish := writeFinishWire
  peCtrlIO.ctrlTop.writeFinish := writeFinishWire
  pePadIO.dataStream.ipsIO <> Queue(io.dataStream.ipsIO, fifoSize, flow = true, pipe = true)
  io.dataStream.opsIO <> Queue(pePadIO.dataStream.opsIO, fifoSize, flow = true, pipe = true)
  if (debug) {
    io.debugIO.peControlDebugIO <> peCtrlIO.debugIO
    io.debugIO.peSPadDebugIO <> pePadIO.debugIO
    io.debugIO.writeFinishRegVec <> writeFinishRegVec
  } else {
    io.debugIO <> DontCare
  }
}

class ProcessingElementControl(debug: Boolean) extends Module with MCRENFConfig {
  val io: ProcessingElementControlIO = IO(new ProcessingElementControlIO)
  // state machine, control the process of MAC
  // psIdle: wait for signal
  // psLoad: load input activations, weights, partial sums outside and read out output partial sum
  // psCal: do MAC computations
  private val psIdle :: psLoad :: psCal :: Nil = Enum(3)
  private val stateMac: UInt = RegInit(psIdle) // the state of the mac process
  stateMac.suggestName("PEStateReg")
  io.ctrlTop.calFinish := io.ctrlPad.fromTopIO.calFinish && stateMac === psCal // TODO
  io.ctrlPad.fromTopIO.pSumEnqEn := io.ctrlTop.pSumEnqEn
  io.ctrlPad.fromTopIO.doLoadEn := io.ctrlTop.doLoadEn
  io.ctrlPad.doMACEn := stateMac === psCal
  switch (stateMac) {
    is (psIdle) {
      when (io.ctrlTop.doLoadEn) { // when there is any mac leaving
        stateMac := psLoad
      }
    }
    is (psLoad) {
      when (io.ctrlTop.writeFinish) { //after the pad receives the data
        stateMac := psCal
      }
    }
    is (psCal) {
      when (io.ctrlPad.fromTopIO.calFinish) {
        stateMac := psIdle
      }
    }
  }
  if (debug) {
    io.debugIO.peState := stateMac
    io.debugIO.doMACEnDebug := io.ctrlPad.doMACEn
  } else {
    io.debugIO <> DontCare
  }
}

class ProcessingElementPad(debug: Boolean) extends Module with MCRENFConfig with SPadSizeConfig with PESizeConfig {
  val io: ProcessingElementPadIO = IO(new ProcessingElementPadIO)
  private def nextSPadInActAdr(): Unit = {
    sPad := padInActAdr
    inActAdrSPadReadEnReg := true.B
    inActDataSPadReadEnReg := false.B
    weightAdrSPadReadEnReg := false.B
    weightDataSPadReadEnReg := false.B
    weightDataSPadFirstRead := true.B // if need read a new column of input activation matrix, then true
  }
  private def nextSPadInActData(): Unit = {
    sPad := padInActData
    inActAdrSPadReadEnReg := false.B
    inActDataSPadReadEnReg := true.B
    weightAdrSPadReadEnReg := false.B
    weightDataSPadReadEnReg := false.B
    weightDataSPadFirstRead := true.B // if need to read a new data, also a new weight matrix column
  }
  private def nextSPadWeightAdr(): Unit = {
    sPad := padWeightAdr
    inActAdrSPadReadEnReg := false.B
    inActDataSPadReadEnReg := false.B
    weightAdrSPadReadEnReg := true.B
    weightDataSPadReadEnReg := false.B
  }
  private def nextSPadWeightData(): Unit = {
    sPad := padWeightData1
    inActAdrSPadReadEnReg := false.B
    inActDataSPadReadEnReg := false.B
    weightAdrSPadReadEnReg := false.B
    weightDataSPadReadEnReg := true.B
  }
  private def readOff(): Unit = {
    inActAdrSPadReadEnReg := false.B
    inActDataSPadReadEnReg := false.B
    weightAdrSPadReadEnReg := false.B
    weightDataSPadReadEnReg := false.B
  }
  private def readFinish(): Unit = {
    sPad := padIdle
    inActMatrixColumnReg := 0.U
  }
  // reg, partial sum scratch pad
  private val pSumSPad = Module(new PSumSPad(debug = debug))
  pSumSPad.suggestName("pSumSPadModule")
  private val inActAdrSPad = Module(new SPadAdrModule(inActAdrSPadSize, inActAdrWidth))
  inActAdrSPad.suggestName("inActAdrSPadModule")
  private val inActDataSPad = Module(new SPadDataModule(inActDataSPadSize, inActDataWidth, false))
  inActDataSPad.suggestName("inActDataSPadModule")
  private val weightAdrSPad = Module(new WeightSPadAdrModule(weightAdrSPadSize, weightAdrWidth))
  weightAdrSPad.suggestName("weightAdrSPadModule")
  private val weightDataSPad = Module(new SPadDataModule(weightDataSPadSize, weightDataWidth, true))
  weightDataSPad.suggestName("weightDataSPadModule")
  // get the IOs
  private val pSumSPadIO = pSumSPad.io
  private val inActAdrSPadIO = inActAdrSPad.io
  private val inActDataSPadIO = inActDataSPad.io
  private val weightAdrSPadIO = weightAdrSPad.io
  private val weightDataSPadIO = weightDataSPad.io
  // SPadToCtrl
  // several signals which can help to indicate the process
  private val mightInActZeroColumnWire: Bool = Wire(Bool())
  private val inActSPadZeroColumnReg: Bool = RegInit(false.B) // true, it is a zero column, then need read again
  private val mightInActIdxIncWire: Bool = Wire(Bool())
  private val mightWeightZeroColumnWire: Bool = Wire(Bool())
  private val mightWeightIdxIncWire: Bool = Wire(Bool())
  private val mightInActReadFinish: Bool = Wire(Bool())
  private val mightWeightReadFinish: Bool = Wire(Bool())
  private val psDataSPadIdxWire: UInt = Wire(UInt(log2Ceil(pSumDataSPadSize).W))
  // InActSPad
  private val inActAdrIndexWire: UInt = Wire(UInt(inActAdrIdxWidth.W))
  private val inActAdrDataWire: UInt = Wire(UInt(inActAdrWidth.W))
  private val inActDataIndexWire: UInt = Wire(UInt(inActDataIdxWidth.W)) // use for address vector readEn
  private val inActAdrSPadReadEnReg: Bool = RegInit(false.B)
  private val inActDataSPadReadEnReg: Bool = RegInit(false.B)
  private val inActAdrSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of inAct address SPad
  private val inActDataSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of inAct data SPad
  private val inActMatrixColumnReg: UInt = RegInit(0.U(inActAdrIdxWidth.W))
  private val inActZeroColumnNumber: UInt = RegInit(0.U(inActAdrIdxWidth.W)) // use for get the right column number
  private val inActDataSPadFirstReadReg: Bool = RegInit(true.B)
  private val inActMatrixRowWire: UInt = Wire(UInt(cscCountWidth.W))
  inActMatrixRowWire.suggestName("inActMatrixRowWire")
  private val inActMatrixDataWire: UInt = Wire(UInt(cscDataWidth.W))
  inActMatrixDataWire.suggestName("inActMatrixDataWire")
  // WeightSPad
  private val weightAdrIndexWire: UInt = Wire(UInt(weightAdrIdxWidth.W))
  private val weightAdrDataWire: UInt = Wire(UInt(weightAdrWidth.W))
  private val weightDataIndexWire: UInt = Wire(UInt(weightDataIdxWidth.W)) // use for address vector readEn
  private val weightAdrSPadReadEnReg: Bool = RegInit(false.B)
  private val weightDataSPadReadEnReg: Bool = RegInit(false.B)
  private val weightAdrSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of weight address SPad
  private val weightDataSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of weight data SPad
  private val weightMatrixDataReg: UInt = Wire(UInt(cscDataWidth.W))
  private val weightDataSPadFirstRead: Bool = RegInit(true.B)
  private val weightDataIdxMuxWire: Bool = Wire(Bool()) // false, then means need to read the first column of weight Matrix
  private val weightAdrSPadReadIdxWire: UInt = Wire(UInt(cscCountWidth.W))
  private val weightDataIdxEnWire: Bool = Wire(Bool()) // true then assign a special one for data index
  private val weightAdrIdxEnWire: Bool = Wire(Bool()) // true then assign a special one for data index
  private val weightMatrixReadFirstColumn: Bool = Wire(Bool())
  // pSumSPadIO
  private val productReg: UInt = RegInit(0.U(psDataWidth.W))
  private val pSumSPadLoadReg: UInt = RegInit(0.U(psDataWidth.W))
  private val pSumSPadLoadWire: UInt = Wire(UInt(psDataWidth.W))
  // State Machine
  // padIdle: the pad is idle, and if received valid signal from control, then read and send data to mac
  // padInActAdr: read the input activation address
  // padInActData: read the input activation data
  // padWeightAdr: read the weight address
  // padWeightData1: read the weight data
  // padWeightData2: wait one cycle as SRAM
  // padMpy: wait for mac computation
  // padWriteBack: write the partial sum back
  private val padIdle :: padInActAdr :: padInActData :: padWeightAdr :: padWeightData1 :: padWeightData2 :: padMpy :: padWriteBack :: Nil = Enum(8)
  private val sPad: UInt = RegInit(padIdle)
  sPad.suggestName("PESPadStateReg")
  private val padEqIA: Bool = Wire(Bool())
  private val padEqID: Bool = Wire(Bool())
  private val padEqWA: Bool = Wire(Bool())
  private val padEqMpy: Bool = Wire(Bool())
  private val padEqWB: Bool = Wire(Bool())
  padEqIA := sPad === padInActAdr
  padEqMpy := sPad === padMpy
  padEqWB := sPad === padWriteBack
  padEqWA := sPad === padWeightAdr
  padEqID := sPad === padInActData
  weightMatrixReadFirstColumn := inActMatrixRowWire === 0.U
  private val weightMatrixRowReg: UInt = Wire(UInt(cscCountWidth.W))
  private val SPadSeq = Seq(inActAdrSPadIO, inActDataSPadIO, weightAdrSPadIO, weightDataSPadIO)
  // Connections
  SPadSeq.map(_.ctrlPath.writeEn := io.padCtrl.fromTopIO.doLoadEn)
  io.padCtrl.fromTopIO.writeFinish := DontCare
  // Input activation Address Scratch Pad
  inActAdrSPadIO.dataPath.writeInData <> io.dataStream.inActIOs.adrIOs
  inActAdrIndexWire := inActAdrSPadIO.dataPath.columnNum
  inActAdrDataWire := inActAdrSPadIO.dataPath.readOutData
  io.padWF.inActWriteFin.adrWriteFin := inActAdrSPadIO.ctrlPath.writeFin
  inActAdrSPadIO.ctrlPath.readEn := inActAdrSPadReadEnReg
  inActAdrSPadIO.ctrlPath.readInIdx := DontCare
  inActAdrSPadIO.ctrlPath.indexInc := inActAdrSPadIdxIncWire
  inActAdrSPadIO.ctrlPath.readInIdxEn := DontCare
  // Input activation Data Scratch Pad
  inActDataSPadIO.dataPath.writeInData <> io.dataStream.inActIOs.dataIOs
  inActDataIndexWire := inActDataSPadIO.dataPath.columnNum
  inActMatrixDataWire := inActDataSPadIO.dataPath.readOutData(cscDataWidth + cscCountWidth - 1, cscCountWidth)
  inActMatrixRowWire := inActDataSPadIO.dataPath.readOutData(cscCountWidth - 1, 0)
  io.padWF.inActWriteFin.dataWriteFin := inActDataSPadIO.ctrlPath.writeFin
  inActDataSPadIO.ctrlPath.readEn := inActDataSPadReadEnReg
  inActDataSPadIO.ctrlPath.readInIdx := inActAdrDataWire
  inActDataSPadIO.ctrlPath.indexInc := inActDataSPadIdxIncWire
  inActDataSPadIO.ctrlPath.readInIdxEn := DontCare
  // Weight Address Scratch Pad
  weightAdrSPadIO.dataPath.writeInData <> io.dataStream.weightIOs.adrIOs
  weightAdrIndexWire := weightAdrSPadIO.dataPath.columnNum
  weightAdrDataWire := weightAdrSPadIO.dataPath.readOutData
  io.padWF.weightWriteFin.adrWriteFin := weightAdrSPadIO.ctrlPath.writeFin
  weightAdrSPadIO.ctrlPath.readEn := weightAdrSPadReadEnReg
  weightAdrSPadIO.ctrlPath.readInIdx := weightAdrSPadReadIdxWire // the weight address SPad's columns corresponds to
                                                          // the inAct address SPad's rows, and it takes one clock cycle
                                                          // for the reg inside SPad to change the index it need
  weightAdrSPadIO.ctrlPath.indexInc := weightAdrSPadIdxIncWire
  weightAdrSPadIO.ctrlPath.readInIdxEn := weightAdrIdxEnWire
  // Weight Data Scratch Pad
  weightDataSPadIO.dataPath.writeInData <> io.dataStream.weightIOs.dataIOs
  weightDataIndexWire := weightDataSPadIO.dataPath.columnNum
  weightMatrixDataReg := weightDataSPadIO.dataPath.readOutData(cscDataWidth + cscCountWidth - 1, cscCountWidth)
  weightMatrixRowReg := weightDataSPadIO.dataPath.readOutData(cscCountWidth - 1, 0)
  io.padWF.weightWriteFin.dataWriteFin := weightDataSPadIO.ctrlPath.writeFin
  weightDataSPadIO.ctrlPath.readEn := inActDataSPadReadEnReg
  weightDataSPadIO.ctrlPath.readInIdx := Mux(weightMatrixReadFirstColumn, 0.U, weightAdrDataWire)
  weightDataSPadIO.ctrlPath.indexInc := weightDataSPadIdxIncWire
  weightDataSPadIO.ctrlPath.readInIdxEn := weightDataIdxEnWire
  // Partial Sum Scratch Pad
  val pSumAddResultWire: UInt = Mux(io.padCtrl.fromTopIO.pSumEnqEn, pSumSPadLoadWire, pSumSPadLoadReg) +
    Mux(io.padCtrl.fromTopIO.pSumEnqEn, io.dataStream.ipsIO.bits, productReg) // to save adder
  private val pSumResultWire = Mux(padEqWB, pSumAddResultWire, 0.U)
  private val pSumPadReadIdxReg = RegInit(0.U(log2Ceil(pSumDataSPadSize).W))
  pSumPadReadIdxReg.suggestName("pSumPadReadIdxReg")
  private val pSumPadReadIdxIncWire = Wire(Bool())
  pSumPadReadIdxIncWire.suggestName("pSumPadReadIdxIncWire")
  private val pSumPadReadWrap = Wire(Bool())
  private val pSumReadOutValid = io.dataStream.ipsIO.valid && io.padCtrl.fromTopIO.pSumEnqEn
  // top connections
  io.padWF.pSumAddFin := pSumPadReadWrap && io.padCtrl.fromTopIO.pSumEnqEn // when need read and wrap
  io.dataStream.ipsIO.ready := io.dataStream.opsIO.ready && io.padCtrl.fromTopIO.pSumEnqEn
  /** the PSumSPad read index only increases when io.dataStream.opsIO gets the updated PSum*/
  pSumPadReadIdxIncWire := io.dataStream.opsIO.fire()
  // only when need enqueue, and pSumSPadIO has read out data, and io.dataStream.ops has received data,
  // then increase read index
  io.dataStream.opsIO.bits := pSumAddResultWire
  /** io.dataStream.opsIO.valid when it reads one PSum from PSumSPad
    * AND it can get another PSum from io.dataStream.ipsIO, then can add them together
    * and get the io.dataStream.opsIO.bits*/
  io.dataStream.opsIO.valid := pSumSPadIO.dataPath.opsIO.fire() && pSumReadOutValid
  // once ask for Enq, pSumSPadIO will wait for io.dataStream.ipsIO.valid, then read out data,
  // and add the data read out from pSumSPadIO with io.dataStream.ipsIO.bit.
  // io.dataStream.opsIO.valid will be true when those two addends valid.
  // The read address will increase after io.dataStream.ops.ready is true, as well as io.dataStream.ips.ready.
  // Because we have Queue, so we don't need to worry about the timing of this combination logic.
  pSumSPadIO.dataPath.ipsIO.valid := padEqWB
  pSumSPadIO.dataPath.ipsIO.bits := pSumResultWire // only calculate need write back
  pSumSPadLoadWire := pSumSPadIO.dataPath.opsIO.bits // for emergence use
  pSumSPadLoadReg := pSumSPadLoadWire // used in calculating
  // when ips valid and Enabled, then need read out PSum
  pSumSPadIO.dataPath.opsIO.ready := padEqMpy || pSumReadOutValid
  pSumSPadIO.ctrlPath.readIdx := pSumPadReadIdxReg
  pSumSPadIO.ctrlPath.writeIdx := psDataSPadIdxWire // only calculate need write back
  when (pSumPadReadWrap) {
    pSumPadReadIdxReg := 0.U
  }
  when (pSumPadReadIdxIncWire) {
    pSumPadReadIdxReg := pSumPadReadIdxReg + 1.U
  }

  pSumPadReadWrap := (padEqID && mightInActReadFinish) || (padEqWB && mightInActReadFinish) ||
    (pSumPadReadIdxIncWire && pSumPadReadIdxReg === (M0*E*N0*F0 -1 ).U) // no need to use mux
  // SPadToCtrl
  mightInActZeroColumnWire := inActAdrDataWire === inActZeroColumnCode.U
  mightWeightZeroColumnWire := weightAdrDataWire === weightZeroColumnCode.U
  mightInActIdxIncWire := inActAdrDataWire === (inActDataIndexWire + 1.U)
  mightWeightIdxIncWire := weightAdrDataWire === (weightDataIndexWire + 1.U) || mightWeightReadFinish // or meet finish signal
  mightInActReadFinish := inActMatrixDataWire === 0.U && !inActDataSPadFirstReadReg
  mightWeightReadFinish := weightMatrixDataReg === 0.U && !weightDataSPadFirstRead
  inActAdrSPadIdxIncWire := (padEqIA && mightInActZeroColumnWire) || (((padEqWA && mightWeightZeroColumnWire) ||
    (padEqWB && mightWeightIdxIncWire)) && mightInActIdxIncWire)
  weightAdrSPadIdxIncWire := (padEqMpy || sPad === padWeightData1) && mightWeightZeroColumnWire // FIXME: should add a state
  // if first read, then keep the read index of zero
  inActDataSPadIdxIncWire := (padEqIA && !mightInActZeroColumnWire && !inActDataSPadFirstReadReg) ||
    (((padEqWA && mightWeightZeroColumnWire) || (padEqWB && mightWeightIdxIncWire)) && !mightInActIdxIncWire)
  weightDataSPadIdxIncWire := (padEqWA && !mightWeightZeroColumnWire && !weightDataSPadFirstRead) ||
    (padEqWB && !mightWeightIdxIncWire) // when first read, ask Weight Address Scratch Pad for data index
  weightAdrIdxEnWire := (padEqID || padEqWA) && weightDataSPadFirstRead // read the start and end index from address SPad
  // then it can read the start index in weightDataSPadIO, the end index of that will be read otherwise
  weightDataIdxMuxWire := padEqID && weightDataSPadFirstRead && !weightMatrixReadFirstColumn
  weightAdrSPadReadIdxWire := Mux(weightDataIdxMuxWire, inActMatrixRowWire - 1.U, inActMatrixRowWire)
  weightDataIdxEnWire := padEqWA && weightDataSPadFirstRead && !mightWeightZeroColumnWire
  io.padCtrl.fromTopIO.calFinish := mightInActReadFinish
  psDataSPadIdxWire := weightMatrixRowReg + inActMatrixColumnReg * M0.U
  switch (sPad) {
    is (padIdle) {
      when(io.padCtrl.doMACEn) {
        nextSPadInActAdr()
        inActDataSPadFirstReadReg := true.B
      }
    }
    is (padInActAdr) {
      when (mightInActZeroColumnWire) { // then it is a zero column
        nextSPadInActAdr()
        inActSPadZeroColumnReg := true.B
        inActZeroColumnNumber := inActZeroColumnNumber + 1.U
      } .otherwise {
        nextSPadInActData()
      }
    }
    is (padInActData) {
      when (mightInActReadFinish) {
        readFinish()
      } .otherwise {
        nextSPadWeightAdr()
        inActDataSPadFirstReadReg := false.B
      }
    }
    is (padWeightAdr) {
      when (mightWeightZeroColumnWire) { // need to get next inAct
        when (mightInActIdxIncWire) { // if have read all elements in current inAct Matrix column
          nextSPadInActAdr()
          when (inActSPadZeroColumnReg) {
            inActSPadZeroColumnReg := false.B
            inActMatrixColumnReg := inActMatrixColumnReg + 1.U + inActZeroColumnNumber
            inActZeroColumnNumber := 0.U
          } .otherwise {
            inActMatrixColumnReg := inActMatrixColumnReg + 1.U
          }
        } .otherwise { // still some elements in current inAct Matrix column
          nextSPadInActData()
        }
      } .otherwise { // then it isn't a zero column, can do MAC
        nextSPadWeightData()
      }
    }
    is (padWeightData1) {
      sPad := padWeightData2
    }
    is (padWeightData2) {
      sPad := padMpy
      pSumPadReadIdxReg := psDataSPadIdxWire
      readOff()
    }
    is (padMpy) {
      sPad := padWriteBack
      productReg :=  weightMatrixDataReg * inActMatrixDataWire
      // then load pSum
      //pSumSPadLoadReg := psDataSPadReg(pSumPadReadIdxReg)
    }
    is (padWriteBack) {
      // then write back pSum
      //psDataSPadReg(pSumPadWriteIdxWire) := pSumResultWire //update the partial sum
      when (mightInActReadFinish) {
        readFinish()
      } .otherwise { // then haven't done all the MAC operations
        when (mightWeightIdxIncWire) { // finished read current weight data Matrix column
          when (mightInActIdxIncWire) { // finished read current inAct data Matrix column
            nextSPadInActAdr()
            when (inActSPadZeroColumnReg) {
              inActSPadZeroColumnReg := false.B
              inActMatrixColumnReg := inActMatrixColumnReg + 1.U + inActZeroColumnNumber
              inActZeroColumnNumber := 0.U
            } .otherwise {
              inActMatrixColumnReg := inActMatrixColumnReg + 1.U
            }
          } .otherwise {
            nextSPadInActData()
          }
        } .otherwise {
          nextSPadWeightData()
          weightDataSPadFirstRead := false.B // as it has been read current weight matrix column
        }
      }
    }
  }

  if (debug) {
    io.debugIO.inActMatrixColumn := inActMatrixColumnReg
    io.debugIO.inActMatrixData := inActMatrixDataWire
    io.debugIO.inActMatrixRow := inActMatrixRowWire
    io.debugIO.inActAdrIdx := inActAdrIndexWire
    io.debugIO.inActAdrInc := inActAdrSPadIdxIncWire
    io.debugIO.inActDataInc := inActDataSPadIdxIncWire
    io.debugIO.weightMatrixData := weightMatrixDataReg
    io.debugIO.weightMatrixRow := weightMatrixRowReg
    io.debugIO.weightAdrSPadReadOut := weightAdrDataWire
    io.debugIO.productResult := productReg
    io.debugIO.pSumResult := pSumResultWire
    io.debugIO.pSumLoad := pSumSPadLoadReg
    io.debugIO.weightAdrInIdx := weightAdrSPadReadIdxWire
    io.debugIO.sPadState := sPad
    io.debugIO.pSumReadIdx := pSumPadReadIdxReg
  }else {
    io.debugIO <> DontCare
  }
}

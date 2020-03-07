package dla.pe

import chisel3._
import chisel3.util._
// TODO: add reset signal for every module
class ProcessingElement(debug: Boolean) extends Module with PESizeConfig {
  val io: ProcessingElementIO = IO(new ProcessingElementIO)
  private val peCtrl = Module(new ProcessingElementControl(debug = debug)).io
  peCtrl.suggestName("peCtrl")
  private val pePad = Module(new ProcessingElementPad(debug = debug)).io
  pePad.suggestName("pePad")
  if (fifoEn) {
    pePad.dataStream.inActIOs.adrIOs.data <> Queue(io.dataStream.inActIOs.adrIOs.data, fifoSize, flow = true)
    pePad.dataStream.inActIOs.dataIOs.data <> Queue(io.dataStream.inActIOs.dataIOs.data, fifoSize, flow = true)
    pePad.dataStream.weightIOs.adrIOs.data <> Queue(io.dataStream.weightIOs.adrIOs.data, fifoSize, flow = true)
    pePad.dataStream.weightIOs.dataIOs.data <> Queue(io.dataStream.weightIOs.dataIOs.data, fifoSize, flow = true)
  } else {
    pePad.dataStream.inActIOs <> io.dataStream.inActIOs
    pePad.dataStream.weightIOs <> io.dataStream.weightIOs
 }
  private val inActAndWeightWFIOs = Seq(pePad.padWF.inActWriteFin, pePad.padWF.weightWriteFin)
  val inActAndWeightTopWFIOs = Seq(io.padWF.inActWriteFin, io.padWF.weightWriteFin)
  inActAndWeightWFIOs.zip(inActAndWeightTopWFIOs).foreach{case (x, y) => y <> x}
  io.padWF.pSumWriteFin := pePad.padWF.pSumWriteFin
  peCtrl.ctrlPad <> pePad.padCtrl
  io.topCtrl.pSumEnqEn <> peCtrl.ctrlTop.pSumEnqEn
  io.topCtrl.calFinish := peCtrl.ctrlTop.calFinish
  peCtrl.ctrlTop.doLoadEn := io.topCtrl.doLoadEn
  private val SPadWFSeq = Seq(inActAndWeightWFIOs.head.adrWriteFin, inActAndWeightWFIOs.head.dataWriteFin,
    inActAndWeightWFIOs.last.adrWriteFin, inActAndWeightWFIOs.last.dataWriteFin, pePad.padWF.pSumWriteFin)
  private val writeFinishWire: Bool = Wire(Bool())
  private val writeFinishRegVec: Vec[Bool] = RegInit(VecInit(Seq.fill(SPadWFSeq.length)(false.B)))
  for (i <- 0 until 5) {
    when (SPadWFSeq(i)) {
      writeFinishRegVec(i) := true.B
    }
    when (io.topCtrl.calFinish) {
      writeFinishRegVec(i) := false.B
    }
  }
  writeFinishWire := writeFinishRegVec.reduce(_ && _) // when all five Scratch Pads write finished
  io.topCtrl.writeFinish := writeFinishWire
  peCtrl.ctrlTop.writeFinish := writeFinishWire
  pePad.dataStream.ipsIO <> Queue(io.dataStream.ipsIO, fifoSize, flow = true)
  io.dataStream.opsIO <> Queue(pePad.dataStream.opsIO, fifoSize, flow = true)
  if (debug) {
    io.debugIO.peControlDebugIO <> peCtrl.debugIO
    io.debugIO.peSPadDebugIO <> pePad.debugIO
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
  io.ctrlTop.calFinish := io.ctrlPad.fromTopIO.calFinish
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

class PSumSPad(debug: Boolean) extends Module with SPadSizeConfig with PESizeConfig {
  val io: PSumSPadIO = IO(new PSumSPadIO)
  private val pSumDataSPadReg: Vec[UInt] = RegInit(VecInit(Seq.fill(pSumDataSPadSize)(0.U(psDataWidth.W))))
  pSumDataSPadReg.suggestName("pSumDataSPadReg")
  private val readOutDataWire = Wire(UInt(psDataWidth.W))
  readOutDataWire := pSumDataSPadReg(io.ctrlPath.readIdx)
  io.dataPath.ipsIO.ready := io.dataPath.ipsIO.valid
  io.dataPath.opsIO.valid := io.dataPath.opsIO.ready
  io.dataPath.opsIO.bits := Mux(io.dataPath.opsIO.ready, readOutDataWire, DontCare)
  when (io.dataPath.ipsIO.valid) {
    pSumDataSPadReg(io.ctrlPath.writeIdx) := io.dataPath.ipsIO.bits
  }
}

class PSumSPadIO extends Bundle {
  val dataPath = new PSumSPadDataIO
  val ctrlPath = new PSumSPadCtrlIO
}

class PSumSPadDataIO extends Bundle with PESizeConfig {
  val ipsIO: DecoupledIO[UInt] = Flipped(Decoupled(UInt(psDataWidth.W)))
  val opsIO: DecoupledIO[UInt] = Decoupled(UInt(psDataWidth.W))
}

class PSumSPadCtrlIO extends Bundle with SPadSizeConfig {
  val readIdx: UInt = Input(UInt(log2Ceil(pSumDataSPadSize).W))
  val writeIdx: UInt = Input(UInt(log2Ceil(pSumDataSPadSize).W))
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
    //psPadReadIdxCounter.value := 0.U
  }
  // reg, partial sum scratch pad
  private val pSumSPad = Module(new PSumSPad(debug = debug)).io
  pSumSPad.suggestName("pSumSPad")
  private val inActAdrSPad = Module(new SPadAdrModule(inActAdrSPadSize, inActAdrWidth)).io
  inActAdrSPad.suggestName("inActAdrSPad")
  private val inActDataSPad = Module(new SPadDataModule(inActDataSPadSize, inActDataWidth, false)).io
  inActDataSPad.suggestName("inActDataSPad")
  private val weightAdrSPad = Module(new WeightSPadAdrModule(weightAdrSPadSize, weightAdrWidth)).io
  weightAdrSPad.suggestName("weightAdrSPad")
  private val weightDataSPad = Module(new SPadDataModule(weightDataSPadSize, weightDataWidth, true)).io
  weightDataSPad.suggestName("weightDataSPad")
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
  private val inActMatrixDataWire: UInt = Wire(UInt(cscDataWidth.W))
  // WeightSPad
  private val weightAdrIndexWire: UInt = Wire(UInt(weightAdrIdxWidth.W))
  private val weightAdrDataWire: UInt = Wire(UInt(weightAdrWidth.W))
  private val weightDataIndexWire: UInt = Wire(UInt(weightDataIdxWidth.W)) // use for address vector readEn
  private val weightAdrSPadReadEnReg: Bool = RegInit(false.B)
  private val weightDataSPadReadEnReg: Bool = RegInit(false.B)
  private val weightAdrSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of weight address SPad
  private val weightDataSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of weight data SPad
  private val weightMatrixDataReg: UInt = Wire(UInt(cscDataWidth.W))
  //val weightMatrixDataReg: UInt = RegInit(0.U(cscDataWidth.W))
  private val weightDataSPadFirstRead: Bool = RegInit(true.B)
  private val weightDataIdxMuxWire: Bool = Wire(Bool()) // false, then means need to read the first column of weight Matrix
  private val weightAdrSPadReadIdxWire: UInt = Wire(UInt(cscCountWidth.W))
  private val weightDataIdxEnWire: Bool = Wire(Bool()) // true then assign a special one for data index
  private val weightAdrIdxEnWire: Bool = Wire(Bool()) // true then assign a special one for data index
  private val weightMatrixReadFirstColumn: Bool = Wire(Bool())
  // pSumSPad
  private val productReg: UInt = RegInit(0.U(psDataWidth.W))
  private val pSumSPadLoadReg: UInt = RegInit(0.U(psDataWidth.W))
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
  private val SPadSeq = Seq(inActAdrSPad, inActDataSPad, weightAdrSPad, weightDataSPad)
  // Connections
  SPadSeq.map(_.ctrlPath.writeEn := io.padCtrl.fromTopIO.doLoadEn)
  io.padCtrl.fromTopIO.writeFinish := DontCare
  // Input activation Address Scratch Pad
  inActAdrSPad.dataPath.writeInData <> io.dataStream.inActIOs.adrIOs
  inActAdrIndexWire := inActAdrSPad.dataPath.columnNum
  inActAdrDataWire := inActAdrSPad.dataPath.readOutData
  io.padWF.inActWriteFin.adrWriteFin := inActAdrSPad.ctrlPath.writeFin
  inActAdrSPad.ctrlPath.readEn := inActAdrSPadReadEnReg
  inActAdrSPad.ctrlPath.readInIdx := DontCare
  inActAdrSPad.ctrlPath.indexInc := inActAdrSPadIdxIncWire
  inActAdrSPad.ctrlPath.readInIdxEn := DontCare
  // Input activation Data Scratch Pad
  inActDataSPad.dataPath.writeInData <> io.dataStream.inActIOs.dataIOs
  inActDataIndexWire := inActDataSPad.dataPath.columnNum
  private val inActDataCountVec: Seq[Bool] = inActDataSPad.dataPath.readOutData.asBools()
  inActMatrixDataWire := Cat(inActDataCountVec.reverse.take(cscDataWidth)).asUInt // TODO: figure out why it need reverse
  inActMatrixRowWire := Cat(inActDataCountVec.reverse.takeRight(cscCountWidth)).asUInt
  io.padWF.inActWriteFin.dataWriteFin := inActDataSPad.ctrlPath.writeFin
  inActDataSPad.ctrlPath.readEn := inActDataSPadReadEnReg
  inActDataSPad.ctrlPath.readInIdx := inActAdrDataWire
  inActDataSPad.ctrlPath.indexInc := inActDataSPadIdxIncWire
  inActDataSPad.ctrlPath.readInIdxEn := DontCare
  // Weight Address Scratch Pad
  weightAdrSPad.dataPath.writeInData <> io.dataStream.weightIOs.adrIOs
  weightAdrIndexWire := weightAdrSPad.dataPath.columnNum
  weightAdrDataWire := weightAdrSPad.dataPath.readOutData
  io.padWF.weightWriteFin.adrWriteFin := weightAdrSPad.ctrlPath.writeFin
  weightAdrSPad.ctrlPath.readEn := weightAdrSPadReadEnReg
  weightAdrSPad.ctrlPath.readInIdx := weightAdrSPadReadIdxWire // the weight address SPad's columns corresponds to
                                                          // the inAct address SPad's rows, and it takes one clock cycle
                                                          // for the reg inside SPad to change the index it need
  weightAdrSPad.ctrlPath.indexInc := weightAdrSPadIdxIncWire
  weightAdrSPad.ctrlPath.readInIdxEn := weightAdrIdxEnWire
  // Weight Data Scratch Pad
  weightDataSPad.dataPath.writeInData <> io.dataStream.weightIOs.dataIOs
  weightDataIndexWire := weightDataSPad.dataPath.columnNum
  private val weightDataCountVec: Seq[Bool] = weightDataSPad.dataPath.readOutData.asBools()
  weightMatrixDataReg := Cat(weightDataCountVec.reverse.take(cscDataWidth)).asUInt
  weightMatrixRowReg := Cat(weightDataCountVec.reverse.takeRight(cscCountWidth)).asUInt
  io.padWF.weightWriteFin.dataWriteFin := weightDataSPad.ctrlPath.writeFin
  weightDataSPad.ctrlPath.readEn := inActDataSPadReadEnReg
  weightDataSPad.ctrlPath.readInIdx := Mux(weightMatrixReadFirstColumn, 0.U, weightAdrDataWire)
  weightDataSPad.ctrlPath.indexInc := weightDataSPadIdxIncWire
  weightDataSPad.ctrlPath.readInIdxEn := weightDataIdxEnWire
  // Partial Sum Scratch Pad
  private val pSumResultWire = Mux(padEqWB, pSumSPadLoadReg + productReg, 0.U)
  private val pSumPadReadIdxReg = RegInit(0.U(log2Ceil(pSumDataSPadSize).W))
  pSumPadReadIdxReg.suggestName("pSumPadReadIdxReg")
  private val pSumPadReadIdxInc = Wire(Bool())
  private val pSumPadReadWrap = Wire(Bool())
  private val pSumPadWriteIdxReg = RegInit(0.U(log2Ceil(pSumDataSPadSize).W))
  private val pSumPadWriteIdxInc = Wire(Bool())
  pSumPadWriteIdxInc.suggestName("pSumPadWriteIdxInc")
  private val pSumPadWriteWrap = Wire(Bool())
  private val pSumPadWriteIdxWire = Mux(pSumPadWriteIdxInc, pSumPadWriteIdxReg, psDataSPadIdxWire)
  pSumPadWriteIdxWire.suggestName("pSumPadWriteIdxWire")
  // top connections
  io.padWF.pSumWriteFin := pSumPadWriteWrap
  pSumPadWriteIdxInc := io.padCtrl.fromTopIO.pSumEnqEn && io.dataStream.ipsIO.valid
  pSumSPad.dataPath.ipsIO.bits := Mux(pSumPadWriteIdxInc, io.dataStream.ipsIO.bits, pSumResultWire)
  io.dataStream.ipsIO.ready := pSumSPad.dataPath.ipsIO.ready
  pSumPadReadIdxInc := io.dataStream.opsIO.ready
  io.dataStream.opsIO.bits := pSumSPad.dataPath.opsIO.bits
  io.dataStream.opsIO.valid := Mux(io.dataStream.opsIO.ready, pSumSPad.dataPath.opsIO.valid, false.B)
  // once ask for Enq, then pSum is write in data, then another pe's ops.ready === true.B,
  // then another pe read out data with true valid signal, then pSumPadWriteIdxInc === true.B
  pSumSPad.dataPath.ipsIO.valid := io.padCtrl.fromTopIO.pSumEnqEn || padEqWB
  pSumSPadLoadReg := pSumSPad.dataPath.opsIO.bits
  pSumSPad.dataPath.opsIO.ready := padEqMpy || pSumPadReadIdxInc
  pSumSPad.ctrlPath.readIdx := pSumPadReadIdxReg
  pSumSPad.ctrlPath.writeIdx := pSumPadWriteIdxWire
  when (pSumPadReadWrap) {
    pSumPadReadIdxReg := 0.U
  }
  when (pSumPadReadIdxInc) {
    pSumPadReadIdxReg := pSumPadReadIdxReg + 1.U
  }
  when (pSumPadWriteWrap) {
    pSumPadWriteIdxReg := 0.U
  }
  when (pSumPadWriteIdxInc) {
    pSumPadWriteIdxReg := pSumPadWriteIdxReg + 1.U
  }

  pSumPadReadWrap := (padEqID && mightInActReadFinish) || (padEqWB && mightInActReadFinish)
  pSumPadWriteWrap := pSumPadWriteIdxReg === (M0*E*N0*F0 - 1).U && pSumPadWriteIdxInc
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
  // then it can read the start index in weightDataSPad, the end index of that will be read otherwise
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
  }else {
    io.debugIO <> DontCare
  }
}

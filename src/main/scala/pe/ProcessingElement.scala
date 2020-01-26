package dla.pe

import chisel3._
import chisel3.util._
// TODO: add reset signal for every module
class ProcessingElement extends Module with PESizeConfig {
  val io = IO(new Bundle{
    val dataStream = new DataStreamIO
    val topCtrl = new PETopToCtrlIO
  })
  val peCtrl = new ProcessingElementControl
  val pePad = new ProcessingElementPad(false)
  if (fifoEn) {
    pePad.io.dataStream.iactIOs.addrIOs.writeInDataIO <> Queue(io.dataStream.iactIOs.addrIOs.writeInDataIO, fifoSize, flow = true)
    pePad.io.dataStream.iactIOs.dataIOs.writeInDataIO <> Queue(io.dataStream.iactIOs.dataIOs.writeInDataIO, fifoSize, flow = true)
    pePad.io.dataStream.weightIOs.addrIOs.writeInDataIO <> Queue(io.dataStream.weightIOs.addrIOs.writeInDataIO, fifoSize, flow = true)
    pePad.io.dataStream.weightIOs.dataIOs.writeInDataIO <> Queue(io.dataStream.weightIOs.dataIOs.writeInDataIO, fifoSize, flow = true)
  } else {
    pePad.io.dataStream.iactIOs <> io.dataStream.iactIOs
    pePad.io.dataStream.weightIOs <> io.dataStream.weightIOs
 }
  peCtrl.io.ctrlPad <> pePad.io.padCtrl
  peCtrl.io.ctrlTop <> io.topCtrl
  pePad.io.dataStream.ipsIO <> Queue(io.dataStream.ipsIO, fifoSize, flow = true)
  io.dataStream.opsIO <> Queue(pePad.io.dataStream.opsIO, fifoSize, flow = true)
}

class ProcessingElementControl extends Module with MCRENFConfig {
  val io = IO(new Bundle{
    val ctrlPad = new PECtrlToPadIO
    val ctrlTop: PETopToCtrlIO = Flipped(new PETopToCtrlIO)
  })
  io.ctrlTop.writeFinish := io.ctrlPad.writeFinish
  io.ctrlTop.calFinish := io.ctrlPad.calFinish
  // some config of RS+
  // logic of PE MAC
  // state machine, control the process of MAC
  // psIdle: wait for signal
  // psLoad: load input activations, weights, partial sums outside and read out output partial sum
  // psCal: do MAC computations
  val psIdle :: psLoad :: psCal :: Nil = Enum(3)
  val stateMac: UInt = RegInit(psIdle) // the state of the mac process
  io.ctrlTop.pSumEnqOrProduct.ready := Mux(stateMac === psCal, io.ctrlPad.pSumEnqOrProduct.ready, false.B)
  io.ctrlPad.pSumEnqOrProduct.valid := Mux(stateMac === psCal, io.ctrlTop.pSumEnqOrProduct.valid, false.B)
  io.ctrlPad.doLoadEn := stateMac === psLoad
  io.ctrlPad.doMACEn := stateMac === psCal
  switch (stateMac) {
    is (psIdle) {
      when (io.ctrlTop.doLoadEn) { // when there is any mac leaving
        stateMac := psLoad
      }
    }
    is (psLoad) {
      when (io.ctrlPad.writeFinish) { //after the pad receives the data
        stateMac := psCal
      }
    }
    is (psCal) {
      when (io.ctrlPad.calFinish) {
        stateMac := psIdle
      }
    }
  }
}

class ProcessingElementPad(debug: Boolean) extends Module with MCRENFConfig with SPadSizeConfig {
  val io = IO(new Bundle{
    val padCtrl: PECtrlToPadIO = Flipped(new PECtrlToPadIO)
    val dataStream = new DataStreamIO
    val debugIO = new PESPadDebugIO
  })
  private def iactIdx(m: Vec[UInt]): UInt = m(1) + MCRENF(1).U*(m(2) + MCRENF(2).U*(m(3) + MCRENF(3).U*(m(4) + MCRENF(4).U*m(5))))
  // iactapIdx = c0 + r0*C0 + e0*R0*C0 + n0*E0*R0*C0 + f0*N0*E0*R0*C0 FIXME: check the input feature map
  private def weightIdx(m: Vec[UInt]): UInt = m(0) + MCRENF.head.U*(m(1) + MCRENF(1).U*m(2))
  // weightIdx = m0 + c0*M0 + r0*C0*M0
  private def pSumIdx(m: Vec[UInt]): UInt = m(0) + MCRENF.head.U*(m(3) + MCRENF(3).U*(m(4) + MCRENF(4).U*m(5)))
  // pSumIdx = m0 + e0*M0 + n0*E0*M0 + f0*N0*E0*M0
  private def nextSPadIactAddr(): Unit = {
    sPad := padIactAddr
    iactAddrSPadReadEnReg := true.B
    iactDataSPadReadEnReg := false.B
    weightAddrSPadReadEnReg := false.B
    weightDataSPadReadEnReg := false.B
    weightDataSPadFirstRead := true.B // if need read a new column of input activation matrix, then true
  }
  private def nextSPadIactData(): Unit = {
    sPad := padIactData
    iactAddrSPadReadEnReg := false.B
    iactDataSPadReadEnReg := true.B
    weightAddrSPadReadEnReg := false.B
    weightDataSPadReadEnReg := false.B
    weightDataSPadFirstRead := true.B // if need to read a new data, also a new weight matrix column
  }
  private def nextSPadWeightAddr(): Unit = {
    sPad := padWeightAddr
    iactAddrSPadReadEnReg := false.B
    iactDataSPadReadEnReg := false.B
    weightAddrSPadReadEnReg := true.B
    weightDataSPadReadEnReg := false.B
  }
  private def nextSPadWeightData(): Unit = {
    sPad := padWeightData1
    iactAddrSPadReadEnReg := false.B
    iactDataSPadReadEnReg := false.B
    weightAddrSPadReadEnReg := false.B
    weightDataSPadReadEnReg := true.B
  }
  private def readOff(): Unit = {
    iactAddrSPadReadEnReg := false.B
    iactDataSPadReadEnReg := false.B
    weightAddrSPadReadEnReg := false.B
    weightDataSPadReadEnReg := false.B
  }
  val psDataSPad: Vec[UInt] = RegInit(VecInit(Seq.fill(pSumDataSPadSize)(0.U(psDataWidth.W)))) // reg, partial sum scratch pad
  val iactAddrSPad: SPadAddrModule = Module(new SPadAddrModule(commonLenWidth, iactAddrSPadSize, iactAddrWidth))
  val iactDataSPad: SPadDataModule = Module(new SPadDataModule(commonLenWidth, iactDataSPadSize, iactDataWidth, false))
  val weightAddrSPad: SPadAddrModule = Module(new WeightSPadAddrModule(commonLenWidth, weightAddrSPadSize, weightAddrWidth))
  val weightDataSPad: SPadDataModule = Module(new SPadDataModule(weightDataLenWidth, weightDataSPadSize, weightDataWidth, true))
  // IactSPad
  val iactAddrIndexWire: UInt = Wire(UInt(commonLenWidth.W))
  val iactAddrDataWire: UInt = Wire(UInt(iactAddrWidth.W))
  val iactDataIndexWire: UInt = Wire(UInt(commonLenWidth.W)) // use for address vector readEn
  val iactAddrSPadReadEnReg: Bool = RegInit(false.B)
  val iactDataSPadReadEnReg: Bool = RegInit(false.B)
  val iactAddrSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of iact address SPad
  val iactDataSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of iact data SPad
  val iactMatrixColumnReg: UInt = RegInit(0.U(commonLenWidth.W))
  val iactZeroColumnNumber: UInt = RegInit(0.U(commonLenWidth.W)) // use for get the right column number
  val iactDataSPadFirstReadReg: Bool = RegInit(true.B)
  val iactMatrixRowWire: UInt = Wire(UInt(cscCountWidth.W))
  val iactMatrixDataWire: UInt = Wire(UInt(cscDataWidth.W))
  // WeightSPad
  val weightAddrIndexWire: UInt = Wire(UInt(commonLenWidth.W))
  val weightAddrDataWire: UInt = Wire(UInt(weightAddrWidth.W))
  val weightDataIndexWire: UInt = Wire(UInt(commonLenWidth.W)) // use for address vector readEn
  val weightAddrSPadReadEnReg: Bool = RegInit(false.B)
  val weightDataSPadReadEnReg: Bool = RegInit(false.B)
  val weightAddrSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of weight address SPad
  val weightDataSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of weight data SPad
  val weightMatrixDataReg: UInt = Wire(UInt(cscDataWidth.W))
  //val weightMatrixDataReg: UInt = RegInit(0.U(cscDataWidth.W))
  val weightDataSPadFirstRead: Bool = RegInit(true.B)
  val weightDataIdxMuxWire: Bool = Wire(Bool()) // false, then means need to read the first column of weight Matrix
  val weightAddrSPadReadIdxWire: UInt = Wire(UInt(cscCountWidth.W))
  val weightDataIdxEnWire: Bool = Wire(Bool()) // true then assign a special one for data index
  val weightAddrIdxEnWire: Bool = Wire(Bool()) // true then assign a special one for data index
  val weightMatrixReadFirstColumn: Bool = Wire(Bool())
  // pSumSPad
  val productReg: UInt = RegInit(0.U(psDataWidth.W))
  val pSumSPadLoadReg: UInt = RegInit(0.U(psDataWidth.W))
  val pSumResultWire: UInt = Wire(UInt(psDataWidth.W))
  // State Machine
  // padIdle: the pad is idle, and if received valid signal from control, then read and send data to mac
  // padIactAddr: read the input activation address
  // padIactData: read the input activation data
  // padWeightAddr: read the weight address
  // padWeightData1: read the weight data
  // padWeightData2: wait one cycle as SRAM
  // padMpy: wait for mac computation
  // padWriteBack: write the partial sum back
  val padIdle :: padIactAddr :: padIactData :: padWeightAddr :: padWeightData1 :: padWeightData2 :: padMpy :: padWriteBack :: Nil = Enum(8)
  val sPad: UInt = RegInit(padIdle)
  val padEqIA: Bool = Wire(Bool())
  val padEqID: Bool = Wire(Bool())
  val padEqWA: Bool = Wire(Bool())
  val padEqMpy: Bool = Wire(Bool())
  val padEqWB: Bool = Wire(Bool())
  padEqIA := sPad === padIactAddr
  padEqMpy := sPad === padMpy
  padEqWB := sPad === padWriteBack
  padEqWA := sPad === padWeightAddr
  padEqID := sPad === padIactData
  weightMatrixReadFirstColumn := iactMatrixRowWire === 0.U
  val weightMatrixRowReg: UInt = Wire(UInt(cscCountWidth.W))
  //val weightMatrixRowReg: UInt = RegEnable(0.U(cscCountWidth.W), padEqWA)
  // Connections
  Seq(iactAddrSPad, iactDataSPad, weightAddrSPad, weightDataSPad).map(_.io.commonIO.writeEn := io.padCtrl.doLoadEn)
  // Input activation Address Scratch Pad
  iactAddrSPad.io.commonIO.dataLenFinIO <> io.dataStream.iactIOs.addrIOs
  iactAddrIndexWire := iactAddrSPad.io.commonIO.columnNum
  iactAddrDataWire := iactAddrSPad.io.commonIO.readOutData
  iactAddrSPad.io.commonIO.readEn := iactAddrSPadReadEnReg
  iactAddrSPad.io.addrIO.indexInc := iactAddrSPadIdxIncWire
  iactAddrSPad.io.addrIO.readInIdx := DontCare
  iactAddrSPad.io.addrIO.readInIdxEn := DontCare
  iactAddrSPad.io.dataIO := DontCare
  // Input activation Data Scratch Pad
  iactDataSPad.io.commonIO.dataLenFinIO <> io.dataStream.iactIOs.dataIOs
  iactDataIndexWire := iactDataSPad.io.commonIO.columnNum
  val iactDataCountVec: Seq[Bool] = iactDataSPad.io.commonIO.readOutData.asBools
  iactMatrixDataWire := Cat(iactDataCountVec.reverse.take(cscDataWidth)).asUInt // TODO: figure out why it need reverse
  iactMatrixRowWire := Cat(iactDataCountVec.reverse.takeRight(cscCountWidth)).asUInt
  iactDataSPad.io.commonIO.readEn := iactDataSPadReadEnReg
  iactDataSPad.io.dataIO.indexInc := iactDataSPadIdxIncWire
  iactDataSPad.io.dataIO.readInIdx := iactAddrDataWire
  iactDataSPad.io.dataIO.readInIdxEn := DontCare
  iactDataSPad.io.addrIO := DontCare
  // Weight Address Scratch Pad
  weightAddrSPad.io.commonIO.dataLenFinIO <> io.dataStream.weightIOs.addrIOs
  weightAddrIndexWire := weightAddrSPad.io.commonIO.columnNum
  weightAddrDataWire := weightAddrSPad.io.commonIO.readOutData
  weightAddrSPad.io.commonIO.readEn := weightAddrSPadReadEnReg
  weightAddrSPad.io.addrIO.readInIdx := weightAddrSPadReadIdxWire // the weight address SPad's columns corresponds to
                                                          // the iact address SPad's rows, and it takes one clock cycle
                                                          // for the reg inside SPad to change the index it need
  weightAddrSPad.io.addrIO.indexInc := weightAddrSPadIdxIncWire
  weightAddrSPad.io.addrIO.readInIdxEn := weightAddrIdxEnWire
  weightAddrSPad.io.dataIO := DontCare
  // Weight Data Scratch Pad
  weightDataSPad.io.commonIO.dataLenFinIO <> io.dataStream.weightIOs.dataIOs
  weightDataIndexWire := weightDataSPad.io.commonIO.columnNum
  val weightDataCountVec: Seq[Bool] = weightDataSPad.io.commonIO.readOutData.asBools
  weightMatrixDataReg := Cat(weightDataCountVec.reverse.take(cscDataWidth)).asUInt
  weightMatrixRowReg := Cat(weightDataCountVec.reverse.takeRight(cscCountWidth)).asUInt
  weightDataSPad.io.commonIO.readEn := iactDataSPadReadEnReg
  weightDataSPad.io.dataIO.readInIdx := Mux(weightMatrixReadFirstColumn, 0.U, weightAddrDataWire)
  weightDataSPad.io.dataIO.indexInc := weightDataSPadIdxIncWire
  weightDataSPad.io.dataIO.readInIdxEn := weightDataIdxEnWire
  weightDataSPad.io.addrIO := DontCare
  // Partial Sum Scratch Pad
  io.dataStream.ipsIO.ready := padEqMpy && io.padCtrl.pSumEnqOrProduct.bits
  val psPadReadIdxCounter: Counter = Counter(M0*E*N0*F0 + 1)
  when (io.padCtrl.doLoadEn && io.dataStream.opsIO.ready) {
    io.dataStream.opsIO.bits := psDataSPad(psPadReadIdxCounter.value)
    io.dataStream.opsIO.valid := true.B
    psPadReadIdxCounter.inc()
  } .otherwise {
    io.dataStream.opsIO.valid := false.B
    io.dataStream.opsIO.bits := DontCare
  }
  // SPadToCtrl
  io.padCtrl.pSumEnqOrProduct.ready := padEqMpy
  io.padCtrl.writeFinish := Seq(iactAddrSPad, iactDataSPad, weightAddrSPad, weightDataSPad).map(_.io.commonIO.dataLenFinIO.writeFin).reduce(_ && _) // when all Scratch Pad write finished
  pSumResultWire := Mux(padEqWB, pSumSPadLoadReg + productReg, 0.U)
  /*
  val mcrenfReg: Vec[UInt] = RegInit(VecInit(Seq.fill(6)(0.U(log2Ceil(MCRENF.max).W))))
  val when_carry: IndexedSeq[Bool] = mcrenfReg.zip(MCRENF.map(x=> x - 1)).map{ case (x,y) => x === y.U}
  // when_carry stores the information of whether m0 === M0.U, et al.
  */
  val pSumResultIdxReg: UInt = RegInit(0.U(calDataWidth.W)) // store the index for write back
  // several signals which can help to indicate the process
  val mightIactZeroColumnWire: Bool = Wire(Bool())
  val iactSPadZeroColumnReg: Bool = RegInit(false.B) // true, it is a zero column, then need read again
  val mightIactIdxIncWire: Bool = Wire(Bool())
  val mightWeightZeroColumnWire: Bool = Wire(Bool())
  val mightWeightIdxIncWire: Bool = Wire(Bool())
  val mightIactReadFinish: Bool = Wire(Bool())
  val mightWeightReadFinish: Bool = Wire(Bool())
  val psDataSpadIdxWire: UInt = Wire(UInt(log2Ceil(pSumDataSPadSize).W))
  mightIactZeroColumnWire := iactAddrDataWire === iactZeroColumnCode.U
  mightWeightZeroColumnWire := weightAddrDataWire === weightZeroColumnCode.U
  mightIactIdxIncWire := iactAddrDataWire === (iactDataIndexWire + 1.U)
  mightWeightIdxIncWire := weightAddrDataWire === (weightDataIndexWire + 1.U)
  mightIactReadFinish := (iactDataIndexWire + 1.U) === io.dataStream.iactIOs.dataIOs.streamLen
  mightWeightReadFinish := (weightDataIndexWire + 1.U) === io.dataStream.weightIOs.dataIOs.streamLen
  iactAddrSPadIdxIncWire := (padEqIA && mightIactZeroColumnWire) || (((padEqWA && mightWeightZeroColumnWire) || (padEqWB && mightWeightIdxIncWire)) && mightIactIdxIncWire)
  weightAddrSPadIdxIncWire := (padEqMpy || sPad === padWeightData1) && mightWeightZeroColumnWire // FIXME: should add a state
  iactDataSPadIdxIncWire := (padEqIA && !mightIactZeroColumnWire && !iactDataSPadFirstReadReg) || (((padEqWA && mightWeightZeroColumnWire) || (padEqWB && mightWeightIdxIncWire)) && !mightIactIdxIncWire)// if first read, then keep the read index of zero
  weightDataSPadIdxIncWire := (padEqWA && !mightWeightZeroColumnWire && !weightDataSPadFirstRead) || (padEqWB && !mightWeightIdxIncWire) // when first read, ask Weight Address Scratch Pad for data index
  weightAddrIdxEnWire := (padEqID || padEqWA) && weightDataSPadFirstRead // read the start and end index from address SPad
  weightDataIdxMuxWire := padEqID && weightDataSPadFirstRead && !weightMatrixReadFirstColumn // then it can read the start index in weightDataSPad, the end index of that will be read otherwise
  weightAddrSPadReadIdxWire := Mux(weightDataIdxMuxWire, iactMatrixRowWire - 1.U, iactMatrixRowWire)
  weightDataIdxEnWire := padEqWA && weightDataSPadFirstRead && !mightWeightZeroColumnWire
  io.padCtrl.calFinish := padEqWB && mightIactReadFinish && mightWeightReadFinish
  psDataSpadIdxWire := weightMatrixRowReg + iactMatrixColumnReg*(M0.U)
  switch (sPad) {
    is (padIdle) {
      when(io.padCtrl.doMACEn) {
        nextSPadIactAddr()
        iactDataSPadFirstReadReg := true.B
      }
    }
    is (padIactAddr) {
      when (mightIactZeroColumnWire) { // then it is a zero column
        nextSPadIactAddr()
        iactSPadZeroColumnReg := true.B
        iactZeroColumnNumber := iactZeroColumnNumber + 1.U
      } .otherwise {
        nextSPadIactData()
      }
    }
    is (padIactData) {
      nextSPadWeightAddr()
      iactDataSPadFirstReadReg := false.B
    }
    is (padWeightAddr) {
      when (mightWeightZeroColumnWire) { // need to get next iact
        when (mightIactIdxIncWire) { // if have read all elements in current iact Matrix column
          nextSPadIactAddr()
          when (iactSPadZeroColumnReg) {
            iactSPadZeroColumnReg := false.B
            iactMatrixColumnReg := iactMatrixColumnReg + 1.U + iactZeroColumnNumber
            iactZeroColumnNumber := 0.U
          } .otherwise {
            iactMatrixColumnReg := iactMatrixColumnReg + 1.U
          }
        } .otherwise { // still some elements in current iact Matrix column
          nextSPadIactData()
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
      psPadReadIdxCounter.value := psDataSpadIdxWire
      readOff()
    }
    is (padMpy) {
      when (io.padCtrl.pSumEnqOrProduct.bits) {
        when (io.dataStream.ipsIO.valid) {
          sPad := padWriteBack
          productReg := io.dataStream.ipsIO.bits
          io.dataStream.ipsIO.ready := true.B
          pSumSPadLoadReg := psDataSPad(psPadReadIdxCounter.value)
        }
      } .otherwise {
        sPad := padWriteBack
        productReg :=  weightMatrixDataReg * iactMatrixDataWire
        pSumSPadLoadReg := psDataSPad(psPadReadIdxCounter.value)
      }
    }
    is (padWriteBack) {
      psDataSPad(psDataSpadIdxWire) := pSumResultWire //update the partial sum
      when (mightIactReadFinish && mightWeightReadFinish) {
        sPad := padIdle
        iactMatrixColumnReg := 0.U
        psPadReadIdxCounter.value := 0.U
      } .otherwise { // then haven't done all the MAC operations
        when (mightWeightIdxIncWire) { // finished read current weight data Matrix column
          when (mightIactIdxIncWire) { // finished read current iact data Matrix column
            nextSPadIactAddr()
            when (iactSPadZeroColumnReg) {
              iactSPadZeroColumnReg := false.B
              iactMatrixColumnReg := iactMatrixColumnReg + 1.U + iactZeroColumnNumber
              iactZeroColumnNumber := 0.U
            } .otherwise {
              iactMatrixColumnReg := iactMatrixColumnReg + 1.U
            }
          } .otherwise {
            nextSPadIactData()
          }
        } .otherwise {
          nextSPadWeightData()
          weightDataSPadFirstRead := false.B // as it has been read current weight matrix column
        }
      }
    }
  }
  if (debug) {
    io.debugIO.iactMatrixColumn := iactMatrixColumnReg
    io.debugIO.iactMatrixData := iactMatrixDataWire
    io.debugIO.iactMatrixRow := iactMatrixRowWire
    io.debugIO.iactAddrIdx := iactAddrIndexWire
    io.debugIO.iactAddrInc := iactAddrSPadIdxIncWire
    io.debugIO.iactDataInc := iactDataSPadIdxIncWire
    io.debugIO.weightMatrixData := weightMatrixDataReg
    io.debugIO.weightMatrixRow := weightMatrixRowReg
    io.debugIO.weightAddrSPadReadOut := weightAddrDataWire
    io.debugIO.productResult := productReg
    io.debugIO.pSumResult := pSumResultWire
    io.debugIO.pSumLoad := pSumSPadLoadReg
    io.debugIO.weightAddrInIdx := weightAddrSPadReadIdxWire
    io.debugIO.sPadState := sPad
  }else {
    io.debugIO := DontCare
  }
}

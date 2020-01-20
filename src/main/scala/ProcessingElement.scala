package dla.pe

import chisel3._
import chisel3.util._
// TODO: add reset signal for every module
class ProcessingElement extends Module with PESizeConfig {
  val io = IO(new Bundle{
    val dataStream = new DataStreamIO
  })
  //val iactDataFIFO = Module(new Queue(io.dataStream.iactDataIO, fifoSize, flow = true)) //FIFO for input feature map
  //
  //val iactAddrFIFO = Module(new Queue(io.dataStream.iactAddrIO, fifoSize, flow = true)) //FIFO for input feature map
  //val weightDataFIFO = Module(new Queue(io.dataStream.weightDataIO, fifoSize, flow = true)) //FIFO for weight
  // c0 === C0.U, weightFIFO.deq.valid := true.B
  //val weightAddrFIFO = Module(new Queue(io.dataStream.weightAddrIO, fifoSize, flow = true)) //FIFO for weight
  val ipsFIFO: Queue[DecoupledIO[UInt]] = Module(new Queue(io.dataStream.ipsIO, fifoSize, flow = true)) //FIFO for input partial sum
  // r0 === R.U, ipsFIFO.deq.valid := true.B
  val peCtrl = new ProcessingElementControl
  val pePad = new ProcessingElementPad
  peCtrl.io.ctrlPad <> pePad.io.padCtrl
  pePad.io.dataStream.iactIOs <> io.dataStream.iactIOs
  pePad.io.dataStream.weightIOs <> io.dataStream.weightIOs
  pePad.io.dataStream.ipsIO <> ipsFIFO.io.deq
  val opsFIFO: Queue[DecoupledIO[UInt]] = Module(new Queue(pePad.io.dataStream.opsIO, fifoSize, flow = true)) //FIFO for output partial sum
  //
  io.dataStream.opsIO <> ipsFIFO.io.deq
}

class ProcessingElementControl extends Module with MCRENFConfig {
  val io = IO(new Bundle{
    val ctrlPad = new PECtrlToPadIO
  })
  // some config of RS+

  // logic of PE MAC
  // state machine, the for-loop of m0
  // psIdle: if not all the MAC operations are done, then load
  // ps_load: load input activations, weights, partial sums, then do the MAC
  // ps_done: finish one mac, then jump to idle or load state
  val psIdle :: ps_load :: ps_done :: Nil = Enum(3)
  val s_per_mac: UInt = RegInit(psIdle) // the state of the mac process
  val all_mac_is_done: Bool = RegInit(false.B) //true when all the mac has been done, then pe will keep in psIdle
  // TODO: check the MCRENF.map
  switch (s_per_mac) {
    is (psIdle) {
      when (!all_mac_is_done) { // when there is any mac leaving
        s_per_mac := ps_load
      }
      io.ctrlPad.pSumEnqOrProduct.valid := false.B
    }
    is (ps_load) {
      io.ctrlPad.pSumEnqOrProduct.valid := true.B
      when (io.ctrlPad.pSumEnqOrProduct.ready) { //after the pad receives the data
        s_per_mac := ps_done
        io.ctrlPad.pSumEnqOrProduct.valid := false.B
      }
    }
    is (ps_done) {/*
      mcrenfReg(0) := mcrenfReg(0) + 1.U // no matter whether m0 equals to (M0 - 1).U, we add one, then check it
      // then check whether mcrenf need carry one TODO: check it
      for (i <- 1 until 6) { // from m0 to n0
        when(when_carry.take(i).reduce(_ && _)) {
          if (!isPow2(MCRENF(i-1))) { // if equals to pow2, then don't need reset just let it carries.
            mcrenfReg(i - 1) := 0.U
          }
          mcrenfReg(i) := mcrenfReg(i) + 1.U
        }
      }
      when(when_carry.reduce(_ && _)) { // f0
        if (!isPow2(MCRENF(5))) {
          mcrenfReg(5) := 0.U
        }
        all_mac_is_done := true.B // then all the macs have been done
        // FIXME: need a signal to let the PE start
      }*/

      when (all_mac_is_done) {
        s_per_mac := psIdle
      }.otherwise{
        s_per_mac := ps_load
      }
    }
  }
}

class ProcessingElementPad extends Module with MCRENFConfig with SPadSizeConfig {
  val io = IO(new Bundle{
    val padCtrl: PECtrlToPadIO = Flipped(new PECtrlToPadIO)
    val dataStream = new DataStreamIO
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
  }
  private def nextSPadIactData(): Unit = {
    sPad := padIactData
    iactAddrSPadReadEnReg := false.B
    iactDataSPadReadEnReg := true.B
    weightAddrSPadReadEnReg := false.B
    weightDataSPadReadEnReg := false.B
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
  val psDataSPad: Vec[UInt] = RegInit(VecInit(Seq.fill(pSumDataSPadSize)(0.U(psDataWidth.W)))) // reg, partial sum scratch pad
  val iactAddrSPad: SPadAddrModule = Module(new SPadAddrModule(commonLenWidth, iactAddrSPadSize, iactAddrWidth))
  val iactDataSPad: SPadDataModule = Module(new SPadDataModule(commonLenWidth, iactDataSPadSize, iactDataWidth, false))
  val weightAddrSPad: SPadAddrModule = Module(new SPadAddrModule(commonLenWidth, weightAddrSPadSize, weightAddrWidth))
  val weightDataSPad: SPadDataModule = Module(new SPadDataModule(weightDataLenWidth, weightDataSPadSize, weightDataWidth, true))
  iactAddrSPad.io.commonIO.dataLenFinIO <> io.dataStream.iactIOs.addrIOs
  iactDataSPad.io.commonIO.dataLenFinIO <> io.dataStream.iactIOs.dataIOs
  weightAddrSPad.io.commonIO.dataLenFinIO <> io.dataStream.weightIOs.addrIOs
  weightDataSPad.io.commonIO.dataLenFinIO <> io.dataStream.weightIOs.dataIOs
  // close the unused IO
  iactAddrSPad.io.dataIO.readInIdx := DontCare
  iactAddrSPad.io.addrIO.readInIdx := DontCare
  iactDataSPad.io.addrIO.readInIdx := DontCare
  iactDataSPad.io.dataIO.readInIdx := DontCare
  weightDataSPad.io.addrIO.readInIdx := DontCare
  weightAddrSPad.io.dataIO.readInIdx := DontCare
  // IactSPad
  val iactAddrIndexWire: UInt = Wire(UInt(commonLenWidth.W))
  val iactAddrDataWire: UInt = Wire(UInt(iactAddrWidth.W))
  val iactDataIndexWire: UInt = Wire(UInt(commonLenWidth.W)) // use for address vector readEn
  val iactSPadZeroColumnReg: Bool = RegInit(false.B) // true, it is a zero column, then need read again
  val iactAddrSPadReadEnReg: Bool = RegInit(false.B)
  val iactDataSPadReadEnReg: Bool = RegInit(false.B)
  val iactAddrSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of address SPad
  val iactDataSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of data SPad
  val iactMatrixColumnReg: UInt = RegInit(0.U(commonLenWidth.W))
  val iactZeroColumnNumber: UInt = RegInit(0.U(commonLenWidth.W)) // use for get the right column number
  val iactDataSPadFirstRead: Bool = RegInit(true.B)
  val iactMatrixRowReg: UInt = RegInit(0.U(cscCountWidth.W))
  val iactMatrixDataReg: UInt = RegInit(0.U(cscDataWidth.W))
  // WeightSPad
  val weightAddrIndexWire: UInt = Wire(UInt(commonLenWidth.W))
  val weightAddrDataWire: UInt = Wire(UInt(iactAddrWidth.W))
  val weightDataIndexWire: UInt = Wire(UInt(commonLenWidth.W)) // use for address vector readEn
  val weightSPadZeroColumnReg: Bool = RegInit(false.B) // true, it is a zero column, then read the next column of iact
  val weightAddrSPadReadEnReg: Bool = RegInit(false.B)
  val weightDataSPadReadEnReg: Bool = RegInit(false.B)
  val weightAddrSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of address SPad
  val weightDataSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of data SPad
  val weightMatrixRowReg: UInt = RegInit(0.U(cscCountWidth.W))
  val weightMatrixDataReg: UInt = RegInit(0.U(cscDataWidth.W))
  val productReg: UInt = RegInit(0.U(psDataWidth.W))
  val pSumSPadLoadReg: UInt = RegInit(0.U(psDataWidth.W))
  val pSumResultWire: UInt = Wire(UInt(psDataWidth.W))
  iactAddrSPad.io.addrIO.indexInc := iactAddrSPadIdxIncWire
  weightAddrSPad.io.addrIO.indexInc := weightAddrSPadIdxIncWire
  iactDataSPad.io.dataIO.indexInc := iactDataSPadIdxIncWire
  weightDataSPad.io.dataIO.indexInc := weightDataSPadIdxIncWire
  // IactAddrSPad
  iactAddrIndexWire := iactAddrSPad.io.commonIO.columnNum
  iactAddrDataWire := iactAddrSPad.io.commonIO.readOutData
  iactAddrSPad.io.commonIO.readEn := iactAddrSPadReadEnReg
  iactAddrSPad.io.dataIO := DontCare
  iactAddrSPad.io.addrIO.readInIdx := DontCare
  // IactDataSPad
  iactDataIndexWire := iactDataSPad.io.commonIO.columnNum
  val iactDataCountVec: Seq[Bool] = iactDataSPad.io.commonIO.readOutData.asBools
  iactMatrixDataReg := Cat(iactDataCountVec.reverse.take(8)).asUInt // TODO: figure out why it need reverse
  iactMatrixRowReg := Cat(iactDataCountVec.reverse.takeRight(4)).asUInt
  iactDataSPad.io.commonIO.readEn := iactDataSPadReadEnReg

  // WeightAddrSPad
  weightAddrIndexWire := weightAddrSPad.io.commonIO.columnNum
  weightAddrDataWire := weightAddrSPad.io.commonIO.readOutData
  weightAddrSPad.io.commonIO.readEn := weightAddrSPadReadEnReg
  weightAddrSPad.io.dataIO := DontCare
  weightAddrSPad.io.addrIO.readInIdx := iactMatrixRowReg // the weight address SPad's columns corresponds to
                                                         // the iact address SPad's rows
  // WeightDataSPad
  weightDataIndexWire := weightDataSPad.io.commonIO.columnNum
  val weightDataCountVec: Seq[Bool] = weightDataSPad.io.commonIO.readOutData.asBools
  weightMatrixDataReg := Cat(weightDataCountVec.reverse.take(8)).asUInt // TODO: change the bit-width
  weightMatrixRowReg := Cat(weightDataCountVec.reverse.takeRight(4)).asUInt
  weightDataSPad.io.commonIO.readEn := iactDataSPadReadEnReg

  val mcrenfReg: Vec[UInt] = RegInit(VecInit(Seq.fill(6)(0.U(log2Ceil(MCRENF.max).W))))
  val when_carry: IndexedSeq[Bool] = mcrenfReg.zip(MCRENF.map(x=> x - 1)).map{ case (x,y) => x === y.U}
  // when_carry stores the information of whether m0 === M0.U, et al.

  val pSumResultIdxReg: UInt = RegInit(0.U(calDataWidth.W)) // store the index for write back
  // padIdle: the pad is idle, and if received valid signal from control, then read and send data to mac
  // padIactAddr: read the input activation address
  // padIactData: read the input activation data
  // padWeightAddr: read the weight address
  // padWeightData1: read the weight data
  // padWeightData2: wait one cycle as SRAM
  // pad_mpy: wait for mac computation
  // pad_wb: write the partial sum back
  val padIdle :: padIactAddr :: padIactData :: padWeightAddr :: padWeightData1 :: padWeightData2 :: pad_mpy :: pad_wb :: Nil = Enum(8)
  val sPad: UInt = RegInit(padIdle)
  iactAddrSPadIdxIncWire := ((sPad === padIactData) && (iactAddrDataWire === (iactDataIndexWire + 1.U))) || ((sPad === padIactAddr) && (iactAddrDataWire === zeroColumnCode.U))
  weightAddrSPadIdxIncWire := ((sPad === padWeightData1) && (weightAddrDataWire === (weightDataIndexWire + 1.U))) || ((sPad === padIactAddr) && (iactAddrDataWire === zeroColumnCode.U))
  iactDataSPadIdxIncWire := ((sPad === padIactAddr) && !iactSPadZeroColumnReg && !iactDataSPadFirstRead) || ((sPad === padIactData) && !iactAddrSPadIdxIncWire)// if first read, then keep the read index of zero
  weightDataSPadIdxIncWire := ((sPad === padIactAddr) && !iactSPadZeroColumnReg && !iactDataSPadFirstRead) || ((sPad === padIactData) && !iactAddrSPadIdxIncWire)// if first read, then keep the read index of zero
  // the_index_m0 = m0 + count_m0
  // addr_m0_index*M0
  switch (sPad) {
    is (padIdle) {
      when(io.padCtrl.doMACEn) {
        nextSPadIactAddr()
        iactDataSPadFirstRead := true.B
      }
    }
    is (padIactAddr) {
      when (iactAddrDataWire === zeroColumnCode.U) { // then it is a zero column
        nextSPadIactAddr()
        iactSPadZeroColumnReg := true.B
      } .otherwise {
        nextSPadIactData()
      }
    }
    is (padIactData) {
    }
    is (padWeightAddr) {
      when (weightAddrDataWire === zeroColumnCode.U) { // need to get next iact
        weightSPadZeroColumnReg := true.B
        when (iactAddrSPadIdxIncWire) {
          nextSPadIactAddr()
        } .otherwise {
          nextSPadIactData()
        }
      }
    }
    is (padWeightData1) {
      sPad := padWeightData2
      //weightDataSPadReadEnReg := false.B
    }
    is (padWeightData2) {
      weightDataSPadReadEnReg := false.B
      sPad := pad_mpy
    }
    is (pad_mpy) {
      sPad := pad_wb
      pSumSPadLoadReg := psDataSPad(weightMatrixRowReg)
      productReg := Mux(io.padCtrl.pSumEnqOrProduct.bits, io.dataStream.ipsIO.bits, weightMatrixDataReg * iactMatrixDataReg)
    }
    is (pad_wb) {
      pSumResultWire := pSumSPadLoadReg + productReg
      // FIXME: add ready valid signal for ips FIFO
      psDataSPad(weightMatrixRowReg) := pSumResultWire //update the partial sum
      when ((iactDataIndexWire === io.dataStream.iactIOs.dataIOs.streamLen - 1.U) && (weightDataIndexWire === io.dataStream.weightIOs.dataIOs.streamLen - 1.U)) {
        sPad := padIdle
      }
      when (iactAddrSPadIdxIncWire) {
        nextSPadIactAddr()
      }
    }
  }
}

class SimplyCombineAddrDataSPad extends Module with SPadSizeConfig{
  val io = IO(new Bundle{
    val iactIOs = new DataAddrStreanIO(iactDataWidth, iactAddrWidth, commonLenWidth, commonLenWidth)
    val iactAddrWriteIdx: UInt = Output(UInt(commonLenWidth.W)) // use for test
    val iactDataReq: Bool = Input(Bool()) // control to read data vector
    // we are supposed to see address SPad and data SPad together
    val iactMatrixColumn: UInt = Output(UInt(commonLenWidth.W))
    val iactMatrixRow: UInt = Output(UInt(cscCountWidth.W))
    val iactMatrixData: UInt = Output(UInt(cscDataWidth.W))
    val iactMatrixDataBin: UInt = Output(UInt(iactDataWidth.W))
    val iactAddrReadEn: Bool = Output(Bool())
    val iactAddrReadData: UInt = Output(UInt(iactAddrWidth.W))
    // val iactAddrReadIndex: UInt = Output(UInt(commonLenWidth.W)) = iactMatrixColumn
    val iactDataReadIndex: UInt = Output(UInt(commonLenWidth.W))
    val iactDataWriteIdx: UInt = Output(UInt(commonLenWidth.W))
  })
  val iactAddrSPad: SPadAddrModule = Module(new SPadAddrModule(commonLenWidth, iactAddrSPadSize, iactAddrWidth))
  val iactDataSPad: SPadDataModule = Module(new SPadDataModule(commonLenWidth, iactDataSPadSize, iactDataWidth, false))

  val padIdle :: padIactAddr :: padIactData :: Nil = Enum(3)
  val sPad: UInt = RegInit(padIdle)
  val iactAddrIndexWire: UInt = Wire(UInt(commonLenWidth.W))
  val iactAddrDataWire: UInt = Wire(UInt(iactAddrWidth.W))
  val iactDataIndexWire: UInt = Wire(UInt(commonLenWidth.W)) // use for address vector readEn
  val iactSPadZeroColumnReg: Bool = RegInit(false.B) // true, it is a zero column, then need read again
  val iactAddrSPadReadEnReg: Bool = RegInit(false.B)
  val iactDataSPadReadEnReg: Bool = RegInit(false.B)
  val iactAddrSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of address SPad
  val iactDataSPadIdxIncWire: Bool = Wire(Bool()) // true, then increase the read index of data SPad
  val iactMatrixColumnReg: UInt = RegInit(0.U(commonLenWidth.W))
  val iactZeroColumnNumber: UInt = RegInit(0.U(commonLenWidth.W)) // use for get the right column number
  val iactDataSPadFirstRead: Bool = RegInit(true.B)
  iactAddrSPad.io.addrIO.indexInc := iactAddrSPadIdxIncWire
  iactDataSPad.io.dataIO.indexInc := iactDataSPadIdxIncWire
  iactAddrSPadIdxIncWire := ((sPad === padIactData) && (iactAddrDataWire === (iactDataIndexWire + 1.U))) || ((sPad === padIactAddr) && (iactAddrDataWire === zeroColumnCode.U))
  iactDataSPadIdxIncWire := ((sPad === padIactAddr) && !iactSPadZeroColumnReg && !iactDataSPadFirstRead) || ((sPad === padIactData) && !iactAddrSPadIdxIncWire)// if first read, then keep the read index of zero
  iactAddrSPad.io.commonIO.dataLenFinIO <> io.iactIOs.addrIOs // this is different from the real module
  iactDataSPad.io.commonIO.dataLenFinIO <> io.iactIOs.dataIOs
  io.iactAddrWriteIdx := iactAddrSPad.io.commonIO.writeIdx
  io.iactDataWriteIdx := iactDataSPad.io.commonIO.writeIdx
  // AddrSPad
  iactAddrIndexWire := iactAddrSPad.io.commonIO.columnNum
  io.iactMatrixColumn := iactMatrixColumnReg // for debug
  iactAddrDataWire := iactAddrSPad.io.commonIO.readOutData
  io.iactAddrReadData := iactAddrDataWire // for debug
  iactAddrSPad.io.commonIO.readEn := iactAddrSPadReadEnReg
  io.iactAddrReadEn := iactAddrSPadReadEnReg
  iactAddrSPad.io.dataIO := DontCare
  iactAddrSPad.io.addrIO.readInIdx := DontCare
  // DataSPad
  iactDataIndexWire := iactDataSPad.io.commonIO.columnNum
  io.iactDataReadIndex := iactDataIndexWire // for debug
  io.iactMatrixDataBin := iactDataSPad.io.commonIO.readOutData
  val iactDataCountVec: Seq[Bool] = iactDataSPad.io.commonIO.readOutData.asBools
  io.iactMatrixData := Cat(iactDataCountVec.reverse.take(8)).asUInt // TODO: figure out why it need reverse
  io.iactMatrixRow := Cat(iactDataCountVec.reverse.takeRight(4)).asUInt
  iactDataSPad.io.commonIO.readEn := iactDataSPadReadEnReg
  // disable the unused IOs
  iactDataSPad.io.dataIO.readInIdx := DontCare
  iactDataSPad.io.addrIO := DontCare
  // the_index_m0 = m0 + count_m0
  // addr_m0_index*M0
  // SPad read state machine
  switch (sPad) {
    is(padIdle) {
      when(io.iactDataReq) {
        sPad := padIactAddr
        iactAddrSPadReadEnReg := true.B
        iactDataSPadReadEnReg := false.B
        iactDataSPadFirstRead := true.B
      }
    }
    is(padIactAddr) {
      // state transform
      when (iactAddrDataWire === zeroColumnCode.U) {
        sPad := padIactAddr
        iactSPadZeroColumnReg := true.B
        iactAddrSPadReadEnReg := true.B
        iactDataSPadReadEnReg := false.B
        iactZeroColumnNumber := iactZeroColumnNumber + 1.U
      }.otherwise {
        sPad := padIactData
        iactAddrSPadReadEnReg := false.B
        iactDataSPadReadEnReg := true.B
      }
    }
    is(padIactData) {
      iactDataSPadFirstRead := false.B
      when (iactDataIndexWire === io.iactIOs.dataIOs.streamLen - 1.U) {
        sPad := padIdle
        iactDataSPadReadEnReg := false.B
        iactSPadZeroColumnReg := false.B
      }
      when (iactAddrSPadIdxIncWire) {
        sPad := padIactAddr
        iactSPadZeroColumnReg := false.B // only after the last data read and then
        // the reg can be set to false (like the column is 5, 5, 7)
        iactAddrSPadReadEnReg := true.B
        iactDataSPadReadEnReg := false.B
        when (iactSPadZeroColumnReg) {
          iactMatrixColumnReg := iactMatrixColumnReg + 1.U + iactZeroColumnNumber
          // if zero, then add one and the number of continued zero column
          iactZeroColumnNumber := 0.U // then reset it to default zero
        } .otherwise {
          iactMatrixColumnReg := iactMatrixColumnReg + 1.U // if normal column, add one
        }
      } .otherwise {
        sPad := padIactData
        iactAddrSPadReadEnReg := false.B
        iactDataSPadReadEnReg := true.B
      }
    }
  }
}

class SPadAddrModule(val topDataLenWidth: Int, val topPadSize: Int, val topDataWidth: Int)
  extends SPadCommonModule(topDataLenWidth, topPadSize, topDataWidth) with SPadSizeConfig {
  val addrSPad: Vec[UInt] = RegInit(VecInit(Seq.fill(topPadSize)(0.U(topDataWidth.W))))
  // write logic 2
  when (decoupledDataIO.valid) {
    addrSPad(padWriteIndexReg) := decoupledDataIO.bits.data
  }
  // read logic 2
  dataWire := addrSPad(padReadIndexReg)
  io.commonIO.readOutData := dataWire
  readIndexInc := io.addrIO.indexInc
}

class SPadDataModule(val topDataLenWidth: Int, val topPadSize: Int, val topDataWidth: Int, val sramOrReg: Boolean)
  extends SPadCommonModule(topDataLenWidth, topPadSize, topDataWidth) with SPadSizeConfig {
  if (sramOrReg) {
    val dataSPad: SyncReadMem[UInt] = SyncReadMem(topPadSize,UInt(topDataWidth.W))
    // write logic 2
    when (decoupledDataIO.valid) {
      dataSPad.write(padWriteIndexReg, decoupledDataIO.bits.data)
    }
    // read logic 2
    dataWire := dataSPad.read(padReadIndexReg, io.commonIO.readEn)
  }else{
    val dataSPad: Vec[UInt] = RegInit(VecInit(Seq.fill(topPadSize)(0.U(topDataWidth.W))))
    when (decoupledDataIO.valid) {
      dataSPad(padWriteIndexReg) := decoupledDataIO.bits.data
    }
    // read logic 2
    dataWire := dataSPad(padReadIndexReg)
  }
  readIndexInc := io.dataIO.indexInc
  io.commonIO.readOutData := dataWire // TODO: take care, it is the combination of data and count vector
}

class SPadCommonModule(val dataLenWidth: Int, padSize: Int, val dataWidth: Int) extends Module {
  val io = IO(new Bundle{
    val commonIO = new SPadCommonIO(dataLenWidth, dataWidth, padSize)
    val addrIO = new SPadAddrIO(dataWidth, padSize)
    val dataIO = new SPadDataIO(dataWidth, padSize)
  })
  val decoupledDataIO: DecoupledIO[StreamBitsIO] = io.commonIO.dataLenFinIO.writeInDataIO
  val dataLenReg: UInt = RegInit(9.U(dataLenWidth.W))
  val dataWire: UInt = Wire(UInt(dataWidth.W))
  dataLenReg := io.commonIO.dataLenFinIO.streamLen
  val padWriteIndexReg: UInt = RegInit(0.U(log2Ceil(padSize).W))
  val padReadIndexReg: UInt = RegInit(0.U(log2Ceil(padSize).W))

  val writeWrapWire: Bool = Wire(Bool())
  val readWrapWire: Bool = Wire(Bool())
  val readIndexInc: Bool = Wire(Bool()) // true, then read index increase
  writeWrapWire := padWriteIndexReg === (dataLenReg - 1.U)
  readWrapWire := padReadIndexReg === (dataLenReg - 1.U)
  // write logic 1
  when (decoupledDataIO.valid) {
    decoupledDataIO.ready := true.B
    padWriteIndexReg := padWriteIndexReg + 1.U
    when (writeWrapWire) {
      padWriteIndexReg := 0.U
    }
  }.otherwise {
    decoupledDataIO.ready := false.B
  }
  // read logic 1
  when (readIndexInc) {
    padReadIndexReg := padReadIndexReg + 1.U // when readEn == true.B, then next clock cycle, increase
    when (readWrapWire) {
      padReadIndexReg := 0.U
    }
  }
  // common IO connections
  io.commonIO.writeIdx := padWriteIndexReg
  io.commonIO.dataLenFinIO.writeFin := decoupledDataIO.valid && writeWrapWire // TODO: check it
  io.commonIO.columnNum := padReadIndexReg
}

class SPadAddrIO(val dataWidth: Int, val padSize: Int) extends Bundle {
  val readInIdx: UInt = Input(UInt(log2Ceil(padSize).W))
  val indexInc: Bool = Input(Bool())
}

class SPadDataIO(val dataWidth: Int, val padSize: Int) extends Bundle {
  val readInIdx: UInt = Input(UInt(log2Ceil(padSize).W))
  val indexInc: Bool = Input(Bool())
}

class SPadCommonIO(val dataLenWidth: Int, val dataWidth: Int, val padSize: Int) extends Bundle {
  val columnNum: UInt = Output(UInt(dataLenWidth.W)) // the column number, address only
  val readOutData: UInt = Output(UInt(dataWidth.W)) // the data read from SPad
  val readEn: Bool = Input(Bool())
  val writeIdx: UInt = Output(UInt(log2Ceil(padSize).W))
  val dataLenFinIO = new StreamDataLenFinIO(dataWidth, dataLenWidth)
}

class DataStreamIO extends Bundle with PESizeConfig {
  val ipsIO: DecoupledIO[UInt] = Flipped(Decoupled(UInt(psDataWidth.W)))
  val opsIO: DecoupledIO[UInt] = Decoupled(UInt(psDataWidth.W))
  val iactIOs = new DataAddrStreanIO(iactDataWidth, iactAddrWidth, commonLenWidth, commonLenWidth)
  val weightIOs = new DataAddrStreanIO(weightDataWidth, weightAddrWidth, weightDataLenWidth, commonLenWidth)
}

class DataAddrStreanIO(val dataWidth: Int, addr_width: Int, dataLenWidth: Int, addrLen_width: Int) extends Bundle {
  val dataIOs = new StreamDataLenFinIO(dataWidth, dataLenWidth) // dataSPad inputs and output writeFin
  val addrIOs = new StreamDataLenFinIO(addr_width, addrLen_width) // addrSPad inputs and output writeFin
}

class StreamDataLenFinIO(val streamWidth: Int, streamLenWidth: Int) extends Bundle { // for write data into SPad
  val writeInDataIO: DecoupledIO[StreamBitsIO] = Flipped(Decoupled(new StreamBitsIO(streamWidth)))
  val streamLen: UInt = Input(UInt(streamLenWidth.W)) // length of addr
  val writeFin: Bool = Output(Bool())
}

class StreamBitsIO(val dataWidth: Int) extends Bundle {
  val data: UInt = UInt(dataWidth.W)
}

class PECtrlToPadIO extends Bundle {
  //val mcrenf: Vec[UInt] = Vec(6, UInt(5.W))
  val pSumEnqOrProduct: DecoupledIO[Bool] = Decoupled(Bool()) // true, then read from FIFO; false, then use product
  val doMACEn: Bool = Output(Bool())
}

trait MCRENFConfig extends PESizeConfig { // contains some scala values
  val M0: Int = 6 // weights reuse M0 times
  val C0: Int = 3 // different input feature maps and their weights reuse
  val R: Int = weightHeight //
  val E: Int = ofmapHeight // same row of weights in a PE
  val N0: Int = 2 // the number of partial sum
  val F0: Int = 2 // one row of partial sum
  val MCRENF: List[Int] = List(M0, C0, R, E, N0, F0)
  // C0*R0 < iacAddrSize = 9
  // C0*R0*E0*N0*F0 <
}

trait SPadSizeConfig extends PESizeConfig {
  val pSumDataSPadSize: Int = 32
  val iactDataSPadSize: Int = 16
  val iactAddrSPadSize: Int = 15
  //val iactAddrSPadSize: Int = 9
  val weightDataSPadSize: Int = 192 // 96 if SIMD
  val weightAddrSPadSize: Int = 16
}

trait PESizeConfig { // TODO: move this config to a higher level package
  val weightHeight: Int = 2
  val ofmapHeight: Int = 2
  val iactDataWidth: Int = 12 // 8-bit data and 4-bit count
  val iactAddrWidth: Int = 4
  val weightDataWidth: Int = 12 // 24 if SIMD
  val weightAddrWidth: Int = 7
  val cscDataWidth: Int = 8 // compressed sparse column data width
  val cscCountWidth: Int = 4 // compressed sparse column count width
  val calDataWidth: Int = 20
  val psDataWidth: Int = 20
  val fifoSize: Int = 4
  val commonLenWidth: Int = 4
  val weightDataLenWidth: Int = 8
  val zeroColumnCode: Int = 15 // when one address vector's element equals to zeroColumnCode, then it is a zero column
}

package dla.pe

import chisel3._
import chisel3.util._

class nonTpActSPadDataModule(padSize: Int, dataWidth: Int)
  extends Module with SPadSizeConfig with PESizeConfig with MCRENFConfigRS {
  val io: SPadNonTpModuleIO = IO(new SPadNonTpModuleIO (dataWidth = dataWidth, padSize = padSize))
  protected val decoupledDataIO: DecoupledIO[UInt] = io.dataPath.writeInData.data
  protected val dataWire: UInt = Wire(UInt(dataWidth.W))
  protected val padWriteIndexReg: UInt = RegInit(0.U(log2Ceil(padSize).W))
  protected val padReadIndexReg: UInt = RegInit(0.U(log2Ceil(padSize).W))
  protected val writeWrapWire: Bool = Wire(Bool())
  protected val readWrapWire: Bool = Wire(Bool())
  protected val readIndexInc: Bool = Wire(Bool()) // true, then read index increase or sliding change back
  protected val writeFire: Bool = Wire(Bool())

  protected val readSlidingInc: Bool = RegInit(false.B)
  protected val currentSlidingWire: UInt = Wire(UInt(log2Ceil(F).W))
  protected val inActDataLB: UInt = RegInit(0.U(log2Ceil(C0 * (S + F)).W))
  protected val inActDataUB: UInt = RegInit(0.U(log2Ceil(C0 * (S + F)).W)) // non-inclusive
  protected val futureInActDataLB: UInt = RegInit(0.U(log2Ceil(C0 * (S + F)).W))
  protected val dataSPad: Vec[UInt] = RegInit(VecInit(Seq.fill(padSize)(0.U(dataWidth.W))))
  dataSPad.suggestName("dataSPadReg")
  protected val currentColTopIdxReg: UInt = RegInit(0.U(log2Ceil(C0 * (S + F)).W))
  //  protected val futureLBStartReg: UInt = Wire(UInt(log2Ceil(E0 * N0).W))
  protected val futureLBStartReg: UInt = RegInit(0.U(log2Ceil(C0 * (S + F)).W)) // setup for future LB, when do sliding, assignment
  protected val inActColIncWire: Bool = Wire(Bool())
  protected val weightIdxIncWire: Bool = Wire(Bool())
  protected val padEqMpy:Bool = Wire(Bool())
  protected val padEqWA:Bool = Wire(Bool())
  protected val padEqID:Bool = Wire(Bool())
  protected val currentCountAtFutureLB: Bool = RegInit(false.B) // these two are used to determine the future LB
  protected val lastCountNotAtFutureLB: Bool = RegInit(false.B)
  protected val mightWeightZeroColumn:Bool = Wire(Bool())
  protected val inActAdrReadInIdxWire: UInt = Wire(UInt(inActAdrWidth.W))
  protected val lastInActAdrDataReg: UInt = RegInit(0.U(inActAdrWidth.W))
  protected val inActAdrLastReadInIdxWire: UInt = Wire(UInt(inActAdrWidth.W))
  protected val futureLBStartRegChange: Bool = Wire(Bool())
  protected val inActReadIdxBeyondUB: Bool = Wire(Bool())
  protected val inActLastNonZeroEle:Bool = Wire(Bool())

  writeFire := decoupledDataIO.fire() && io.ctrlPath.writeEn
  writeWrapWire := decoupledDataIO.bits === 0.U && writeFire  // f: reach our end set 0
  readWrapWire := (dataWire === 0.U) && (currentSlidingWire === (F.U / U.U - 1.U))
  // write logic 1
  decoupledDataIO.ready := true.B // as a memory, its always ready to be wrote in
  padWriteIndexReg := Mux(writeFire, Mux(writeWrapWire, 0.U, padWriteIndexReg + 1.U), padWriteIndexReg)

  // common IO connections
  io.ctrlPath.writeIdx := padWriteIndexReg
  io.ctrlPath.writeFin := writeWrapWire
  inActAdrReadInIdxWire := io.ctrlPath.readInIdx
  io.dataPath.columnNum := padReadIndexReg
  io.dataPath.slidingBoxLB := inActDataLB
  io.dataPath.slidingBoxUB := inActDataUB
  io.dataPath.futureLBStart := futureLBStartReg
  inActColIncWire := io.ctrlPath.inActColInc
  weightIdxIncWire := io.ctrlPath.weightIdxInc
  currentSlidingWire := io.dataPath.currentSliding
  mightWeightZeroColumn := io.ctrlPath.mightWeightZeroColumn
  inActDataLB := currentSlidingWire * U.U * C0.U
  inActDataUB := currentSlidingWire * U.U * C0.U + C0.U * S.U - 1.U // make the upper bound inclusive to potentially save bitwidth
  futureInActDataLB := (currentSlidingWire + 1.U) * U.U * C0.U
  padEqMpy := io.ctrlPath.padEqMpyBool
  padEqWA := io.ctrlPath.padEqWABool
  padEqID := io.ctrlPath.padEqIDBool
  futureLBStartRegChange := futureLBStartReg === padReadIndexReg
  io.ctrlPath.inActReadIdxBeyondUB := inActReadIdxBeyondUB
  io.ctrlPath.inActLastNonZeroEle := inActLastNonZeroEle

  // it monitors counts in this col
  currentCountAtFutureLB := idxGetRow(padReadIndexReg) >= futureInActDataLB
  lastCountNotAtFutureLB := (dataSPad(padReadIndexReg - 1.U)(cscCountWidth - 1, 0) < futureInActDataLB) || (padReadIndexReg === 0.U) ||
    (padReadIndexReg === inActAdrLastReadInIdxWire)

  // when Mpy & weightColFinish & !inActColFinish & nextIdxData > UB
  // logic first or part: make sure when dealing the last inAct in a certain sliding window, before the last weight MAC result WB,
  // set the Reg as true so that the WB stage could turn to ID stage for right data with sliding rather than taking next padReadIndexReg+1
  // logic second or part: make sure when last inAct in the window occur weightZeroCol, it cannot proceed to padEqMpy,
  // so that need to take it into account also
  // weightIdxIncWire: when weightFinish or this weightColFinish
  readSlidingInc := ((padEqMpy && weightIdxIncWire && (!inActColIncWire)) || (padEqWA && mightWeightZeroColumn)) &&
    ((((dataSPad(padReadIndexReg + 1.U)(cscCountWidth - 1, 0) > inActDataUB) && (padReadIndexReg + 1.U < inActAdrReadInIdxWire)) || // when the sliding window finish, which is next readInd is larger than the UB
      (padReadIndexReg + 1.U === inActAdrReadInIdxWire)) &&   // when the sliding window finish, which is needed to slide but there is no other element in this inAct col
      (currentSlidingWire =/= F.U - 1.U))   // when the UB is not at the maximum, precondition

  io.ctrlPath.slidingInc := readSlidingInc
  inActAdrLastReadInIdxWire := lastInActAdrDataReg
  inActReadIdxBeyondUB := idxGetRow(padReadIndexReg) > inActDataUB
  inActLastNonZeroEle := padReadIndexReg === 12.U

  when(futureLBStartRegChange){
    lastInActAdrDataReg := inActAdrReadInIdxWire
  }
  // write logic 2
  when(writeFire) {
    dataSPad(padWriteIndexReg) := decoupledDataIO.bits
  }
  // read logic 1
  when(readIndexInc) {
    when(readSlidingInc) { // re-direct the readInIdx
      padReadIndexReg := futureLBStartReg
    } .otherwise {
      when(currentCountAtFutureLB && lastCountNotAtFutureLB) {
        futureLBStartReg := padReadIndexReg
      }
      when(readWrapWire) {
        padReadIndexReg := 0.U
      }
      padReadIndexReg := padReadIndexReg + 1.U // when readEn == true.B, then next clock cycle, increase
    }
  }
  dataWire := dataSPad(padReadIndexReg)
  def idxGetRow(padReadIndexReg: UInt): UInt = {
    dataSPad(padReadIndexReg)(cscCountWidth - 1, 0)
  }
  // read logic 2
  readIndexInc := io.ctrlPath.indexInc
  io.dataPath.readOutData := dataWire
}

class WeightSPadAdrModule(PadSize: Int, DataWidth: Int) extends SPadAdrModule(PadSize, DataWidth) {
  when (io.ctrlPath.readInIdxEn) {
    padReadIndexReg := io.ctrlPath.readInIdx               // f: why here?
    // seems like the readInIdx is connected to the readOutData in PE
    // and the weight needs to jump with the InAct
  }
}

class SPadAdrModule(PadSize: Int, val DataWidth: Int)  // the padSize make sure larger than input num + 1, for the nextDataWire
  extends SPadCommonModule(PadSize, DataWidth) with SPadSizeConfig {
  protected val adrSPad: Vec[UInt] = RegInit(VecInit(Seq.fill(PadSize)(0.U(DataWidth.W))))
  adrSPad.suggestName("addressSPad")
  // write logic 2
  when (writeFire) {
    adrSPad(padWriteIndexReg) := decoupledDataIO.bits
  }
  // read logic 1
  regReadLogic1()
  // read logic 2
  dataWire := adrSPad(padReadIndexReg)
  io.dataPath.readOutData := dataWire   // connect readoutdata to MatrixdataReg/wire for multiply
  readIndexInc := io.ctrlPath.indexInc
}

class SPadDataModule(PadSize: Int, DataWidth: Int, val sramOrReg: Boolean)
  extends SPadCommonModule(PadSize, DataWidth) with SPadSizeConfig {
  if (sramOrReg) { // true for weight SPad sram
    val dataSPad: SyncReadMem[UInt] = SyncReadMem(PadSize,UInt(DataWidth.W))
    dataSPad.suggestName("dataSPadSRAM")
    // write logic 2
    when (writeFire) {
      dataSPad.write(padWriteIndexReg, decoupledDataIO.bits)  // f: Sync RAM write
    }
    // read logic 1
    when (io.ctrlPath.readInIdxEn) {
      padReadIndexReg := io.ctrlPath.readInIdx
    } .otherwise {
      regReadLogic1()
    }
    // read logic 2
    dataWire := dataSPad.read(padReadIndexReg, io.ctrlPath.readEn)
  }else{
    val dataSPad: Vec[UInt] = RegInit(VecInit(Seq.fill(PadSize)(0.U(DataWidth.W))))
    dataSPad.suggestName("dataSPadReg")
    when (writeFire) {
      dataSPad(padWriteIndexReg) := decoupledDataIO.bits
    }
    // read logic 1
    regReadLogic1()
    // read logic 2
    dataWire := dataSPad(padReadIndexReg)
  }
  readIndexInc := io.ctrlPath.indexInc
  io.dataPath.readOutData := dataWire        //f: no matter what kind, it will connect to the readOutData
}

class SPadCommonModule(padSize: Int, dataWidth: Int) extends Module {
  lazy val io: SPadCommonModuleIO = IO(new SPadCommonModuleIO(dataWidth = dataWidth, padSize = padSize))
  protected val decoupledDataIO: DecoupledIO[UInt] = io.dataPath.writeInData.data
  protected val dataWire: UInt = Wire(UInt(dataWidth.W))
  protected val padWriteIndexReg: UInt = RegInit(0.U(log2Ceil(padSize).W))
  protected val padReadIndexReg: UInt = RegInit(0.U(log2Ceil(padSize).W))
  protected val writeWrapWire: Bool = Wire(Bool())
  protected val readWrapWire: Bool = Wire(Bool())
  protected val readIndexInc: Bool = Wire(Bool()) // true, then read index increase
  protected val writeFire: Bool = Wire(Bool())
  writeFire := decoupledDataIO.fire() && io.ctrlPath.writeEn
  writeWrapWire := decoupledDataIO.bits === 0.U && writeFire  // f: reach our end set 0
  readWrapWire := dataWire === 0.U && readIndexInc
  // write logic 1
  decoupledDataIO.ready := true.B // as a memory, its always ready to be wrote in
  padWriteIndexReg := Mux(writeFire, Mux(writeWrapWire, 0.U, padWriteIndexReg + 1.U), padWriteIndexReg)
  // common IO connections
  io.ctrlPath.writeIdx := padWriteIndexReg
  io.ctrlPath.writeFin := writeWrapWire
  io.dataPath.columnNum := padReadIndexReg
  def regReadLogic1(): Unit = {
    when (readIndexInc) {
      padReadIndexReg := padReadIndexReg + 1.U // when readEn == true.B, then next clock cycle, increase
      when (readWrapWire) {
        padReadIndexReg := 0.U
      }
    }
  }
}

class PSumSPad(debug: Boolean) extends Module with SPadSizeConfig with PESizeConfig {
  val io: PSumSPadIO = IO(new PSumSPadIO)
  protected val pSumDataSPadReg: Vec[UInt] = RegInit(VecInit(Seq.fill(pSumDataSPadSize)(0.U(psDataWidth.W))))
  pSumDataSPadReg.suggestName("pSumDataSPadReg")
  protected val readOutDataWire = Wire(UInt(psDataWidth.W))
  readOutDataWire := pSumDataSPadReg(io.ctrlPath.readIdx)
  io.dataPath.ipsIO.ready := !io.dataPath.opsIO.ready // when not read, (fred) if receive opsIO.ready true, the ipsIO.ready be false and halt input
  io.dataPath.opsIO.valid := !io.dataPath.ipsIO.valid // when not write
  io.dataPath.opsIO.bits := readOutDataWire  // for read
  when (io.dataPath.ipsIO.fire()) { //my output ready true, my input valid is also true
    pSumDataSPadReg(io.ctrlPath.writeIdx) := io.dataPath.ipsIO.bits // for write
  }
}

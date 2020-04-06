package dla.pe

import chisel3._
import chisel3.util._

class WeightSPadAdrModule(PadSize: Int, DataWidth: Int) extends SPadAdrModule(PadSize, DataWidth) {
  when (io.ctrlPath.readInIdxEn) {
    padReadIndexReg := io.ctrlPath.readInIdx
  }
}

class SPadAdrModule(PadSize: Int, val DataWidth: Int)
  extends SPadCommonModule(PadSize, DataWidth) with SPadSizeConfig {
  private val adrSPad: Vec[UInt] = RegInit(VecInit(Seq.fill(PadSize)(0.U(DataWidth.W))))
  adrSPad.suggestName("addressSPad")
  // write logic 2
  when (writeFire) {
    adrSPad(padWriteIndexReg) := decoupledDataIO.bits
  }
  // read logic 1
  regReadLogic1()
  // read logic 2
  dataWire := adrSPad(padReadIndexReg)
  io.dataPath.readOutData := dataWire
  readIndexInc := io.ctrlPath.indexInc
}

class SPadDataModule(PadSize: Int, DataWidth: Int, val sramOrReg: Boolean)
  extends SPadCommonModule(PadSize, DataWidth) with SPadSizeConfig {
  if (sramOrReg) { // true for weight SPad
    val dataSPad: SyncReadMem[UInt] = SyncReadMem(PadSize,UInt(DataWidth.W))
    dataSPad.suggestName("dataSPadSRAM")
    // write logic 2
    when (writeFire) {
      dataSPad.write(padWriteIndexReg, decoupledDataIO.bits)
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
  io.dataPath.readOutData := dataWire
}

class SPadCommonModule(padSize: Int, dataWidth: Int) extends Module {
  val io: SPadCommonModuleIO = IO(new SPadCommonModuleIO(dataWidth = dataWidth, padSize = padSize))
  protected val decoupledDataIO: DecoupledIO[UInt] = io.dataPath.writeInData.data
  protected val dataWire: UInt = Wire(UInt(dataWidth.W))
  protected val padWriteIndexReg: UInt = RegInit(0.U(log2Ceil(padSize).W))
  protected val padReadIndexReg: UInt = RegInit(0.U(log2Ceil(padSize).W))
  protected val writeWrapWire: Bool = Wire(Bool())
  protected val readWrapWire: Bool = Wire(Bool())
  protected val readIndexInc: Bool = Wire(Bool()) // true, then read index increase
  protected val writeFire: Bool = Wire(Bool())
  writeFire := decoupledDataIO.fire() && io.ctrlPath.writeEn
  writeWrapWire := decoupledDataIO.bits === 0.U && writeFire
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

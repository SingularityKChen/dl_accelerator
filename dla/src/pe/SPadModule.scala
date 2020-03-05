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
  //adrSPad.suggestName("addressSPad")
  // write logic 2
  when (decoupledDataIO.valid && io.ctrlPath.writeEn) {
    adrSPad(padWriteIndexReg) := decoupledDataIO.bits
  }
  // read logic 1
  when (readIndexInc) {
    padReadIndexReg := padReadIndexReg + 1.U // when readEn == true.B, then next clock cycle, increase
    when (readWrapWire) {
      padReadIndexReg := 0.U
    }
  }
  // read logic 2
  dataWire := adrSPad(padReadIndexReg)
  io.dataPath.readOutData := dataWire
  readIndexInc := io.ctrlPath.indexInc
}

class SPadDataModule(PadSize: Int, DataWidth: Int, val sramOrReg: Boolean)
  extends SPadCommonModule(PadSize, DataWidth) with SPadSizeConfig {
  if (sramOrReg) { // true for weight SPad
    val dataSPad: SyncReadMem[UInt] = SyncReadMem(PadSize,UInt(DataWidth.W))
    //dataSPad.suggestName("dataSPadSRAM")
    // write logic 2
    when (decoupledDataIO.valid && io.ctrlPath.writeEn) {
      dataSPad.write(padWriteIndexReg, decoupledDataIO.bits)
    }
    // read logic 1
    when (io.ctrlPath.readInIdxEn) {
      padReadIndexReg := io.ctrlPath.readInIdx
    } .otherwise {
      when (readIndexInc) {
        padReadIndexReg := padReadIndexReg + 1.U // when readEn == true.B, then next clock cycle, increase
        when (readWrapWire) {
          padReadIndexReg := 0.U
        }
      }
    }
    // read logic 2
    dataWire := dataSPad.read(padReadIndexReg, io.ctrlPath.readEn)
  }else{
    val dataSPad: Vec[UInt] = RegInit(VecInit(Seq.fill(PadSize)(0.U(DataWidth.W))))
    //dataSPad.suggestName("dataSPadReg")
    when (decoupledDataIO.valid && io.ctrlPath.writeEn) {
      dataSPad(padWriteIndexReg) := decoupledDataIO.bits
    }
    // read logic 1
    when (readIndexInc) {
      padReadIndexReg := padReadIndexReg + 1.U // when readEn == true.B, then next clock cycle, increase
      when (readWrapWire) {
        padReadIndexReg := 0.U
      }
    }
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
  writeWrapWire := decoupledDataIO.bits === 0.U && io.ctrlPath.writeEn
  readWrapWire := dataWire === 0.U && readIndexInc
  // write logic 1
  when (decoupledDataIO.valid && io.ctrlPath.writeEn) {
    decoupledDataIO.ready := true.B
    padWriteIndexReg := padWriteIndexReg + 1.U
    when (writeWrapWire) {
      padWriteIndexReg := 0.U
    }
  }.otherwise {
    decoupledDataIO.ready := false.B
  }
  // common IO connections
  io.ctrlPath.writeIdx := padWriteIndexReg
  io.ctrlPath.writeFin := writeWrapWire
  io.dataPath.columnNum := padReadIndexReg
}

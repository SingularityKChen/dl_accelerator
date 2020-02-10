package dla.pe

import chisel3._
import chisel3.util._

class WeightSPadAddrModule(PadSize: Int, DataWidth: Int) extends SPadAddrModule(PadSize, DataWidth) {
  when (io.addrIO.readInIdxEn) {
    padReadIndexReg := io.addrIO.readInIdx
  }
}

class SPadAddrModule(PadSize: Int, val DataWidth: Int)
  extends SPadCommonModule(PadSize, DataWidth) with SPadSizeConfig {
  val addrSPad: Vec[UInt] = RegInit(VecInit(Seq.fill(PadSize)(0.U(DataWidth.W))))
  // write logic 2
  when (decoupledDataIO.valid && io.commonIO.writeEn) {
    addrSPad(padWriteIndexReg) := decoupledDataIO.bits.data
  }
  // read logic 1
  when (readIndexInc) {
    padReadIndexReg := padReadIndexReg + 1.U // when readEn == true.B, then next clock cycle, increase
    when (readWrapWire) {
      padReadIndexReg := 0.U
    }
  }
  // read logic 2
  dataWire := addrSPad(padReadIndexReg)
  io.commonIO.readOutData := dataWire
  readIndexInc := io.addrIO.indexInc
}

class SPadDataModule(PadSize: Int, DataWidth: Int, val sramOrReg: Boolean)
  extends SPadCommonModule(PadSize, DataWidth) with SPadSizeConfig {
  if (sramOrReg) { // true for weight SPad
    val dataSPad: SyncReadMem[UInt] = SyncReadMem(PadSize,UInt(DataWidth.W))
    // write logic 2
    when (decoupledDataIO.valid && io.commonIO.writeEn) {
      dataSPad.write(padWriteIndexReg, decoupledDataIO.bits.data)
    }
    // read logic 1
    when (io.dataIO.readInIdxEn) {
      padReadIndexReg := io.dataIO.readInIdx
    } .otherwise {
      when (readIndexInc) {
        padReadIndexReg := padReadIndexReg + 1.U // when readEn == true.B, then next clock cycle, increase
        when (readWrapWire) {
          padReadIndexReg := 0.U
        }
      }
    }
    // read logic 2
    dataWire := dataSPad.read(padReadIndexReg, io.commonIO.readEn)
  }else{
    val dataSPad: Vec[UInt] = RegInit(VecInit(Seq.fill(PadSize)(0.U(DataWidth.W))))
    when (decoupledDataIO.valid && io.commonIO.writeEn) {
      dataSPad(padWriteIndexReg) := decoupledDataIO.bits.data
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
  readIndexInc := io.dataIO.indexInc
  io.commonIO.readOutData := dataWire
}

class SPadCommonModule(padSize: Int, dataWidth: Int) extends Module {
  val io = IO(new Bundle{
    val commonIO = new SPadCommonIO(dataWidth, padSize)
    val addrIO = new SPadAddrIO(dataWidth, padSize)
    val dataIO = new SPadDataIO(dataWidth, padSize)
  })
  val decoupledDataIO: DecoupledIO[StreamBitsIO] = io.commonIO.dataLenFinIO.writeInDataIO
  val dataWire: UInt = Wire(UInt(dataWidth.W))
  val padWriteIndexReg: UInt = RegInit(0.U(log2Ceil(padSize).W))
  val padReadIndexReg: UInt = RegInit(0.U(log2Ceil(padSize).W))

  val writeWrapWire: Bool = Wire(Bool())
  val readWrapWire: Bool = Wire(Bool())
  val readIndexInc: Bool = Wire(Bool()) // true, then read index increase
  writeWrapWire := decoupledDataIO.bits.data === 0.U && io.commonIO.writeEn
  readWrapWire := dataWire === 0.U && readIndexInc
  // write logic 1
  when (decoupledDataIO.valid && io.commonIO.writeEn) {
    decoupledDataIO.ready := true.B
    padWriteIndexReg := padWriteIndexReg + 1.U
    when (writeWrapWire) {
      padWriteIndexReg := 0.U
    }
  }.otherwise {
    decoupledDataIO.ready := false.B
  }
  // common IO connections
  io.commonIO.writeIdx := padWriteIndexReg
  io.commonIO.dataLenFinIO.writeFin := writeWrapWire
  io.commonIO.columnNum := padReadIndexReg
}

package dla.diplomatic

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{IdRange, TransferSizes}
import freechips.rocketchip.tilelink.TLClientParameters

case class EyerissGetNodeParameters(sramName: String, sourceNum: Int) extends TLClientParameters(
  name = s"Eyeriss$sramName",
  // @todo
  sourceId = IdRange(0, sourceNum),
  // @todo
  supportsGet = TransferSizes(1, 4)
)

case class EyerissPutGetNodeParameters(sramName: String, sourceNum: Int) extends TLClientParameters(
  name = s"EyerissPSumSRAM$sramName",
  // @todo
  sourceId = IdRange(0, sourceNum),
  // @todo
  supportsGet = TransferSizes(1, 4),
  // @todo
  supportsPutFull = TransferSizes(1, 4) // avoid using partial to avoid mask
)

case class MemCtrlParameters (
                             addressBits: Int,
                             sizeBits: Int,
                             dataBits: Int,
                             nIds: Int // the number of source id
                             )

class MemCommonIO()(implicit p: MemCtrlParameters) extends Bundle {
  val address: UInt = Output(UInt(p.addressBits.W))
  val size: UInt = Output(UInt(p.sizeBits.W))
  val source: UInt = Output(UInt(log2Ceil(p.nIds).W))
  val inActStarAdr: UInt = Input(UInt(p.addressBits.W)) // the start address of inAct
  val weightStarAdr: UInt = Input(UInt(p.addressBits.W)) // the start address of weight
  val pSumStarAdr: UInt = Input(UInt(p.addressBits.W)) // the start address of PSum
  val oneInActSRAMSize: UInt = Input(UInt(p.sizeBits.W))
  val oneWeightSize: UInt = Input(UInt(p.sizeBits.W))
  val onePSumSRAMSize: UInt = Input(UInt(p.sizeBits.W))
}

class MemCtrlModule(implicit p: MemCtrlParameters) extends Module {
  val io: MemCommonIO = IO(new MemCommonIO()(p))
  private val inActStarAdrReg = RegInit(0.U(p.addressBits.W))
  private val weightStarAdrReg = RegInit(0.U(p.addressBits.W))
  private val pSumStarAdrReg = RegInit(0.U(p.addressBits.W))
  private val readAdrReg = RegInit(0.U(p.addressBits.W))
  private val writeAdrReg = RegInit(0.U(p.addressBits.W)) // for pSum to writeBack
  private val oneInActSRAMSizeReg = RegInit(0.U(p.sizeBits.W))
  private val oneWeightSizeReg = RegInit(0.U(p.sizeBits.W))
  private val onePSumSRAMSizeReg = RegInit(0.U(p.sizeBits.W))
  // source
  io.source := 0.U // TODO
  // address
  /** the address of read inAct */
  inActStarAdrReg := io.inActStarAdr
  oneInActSRAMSizeReg := io.oneInActSRAMSize
  readAdrReg := inActStarAdrReg + (oneInActSRAMSizeReg << 3).asUInt // TODO: check and add holdUnless
  io.address := readAdrReg
  // size
  io.size := oneInActSRAMSizeReg // TODO: add more cases
}

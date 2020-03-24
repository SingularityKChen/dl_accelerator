package dla.diplomatic

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.leftOR

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

class EyerissIDMapGenerator(numIds: Int) extends Module {
  require(numIds > 0)

  val w = log2Up(numIds)
  val io = IO(new Bundle {
    val free: DecoupledIO[UInt] = Flipped(Decoupled(UInt(w.W)))
    val alloc: DecoupledIO[UInt] = Decoupled(UInt(w.W))
    val finish: Bool = Output(Bool())
  })

  io.free.ready := true.B

  // True indicates that the id is available
  private val reqBitmap: UInt = RegInit(((BigInt(1) << numIds) - 1).U(numIds.W)) // True indicates that the id is available
  private val respBitmap: UInt = RegInit(0.U(numIds.W)) // false means haven't receive response

  private val select: UInt = (~(leftOR(reqBitmap) << 1)).asUInt & reqBitmap
  io.alloc.bits := OHToUInt(select)
  io.alloc.valid := reqBitmap.orR()

  private val clr: UInt = WireDefault(0.U(numIds.W))
  when(io.alloc.fire()) {
    clr := UIntToOH(io.alloc.bits)
  }

  private val set: UInt = WireDefault(0.U(numIds.W))
  when(io.free.fire()) {
    set := UIntToOH(io.free.bits) // this is the sourceId that finishes
  }
  respBitmap := respBitmap | set
  reqBitmap := (reqBitmap & (~clr).asUInt)
  private val finishWire = respBitmap.andR()
  when (finishWire) {
    respBitmap := 0.U
    reqBitmap := ((BigInt(1) << numIds) - 1).U
  }
  io.finish := finishWire
  //assert(!io.free.valid || !(reqBitmap & (~clr).asUInt) (io.free.bits)) // No double freeing
}

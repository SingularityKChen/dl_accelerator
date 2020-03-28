package dla.diplomatic

import chisel3._
import chisel3.util._
import dla.pe.PESizeConfig
import freechips.rocketchip.util._

case class EyerissMemCtrlParameters (
                             addressBits: Int,
                             inActSizeBits: Int,
                             weightSizeBits: Int,
                             pSumSizeBits: Int,
                             inActIds: Int, // the number of inAct source id
                             weightIds: Int, // the number of weight source id
                             pSumIds: Int// the number of pSum source id
                             )

class MemCommonIO(val nIds: Int, val addressBits: Int, val sizeBits: Int) extends Bundle {
  val address: UInt = Output(UInt(addressBits.W))
  val sourceAlloc: DecoupledIO[UInt] = Decoupled(UInt(log2Ceil(nIds).W))
  val sourceFree: DecoupledIO[UInt] = Flipped(Decoupled(UInt(log2Ceil(nIds).W)))
  val startAdr: UInt = Input(UInt(addressBits.W))
  val reqSize: UInt = Input(UInt(sizeBits.W))
}

class EyerissMemCtrlIO()(implicit val p: EyerissMemCtrlParameters) extends Bundle {
  val inActIO = new MemCommonIO(nIds = p.inActIds, addressBits = p.addressBits, sizeBits = p.inActSizeBits)
  val weightIO = new MemCommonIO(nIds = p.weightIds, addressBits = p.addressBits, sizeBits = p.weightSizeBits)
  val pSumIO = new MemCommonIO(nIds = p.pSumIds, addressBits = p.addressBits, sizeBits = p.pSumSizeBits)
}

/** This module can generate the address, sourceId, which are used in TileLink get/put.
  * Also, this module is able to manage all the source id.
  * */

class EyerissMemCtrlModule()(implicit val p: EyerissMemCtrlParameters) extends Module
  with PESizeConfig {
  val io: EyerissMemCtrlIO = IO(new EyerissMemCtrlIO()(p))
  private val inActIdMap = Module(new EyerissIDMapGenerator(p.inActIds)).io
  private val weightIdMap = Module(new EyerissIDMapGenerator(p.weightIds)).io
  private val pSumIdMap = Module(new EyerissIDMapGenerator(p.pSumIds)).io
  private val inActStarAdrReg = RegInit(0.U(p.addressBits.W))
  private val weightStarAdrReg = RegInit(0.U(p.addressBits.W))
  private val pSumStarAdrReg = RegInit(0.U(p.addressBits.W))
  private val inActReqAdrReg = RegInit(0.U(p.addressBits.W))
  private val weightReqAdrReg = RegInit(0.U(p.addressBits.W))
  private val pSumReqAdrReg = RegInit(0.U(p.addressBits.W))
  private val inActReqSizeReg = RegInit(0.U(p.inActSizeBits.W))
  private val weightReqSizeReg = RegInit(0.U(p.weightSizeBits.W))
  private val pSumReqSizeReg = RegInit(0.U(p.pSumSizeBits.W))
  /** connections of input and source generate module */
  io.inActIO.sourceAlloc <> inActIdMap.alloc
  io.inActIO.sourceFree <> inActIdMap.free
  io.weightIO.sourceAlloc <> weightIdMap.alloc
  io.weightIO.sourceFree <> weightIdMap.free
  io.pSumIO.sourceAlloc <> pSumIdMap.alloc
  io.pSumIO.sourceFree <> pSumIdMap.free
  /** the start address */
  inActStarAdrReg := io.inActIO.startAdr
  weightStarAdrReg := io.weightIO.startAdr
  pSumStarAdrReg := io.pSumIO.startAdr
  /** the require size from decoder module*/
  inActReqSizeReg := io.inActIO.reqSize
  weightReqSizeReg := io.weightIO.reqSize
  pSumReqSizeReg := io.pSumIO.reqSize
  /** each require address */
  private val inActReqAdrAcc = RegInit(0.U(p.addressBits.W))
  private val weightReqAdrAcc = RegInit(0.U(p.addressBits.W))
  private val pSumReqAdrAcc = RegInit(0.U(p.addressBits.W))
  inActReqAdrAcc := (inActReqAdrAcc + (inActReqSizeReg << log2Ceil(cscDataWidth)).asUInt()).holdUnless(inActIdMap.finish)
  // TODO: different source id should own different address accumulator regs.
  inActReqAdrReg := inActStarAdrReg + inActReqAdrAcc
  weightReqAdrAcc := (weightReqAdrAcc + (weightReqSizeReg << log2Ceil(cscDataWidth)).asUInt()).holdUnless(weightIdMap.finish)
  weightReqAdrReg := weightStarAdrReg + weightReqAdrAcc
  pSumReqAdrAcc := (pSumReqAdrAcc + pSumReqSizeReg * psDataWidth.U).holdUnless(pSumIdMap.finish)
  pSumReqAdrReg := pSumStarAdrReg + pSumReqAdrAcc
  /** connections of require address */
  io.inActIO.address := inActReqAdrReg
  io.weightIO.address := weightReqAdrReg
  io.pSumIO.address := pSumReqAdrReg
}

class EyerissIDMapGenIO(val sourceWidth: Int) extends Bundle {
  val free: DecoupledIO[UInt] = Flipped(Decoupled(UInt(sourceWidth.W)))
  val alloc: DecoupledIO[UInt] = Decoupled(UInt(sourceWidth.W))
  val finish: Bool = Output(Bool())
}

class EyerissIDMapGenerator(val numIds: Int) extends Module {
  require(numIds > 0)
  private val w = log2Up(numIds)
  val io: EyerissIDMapGenIO = IO(new EyerissIDMapGenIO(w))
  io.free.ready := true.B
  /** [[reqBitmap]] true indicates that the id hasn't send require signal;
    * [[respBitmap]] true indicates that the id has received response;
    * both of them have [[numIds]] bits, and each bit represents one id;
    * */
  private val reqBitmap: UInt = RegInit(((BigInt(1) << numIds) - 1).U(numIds.W)) // True indicates that the id is available
  private val respBitmap: UInt = RegInit(0.U(numIds.W)) // false means haven't receive response
  /** [[select]] is the oneHot code which represents the lowest bit that equals to true;
    * Then use `OHToUInt` to get its binary value.
    * */
  private val select: UInt = (~(leftOR(reqBitmap) << 1)).asUInt & reqBitmap
  io.alloc.bits := OHToUInt(select)
  io.alloc.valid := reqBitmap.orR() // valid when there is any id hasn't sent require signal

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
  /** when all the sources receive response*/
  private val finishWire = respBitmap.andR()
  when (finishWire) {
    respBitmap := 0.U
    reqBitmap := ((BigInt(1) << numIds) - 1).U
  }
  io.finish := finishWire
  //assert(!io.free.valid || !(reqBitmap & (~clr).asUInt) (io.free.bits)) // No double freeing
}

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
  protected val inActIdMap: EyerissIDMapGenerator = Module(new EyerissIDMapGenerator(p.inActIds))
  inActIdMap.suggestName("inActIdMap")
  protected val weightIdMap: EyerissIDMapGenerator = Module(new EyerissIDMapGenerator(p.weightIds))
  weightIdMap.suggestName("weightIdMap")
  protected val pSumIdMap: EyerissIDMapGenerator = Module(new EyerissIDMapGenerator(p.pSumIds))
  pSumIdMap.suggestName("pSumIdMap")
  protected val inActIdMapIO: EyerissIDMapGenIO = inActIdMap.io
  protected val weightIdMapIO: EyerissIDMapGenIO = weightIdMap.io
  protected val pSumIdMapIO: EyerissIDMapGenIO = pSumIdMap.io
  protected val inActStarAdrReg: UInt = RegInit(0.U(p.addressBits.W))
  protected val weightStarAdrReg: UInt = RegInit(0.U(p.addressBits.W))
  protected val pSumStarAdrReg: UInt = RegInit(0.U(p.addressBits.W))
  protected val inActReqAdrReg: UInt = RegInit(0.U(p.addressBits.W))
  protected val weightReqAdrReg: UInt = RegInit(0.U(p.addressBits.W))
  protected val pSumReqAdrReg: UInt = RegInit(0.U(p.addressBits.W))
  protected val inActReqSizeReg: UInt = RegInit(0.U(p.inActSizeBits.W))
  inActReqSizeReg.suggestName("inActReqSizeReg")
  protected val weightReqSizeReg: UInt = RegInit(0.U(p.weightSizeBits.W))
  protected val pSumReqSizeReg: UInt = RegInit(0.U(p.pSumSizeBits.W))
  /** connections of input and source generate module */
  io.inActIO.sourceAlloc <> inActIdMapIO.alloc
  io.inActIO.sourceFree <> inActIdMapIO.free
  io.weightIO.sourceAlloc <> weightIdMapIO.alloc
  io.weightIO.sourceFree <> weightIdMapIO.free
  io.pSumIO.sourceAlloc <> pSumIdMapIO.alloc
  io.pSumIO.sourceFree <> pSumIdMapIO.free
  /** the start address */
  inActStarAdrReg := io.inActIO.startAdr
  weightStarAdrReg := io.weightIO.startAdr
  pSumStarAdrReg := io.pSumIO.startAdr
  /** the require size from decoder module*/
  inActReqSizeReg := io.inActIO.reqSize
  weightReqSizeReg := io.weightIO.reqSize
  pSumReqSizeReg := io.pSumIO.reqSize
  /** each require address */
  protected val inActReqAdrAcc: UInt = RegInit(0.U(p.addressBits.W))
  inActReqAdrAcc.suggestName("inActReqAdrAcc")
  protected val weightReqAdrAcc: UInt = RegInit(0.U(p.addressBits.W))
  weightReqAdrAcc.suggestName("weightReqAdrAcc")
  protected val pSumReqAdrAcc: UInt = RegInit(0.U(p.addressBits.W))
  protected val inActReqFinOnce: Bool = RegInit(false.B) // true when have finished once
  inActReqFinOnce.suggestName("inActReqFinOnce")
  /** as inAct needs require 2 times of SRAM number
    * while `inActReqFinOnce && inActIdMapIO.finish` then that's the second time
    * and it's real finish */
  inActReqFinOnce := Mux(inActIdMapIO.finish, !inActReqFinOnce, inActReqFinOnce)
  inActReqAdrAcc := Mux(inActReqFinOnce && inActIdMapIO.finish, 0.U,
    Mux(inActIdMapIO.alloc.fire(), inActReqAdrAcc + inActReqSizeReg, inActReqAdrAcc)
  )
  inActReqAdrReg := inActStarAdrReg + inActReqAdrAcc
  weightReqAdrAcc := Mux(weightIdMapIO.finish, 0.U,
    Mux(weightIdMapIO.alloc.fire(), weightReqAdrAcc + weightReqSizeReg, weightReqAdrAcc)
  )
  weightReqAdrReg := weightStarAdrReg + weightReqAdrAcc
  pSumReqAdrAcc := Mux(pSumIdMapIO.finish, 0.U,
    Mux(pSumIdMapIO.alloc.fire(), pSumReqAdrAcc + pSumReqSizeReg, pSumReqAdrAcc)
  )
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
  protected val reqBitmap: UInt = RegInit(((BigInt(1) << numIds) - 1).U(numIds.W)) // True indicates that the id is available
  protected val respBitmap: UInt = RegInit(0.U(numIds.W)) // false means haven't receive response
  /** [[select]] is the oneHot code which represents the lowest bit that equals to true;
    * Then use `OHToUInt` to get its binary value.
    * */
  protected val select: UInt = (~(leftOR(reqBitmap) << 1)).asUInt & reqBitmap
  io.alloc.bits := OHToUInt(select)
  io.alloc.valid := reqBitmap.orR() // valid when there is any id hasn't sent require signal

  protected val clr: UInt = WireDefault(0.U(numIds.W))
  when(io.alloc.fire()) {
    clr := UIntToOH(io.alloc.bits)
  }

  protected val set: UInt = WireDefault(0.U(numIds.W))
  when(io.free.fire()) {
    set := UIntToOH(io.free.bits) // this is the sourceId that finishes
  }
  respBitmap := respBitmap | set
  reqBitmap := (reqBitmap & (~clr).asUInt)
  /** when all the sources receive response*/
  protected val finishWire: Bool = respBitmap.andR()
  when (finishWire) {
    respBitmap := 0.U
    reqBitmap := ((BigInt(1) << numIds) - 1).U
  }
  io.finish := finishWire
  //assert(!io.free.valid || !(reqBitmap & (~clr).asUInt) (io.free.bits)) // No double freeing
}

package dla.eyerissTop
import chisel3._
import chisel3.util._
/*import freechips.rocketchip.tile._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

class EyerissLazy(opcodes: OpcodeSet, val n: Int = 4)(implicit p: Parameters) extends LazyRoCC(opcodes) {
  val nXacts = 4      // DMA channel
  val outFlits = 32   // DMA buffer size
  val maxBytes = 64   // DMA TileLink max req byte
  val dmaReader = LazyModule(new StreamReader(nXacts, outFlits, maxBytes)(p))
  val dmaWriter = LazyModule(new StreamWriter(nXacts, maxBytes)(p))
  override lazy val module = new EyerissLazyImp(this)
  tldmaNode :=* dmaReader.node
  tldmaNode :=* dmaWriter.node

}

class EyerissLazyImp(outer: EyerissLazy)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) {

}*/

class EyerissDecodeModule extends Module {
  val io: EyerissDecodeIO = IO(new EyerissDecodeIO)
  private val func = Wire(UInt(7.W))
  private val rs1 = Wire(UInt(5.W))
  private val rs2 = Wire(UInt(5.W))
  private val rd = Wire(UInt(5.W))
  private val inActStrAdr = Reg(UInt(5.W))
  private val weightStrAdr = Reg(UInt(5.W))
  private val pSumStrAdr = Reg(UInt(5.W))
  private val gnmfcsRegVec = Seq.fill(12){RegInit(0.U)}
  private val fnercmRegVec = Seq.fill(6){RegInit(0.U)}
  switch(func) {
    is (0.U) { //LoadPSum
      pSumStrAdr := rd
    }
    is (1.U) { // get InAct and Weight Address
      inActStrAdr := rs1
      weightStrAdr := rs2
    }
    is (2.U) { // G2, N2, M2, F2
      gnmfcsRegVec.head := rs1(5, 3)
      gnmfcsRegVec(1) := rs1(2, 0)
      gnmfcsRegVec(2) := rs2(5, 3)
      gnmfcsRegVec(3) := rs2(2, 0)
    }
    is (3.U) { // C2, S2, G1, N1
      gnmfcsRegVec(4) := rs1(5, 3)
      gnmfcsRegVec(5) := rs1(2, 0)
      gnmfcsRegVec(6) := rs2(5, 3)
      gnmfcsRegVec(7) := rs2(2, 0)
    }
    is (4.U) { // M1, F1, C1, S1
      gnmfcsRegVec(8) := rs1(5, 3)
      gnmfcsRegVec(9) := rs1(2, 0)
      gnmfcsRegVec(10) := rs2(5, 3)
      gnmfcsRegVec(11) := rs2(2, 0)
    }
    is (5.U) { // F0, N0, C0, M0
      fnercmRegVec.head := rs1(5, 3)
      fnercmRegVec(1) := rs1(2, 0)
      fnercmRegVec(4) := rs2(5, 3)
      fnercmRegVec(5) := rs2(2, 0)
    }
    is (6.U) { // E, R
      fnercmRegVec(2) := rs1
      fnercmRegVec(3) := rs2
    }
  }
}

class EyerissDecodeIO extends Bundle {
  //val req = Decoupled()
}
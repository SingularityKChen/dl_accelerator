package dla.eyerissTop
import chisel3._
import chisel3.util._
import freechips.rocketchip.subsystem.{BaseSubsystem, CacheBlockBytes}
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.UIntIsOneOf

case class EyerissDecoderParams(address: BigInt, beatBytes: Int) // whether need beatBytes?

trait EyerissDecoderModule extends HasRegMap {
  val io: EyerissDecoderIO
  implicit val p: Parameters
  def params: EyerissDecoderParams
  private val instructionWidth = 32
  private val compReg = RegInit(false.B) //
  // if compReg = false, that's idle, then CPU sends one instruction, then compReg becomes true,
  // one means busy, and the dla will process this instruction.
  // After this instruction is done, then it becomes false again.
  // In this case, after configuration and get inAct and weight address, this reg will be assigned
  // to true, to show it is computing, and when it finishes, it will be false, and wait for read PSum.
  // When reading PSum, compReg will be assigned to true until it finishes write back.
  private val instructionReg = RegInit(0.U(instructionWidth.W)) // store instructions from CPU TODO: check bits
  regmap(
    0x00 -> Seq(RegField.w(instructionWidth, instructionReg)),
    0x08 -> Seq(RegField.r(1, compReg))
  )
  private val func = Wire(UInt(7.W))
  private val rs1 = Wire(UInt(5.W))
  private val rs2 = Wire(UInt(5.W))
  private val rd = Wire(UInt(5.W))
  private val inActStrAdr = RegInit(0.U(5.W))
  private val weightStrAdr = RegInit(0.U(5.W))
  private val pSumStrAdr = RegInit(0.U(5.W))
  private val gnmfcsRegVec = Seq.fill(12){RegInit(0.U(3.W))}
  private val fnercmRegVec = Seq.fill(6){RegInit(0.U(3.W))}
  // TODO: use other instruction format rather than RoCC instruction format
  func := instructionReg(31, 25)
  rs2 := instructionReg(34, 20)
  rs1 := instructionReg(19, 15)
  rd := instructionReg(11, 7)
  compReg := Mux(func === 0.U, Mux(io.pSumLoadFin, false.B, true.B), false.B) // TODO: check
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

trait EyerissDecoderIO extends Bundle {
  val pSumLoadFin: Bool = Input(Bool()) // when pSum load finish, then this will be input a true
}

class EyerissDecoder(c: EyerissDecoderParams)(implicit p: Parameters) extends TLRegisterRouter (
  c.address, "EyerissDecoder", Seq("SingularityKChen", "EyerissDecoder"), beatBytes = c.beatBytes)(
  new TLRegBundle(c, _) with EyerissDecoderIO)( new TLRegModule(c, _, _) with EyerissDecoderModule)
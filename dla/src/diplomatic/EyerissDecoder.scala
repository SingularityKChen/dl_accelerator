package dla.diplomatic

import chisel3._
import chisel3.util._

class AdrAndSizeIO extends Bundle {
  val starAdr: UInt = Output(UInt(5.W))
  val reqSize: UInt = Output(UInt(10.W))
}

trait HasPSumLoadEn extends Bundle {
  val pSumLoadEn: Bool = Output(Bool())
}

class DecoderIO extends Bundle {
  val instruction: UInt = Input(UInt(32.W))
  val calFin: Bool = Input(Bool()) // when pSum load finish, then this will be input a true
  val valid: Bool = Output(Bool())
  val doMacEn: Bool = Output(Bool())
  val inActIO = new AdrAndSizeIO
  val weightIO = new AdrAndSizeIO
  val pSumIO = new AdrAndSizeIO with HasPSumLoadEn
}

/** the decoder is used to decode instructions from CPU,
  * and when PSum computation finishes, io.valid is true, then it will be translated to interrupt.
  * Via this decoder, the dla can get three addresses, PSum load enable signals
  * and the size of inAct, weight and PSum
  * */
class EyerissDecoder extends Module {
  val io: DecoderIO = IO(new DecoderIO)
  private val func3 = Wire(UInt(3.W))
  private val rs1 = Wire(UInt(5.W))
  private val rd = Wire(UInt(5.W))
  private val imm = Wire(UInt(12.W))
  private val inActStrAdr = RegInit(0.U(5.W))
  private val weightStrAdr = RegInit(0.U(5.W))
  private val pSumStrAdr = RegInit(0.U(5.W))
  private val gnmfcsRegVec = Seq.fill(12) {
    RegInit(0.U(3.W))
  }
  private val fnercmRegVec = Seq.fill(6) {
    RegInit(0.U(3.W))
  }
  imm := io.instruction(31, 20)
  rs1 := io.instruction(19, 15)
  func3 := io.instruction(14, 12)
  rd := io.instruction(11, 7)
  private val pSumIdle :: pSumValid :: Nil = Enum(2)
  private val pSumWBStateReg = RegInit(pSumIdle)
  switch(pSumWBStateReg) {
    is (pSumIdle) {
      when (io.calFin) {
        pSumWBStateReg := pSumValid
      }
    }
    is (pSumValid) {
      pSumWBStateReg := pSumIdle
    }
  }
  io.valid := pSumWBStateReg === pSumValid
  switch(func3) {
    is(0.U) { // InAct and Weight Address, G2, N2, M2, F2
      inActStrAdr := rs1
      weightStrAdr := rd
      gnmfcsRegVec.head := imm(11, 9)
      gnmfcsRegVec(1) := imm(8, 6)
      gnmfcsRegVec(2) := imm(5, 3)
      gnmfcsRegVec(3) := imm(2, 0)
    }
    is(1.U) { // C2, S2, G1, N1
      gnmfcsRegVec(4) := imm(11, 9)
      gnmfcsRegVec(5) := imm(8, 6)
      gnmfcsRegVec(6) := imm(5, 3)
      gnmfcsRegVec(7) := imm(2, 0)
    }
    is(2.U) { // M1, F1, C1, S1
      gnmfcsRegVec(8) := imm(11, 9)
      gnmfcsRegVec(9) := imm(8, 6)
      gnmfcsRegVec(10) := imm(5, 3)
      gnmfcsRegVec(11) := imm(2, 0)
    }
    is(3.U) { // F0, N0, C0, M0
      fnercmRegVec.head := imm(11, 9)
      fnercmRegVec(1) := imm(8, 6)
      fnercmRegVec(4) := imm(5, 3)
      fnercmRegVec(5) := imm(2, 0)
      fnercmRegVec(2) := rs1
      fnercmRegVec(3) := rd
    }
    is(4.U) { //LoadPSum
      pSumStrAdr := rd
    }
  }
  // TODO: use the configurations via instructions rather than those configs
  io.inActIO.starAdr := inActStrAdr
  io.weightIO.starAdr := weightStrAdr
  io.pSumIO.starAdr := pSumStrAdr
  io.pSumIO.pSumLoadEn := func3 === 4.U // 100 TODO: maybe need one register
  io.doMacEn := func3 === 3.U
}
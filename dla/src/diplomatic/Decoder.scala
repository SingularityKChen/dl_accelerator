package dla.diplomatic

import chisel3._
import chisel3.util._

class DecoderIO extends Bundle {
  val instruction: UInt = Input(UInt(32.W))
  val pSumLoadFin: Bool = Input(Bool()) // when pSum load finish, then this will be input a true
  val pSumLoadEn: Bool = Output(Bool())
  val inActAddress: UInt = Output(UInt(5.W))
  val weightAddress: UInt = Output(UInt(5.W))
  val vaild: Bool = Output(Bool())
}

class Decoder extends Module {
  val io = IO(new DecoderIO)
  // if io.vaild = false, that's idle, then CPU sends one instruction, then io.vaild becomes true,
  // one means busy, and the dla will process this instruction.
  // After this instruction is done, then it becomes false again.
  // In this case, after configuration and get inAct and weight address, this reg will be assigned
  // to true, to show it is computing, and when it finishes, it will be false, and wait for read PSum.
  // When reading PSum, io.vaild will be assigned to true until it finishes write back.
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
  /** @todo 1 cycle. */
  io.vaild := Mux(func3 === 4.U, Mux(io.pSumLoadFin, false.B, true.B), false.B) // TODO: check
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
  io.inActAddress := inActStrAdr
  io.weightAddress := weightStrAdr
  io.pSumLoadEn := func3 === 4.U // 100
}
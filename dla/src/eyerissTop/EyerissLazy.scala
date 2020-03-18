package dla.eyerissTop

import chisel3._
import chisel3.util._
import dla.cluster.ClusterGroup
import freechips.rocketchip.subsystem.{BaseSubsystem, CacheBlockBytes}
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{HasRegMap, RegField, RegFieldDesc}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.UIntIsOneOf

trait HasPeripheryEyeriss { this: BaseSubsystem =>
  implicit val p: Parameters
  private val address = 0x2000
  private val portName = "eyeriss"
  val eyeriss: MyEyeriss = LazyModule(new MyEyeriss(EyerissDecoderParams(
    address = address,
    beatBytes = pbus.beatBytes))(p))
  pbus.toFixedWidthSlave(Some(portName)) {
    eyeriss.decoder.node
  }
  //mbus.toDRAMController()
}

class MyEyeriss(c: EyerissDecoderParams)(implicit p: Parameters) extends LazyModule {
  val decoder: EyerissDecoder = LazyModule(new EyerissDecoder(c))
  lazy val module = new MyEyerissImp(this)
}

class MyEyerissImp(outer: MyEyeriss)(implicit p: Parameters) extends LazyModuleImp(outer) {
  private val cGroup = Module(new ClusterGroup(false)).io // TODO: use EyerissTop
  private val decoder = outer.decoder.module.io
  cGroup.ctrlPath.readOutPSum := decoder.pSumLoadEn
  // cGroup data path
  // TODO: add ports to TL
  // cGroup ctrl path
  cGroup.ctrlPath.routerClusterCtrl.inActCtrlSel.inDataSel := 0.U // from inAct SRAM bank
  cGroup.ctrlPath.routerClusterCtrl.inActCtrlSel.outDataSel := 0.U // uni-cast
  cGroup.ctrlPath.routerClusterCtrl.weightCtrlSel.inDataSel := false.B // from GLB Cluster
  cGroup.ctrlPath.routerClusterCtrl.weightCtrlSel.outDataSel := false.B // don't send to its neighborhood
  cGroup.ctrlPath.routerClusterCtrl.pSumCtrlSel.inDataSel := true.B // from PSum SRAM bank
  cGroup.ctrlPath.routerClusterCtrl.pSumCtrlSel.outDataSel := true.B // send it to PE Array
  cGroup.ctrlPath.peClusterCtrl.inActSel.inDataSel := false.B // don't broad-cast inAct
  cGroup.ctrlPath.peClusterCtrl.inActSel.outDataSel := DontCare
  cGroup.ctrlPath.peClusterCtrl.pSumInSel := true.B // load PSum from Router
}

case class EyerissDecoderParams(address: BigInt, beatBytes: Int) // whether need beatBytes?

trait EyerissDecoderImp extends HasRegMap {
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
    0x00 -> Seq(RegField.w(n = instructionWidth, w = instructionReg,
      desc = new RegFieldDesc(name = "instructionReg", desc = "for CPU to write in instructions"))),
    0x08 -> Seq(RegField.r(n = 1, r = compReg,
      desc = new RegFieldDesc(name = "compReg", desc = "for CPU to know whether the dla finishes this instruction")))
  )
  private val func3 = Wire(UInt(3.W))
  private val rs1 = Wire(UInt(5.W))
  private val rd = Wire(UInt(5.W))
  private val imm = Wire(UInt(12.W))
  private val inActStrAdr = RegInit(0.U(5.W))
  private val weightStrAdr = RegInit(0.U(5.W))
  private val pSumStrAdr = RegInit(0.U(5.W))
  private val gnmfcsRegVec = Seq.fill(12){RegInit(0.U(3.W))}
  private val fnercmRegVec = Seq.fill(6){RegInit(0.U(3.W))}
  // TODO: use other instruction format rather than RoCC instruction format
  imm := instructionReg(31, 20)
  rs1 := instructionReg(19, 15)
  func3 := instructionReg(14, 12)
  rd := instructionReg(11, 7)
  compReg := Mux(func3 === 4.U, Mux(io.pSumLoadFin, false.B, true.B), false.B) // TODO: check
  switch(func3) {
    is (0.U) { // InAct and Weight Address, G2, N2, M2, F2
      inActStrAdr := rs1
      weightStrAdr := rd
      gnmfcsRegVec.head := imm(11, 9)
      gnmfcsRegVec(1) := imm(8, 6)
      gnmfcsRegVec(2) := imm(5, 3)
      gnmfcsRegVec(3) := imm(2, 0)
    }
    is (1.U) { // C2, S2, G1, N1
      gnmfcsRegVec(4) := imm(11, 9)
      gnmfcsRegVec(5) := imm(8, 6)
      gnmfcsRegVec(6) := imm(5, 3)
      gnmfcsRegVec(7) := imm(2, 0)
    }
    is (2.U) { // M1, F1, C1, S1
      gnmfcsRegVec(8) := imm(11, 9)
      gnmfcsRegVec(9) := imm(8, 6)
      gnmfcsRegVec(10) := imm(5, 3)
      gnmfcsRegVec(11) := imm(2, 0)
    }
    is (3.U) { // F0, N0, C0, M0
      fnercmRegVec.head := imm(11, 9)
      fnercmRegVec(1) := imm(8, 6)
      fnercmRegVec(4) := imm(5, 3)
      fnercmRegVec(5) := imm(2, 0)
      fnercmRegVec(2) := rs1
      fnercmRegVec(3) := rd
    }
    is (4.U) {//LoadPSum
      pSumStrAdr := rd
    }
  }
  // TODO: use the configurations via instructions rather than those configs
  io.inActAddress := inActStrAdr
  io.weightAddress := weightStrAdr
  io.pSumLoadEn := func3 === 4.U // 100
}

trait EyerissDecoderIO extends Bundle {
  val pSumLoadFin: Bool = Input(Bool()) // when pSum load finish, then this will be input a true
  val pSumLoadEn: Bool = Output(Bool())
  val inActAddress: UInt = Output(UInt(5.W))
  val weightAddress: UInt = Output(UInt(5.W))
}

class EyerissDecoder(c: EyerissDecoderParams)(implicit p: Parameters) extends TLRegisterRouter ( //
  base = c.address,
  devname = "EyerissDecoder",
  devcompat = Seq("SingularityKChen", "EyerissDecoder"),
  beatBytes = c.beatBytes)(
  new TLRegBundle(c, _) with EyerissDecoderIO)( new TLRegModule(c, _, _) with EyerissDecoderImp)
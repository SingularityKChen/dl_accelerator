package dla.diplomatic

import chisel3._
import dla.cluster.ClusterGroup
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts.HasInterruptSources
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegisterRouter, RegisterRouterParams}
import freechips.rocketchip.tilelink._

case class EyerissParams(address: BigInt, beatBytes: Int) // whether need beatBytes?

class LazyEyeriss(params: EyerissParams)(implicit p: Parameters) extends RegisterRouter(
  RegisterRouterParams(
    name = "eyeriss",
    compat = Seq("eyeriss"),
    base = params.address)
) with HasTLControlRegMap
  with HasInterruptSources {

  override def nInterrupts: Int = 1

  /** memory access node. */
  val node: TLClientNode = TLClientNode(
    portParams = Seq(
      TLClientPortParameters(
        clients = Seq(TLClientParameters(
          name = "EyerissReader",
          // @todo
          sourceId = IdRange(0, 1),
          // @todo
          supportsGet = TransferSizes(1, 4),
          // @todo
          supportsPutPartial = TransferSizes(1, 4) // use full or partial?
        )))))

  //@todo chisel logic
  // LazyModuleImp:
  lazy val module = new LazyModuleImp(this) {
    val cGroup = Module(new ClusterGroup(false)).io
    // 1. Decoder -> chiselModule
    val instructionWidth = 32
    /** if `compReg = false`, that's idle, then CPU sends one instruction, then compReg becomes true,
      * one means busy, and the dla will process this instruction.
      * After this instruction is done, then it becomes false again.
      * In this case, after configuration and get inAct and weight address, this reg will be assigned
      * to true, to show it is computing, and when it finishes, it will be false, and wait for read PSum.
      * When reading PSum, compReg will be assigned to true until it finishes write back.
      * */
    // store instructions from CPU TODO: check bits
    val instructionReg = RegInit(0.U(instructionWidth.W))
    regmap(
      0x00 -> Seq(RegField.w(n = instructionWidth, w = instructionReg, desc = RegFieldDesc(name = "instructionReg", desc = "for CPU to write in instructions"))),
    )
    val decoder = Module(new Decoder).io
    decoder.instruction := instructionReg

    // 2. TileLink access -> write/read memory.
    // (@todo DMA3)
    val (memBundle, memEdge) = node.out.head
    // 3. Int
    // (@todo add Int)
    val (intBundle, intEdge) = intnode.out.head

    // cGroup data path
    // @todo add ports to TL
    // @todo cGroup ctrl path
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
}
package dla.diplomatic

import chisel3._
import dla.cluster.ClusterGroup
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts.HasInterruptSources
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegisterRouter, RegisterRouterParams}
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.tilelink._

trait HasEyeriss { this: BaseSubsystem =>
  implicit val p: Parameters
  private val address = 0x2000 // address: 4 hex, 16-bits
  private val portName = "Eyeriss"
  val eyeriss: LazyEyeriss = LazyModule(new LazyEyeriss(EyerissParams(address, pbus.beatBytes))(p))
  /** attach control reg at periphery bus */
  pbus.coupleTo(name = portName) { eyeriss.controlXing(NoCrossing) := TLFragmenter(pbus) := _ }
  /** attach interrupt signal */
  ibus.fromSync := eyeriss.intXing(NoCrossing) // or use fromAsync
  /** attach EyerissSRAMNodes at memory bus*/
  mbus.coupleTo(name = "EyerissSRAMs") { eyeriss.memNode := TLFragmenter(mbus) := _} // := is read only, use :=* instead
}

case class EyerissParams(address: BigInt, beatBytes: Int)

class LazyEyeriss(params: EyerissParams)(implicit p: Parameters) extends RegisterRouter(
  RegisterRouterParams(
    name = "eyeriss",
    compat = Seq("eyeriss"),
    base = params.address)
) with HasTLControlRegMap
  with HasInterruptSources {

  override def nInterrupts: Int = 1
  /** functions [[eyerissPutNodeParameters]] and [[eyerissGetNodeParameters]] are TLClientParameters*/
  private def eyerissPutNodeParameters(sramName: String, sourceNum: Int) = Seq(TLClientParameters(
    /** write only */
    name = s"EyerissPSumSRAM$sramName",
    // TODO: change sourceID
    sourceId = IdRange(0, sourceNum),
    // @todo
    supportsPutFull = TransferSizes(1, 4) // avoid using partial to avoid mask
  ))
  private def eyerissGetNodeParameters(sramName: String, sourceNum: Int) = Seq(TLClientParameters(
    /** read only, for inAct and weight */
    name = s"Eyeriss$sramName",
    // TODO: change sourceID
    sourceId = IdRange(0, sourceNum),
    // TODO: check transferSizes
    supportsGet = TransferSizes(1, 4)
  ))
  /** memory access node. */
  val memNode: TLClientNode = TLClientNode(
    portParams = Seq(
      TLClientPortParameters(
        eyerissGetNodeParameters(sramName = "inActSRAM", sourceNum = 1))))
  // LazyModuleImp:
  lazy val module: LazyModuleImp = new LazyModuleImp(this) {
    val instructionWidth = 32
    /** store instructions from CPU */
    private val instructionReg = RegInit(0.U(instructionWidth.W))
    instructionReg.suggestName("instructionReg")
    regmap(
      0x00 -> Seq(RegField.w(n = instructionWidth, w = instructionReg, // offset: 2 hex
        desc = RegFieldDesc(name = "instructionReg", desc = "for CPU to write in instructions"))),
    )
    private val cGroup = Module(new ClusterGroup(false)).io
    /** Decoder */
    private val decoder = Module(new Decoder).io
    decoder.suggestName("decoderIO")
    // (@todo DMA3)
    // 2. TileLink access -> write/read memory.
    val (memBundle, memEdge) = memNode.out.head
    // 3. Int
    interrupts.head := decoder.valid
    /** decoder connections*/
    decoder.instruction := instructionReg
    decoder.calFin := cGroup.ctrlPath.calFin
    /** cGroup data path */
    /** cGroup ctrl path*/
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
package dla.diplomatic

import chisel3._
import dla.cluster.{ClusterConfig, ClusterGroup}
import dla.pe.PESizeConfig
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
  mbus.coupleTo(name = "EyerissSRAMs") { eyeriss.memGetNode := TLFragmenter(mbus) := _} // := is read only, use :=* instead
}

case class EyerissParams(address: BigInt, beatBytes: Int)

class LazyEyeriss(params: EyerissParams)(implicit p: Parameters) extends RegisterRouter(
  RegisterRouterParams(
    name = "eyeriss",
    compat = Seq("eyeriss"),
    base = params.address)
) with HasTLControlRegMap
  with HasInterruptSources
  with PESizeConfig with ClusterConfig {
  private val getSourceNum = inActRouterNum + weightRouterNum
  private val putSourceNum = pSumRouterNum

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
  val memGetNode: TLClientNode = TLClientNode(
    portParams = Seq(
      TLClientPortParameters(
        eyerissGetNodeParameters(sramName = "inActAndWeight", sourceNum = getSourceNum))))
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
    cGroup.suggestName("ClusterGroupIO")
    /** Decoder */
    private val decoder = Module(new Decoder).io
    decoder.suggestName("decoderIO")
    private val memCtrl = Module(new EyerissMemCtrlModule()(EyerissMemCtrlParameters(
      addressBits = params.address.bitLength, // TODO: check
      sizeBits = 16, // TODO
      dataBits = cscDataWidth,
      nIds = getSourceNum // TODO
    ))).io
    memCtrl.suggestName("EyerissMemCtrlIO")
    private val getSourceId = memCtrl.dataPath.sourceAlloc.bits
    private val getAddress = memCtrl.dataPath.address
    private val getSize = memCtrl.dataPath.size
    // (@todo DMA3)
    // 2. TileLink access -> write/read memory.
    val (memBundle, memEdge) = memGetNode.out.head
    val (getLegal, getBits) = memEdge.Get(getSourceId, getAddress, getSize)
    private val legalDest = memEdge.manager.containsSafe(getAddress)
    private val legal = legalDest && getLegal
    private val (getReqFirst, getReqLast, getReqDone) = memEdge.firstlast(memBundle.a)
    private val (getRespFirst, getRespLast, getRespDone) = memEdge.firstlast(memBundle.d)
    memBundle.a.valid := legal && (!getReqFirst || memCtrl.dataPath.sourceAlloc.valid)
    memCtrl.dataPath.sourceAlloc.ready := legal && getReqFirst && memBundle.a.ready
    memCtrl.dataPath.sourceFree.valid := getRespFirst && memBundle.d.fire()
    memCtrl.dataPath.sourceFree.bits := memBundle.d.bits.source
    memBundle.a.bits := getBits // TODO: check
    memBundle.b.ready := true.B // TODO: why?
    memBundle.c.valid := false.B
    memBundle.d.ready := true.B
    memBundle.e.valid := false.B
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
    //cGroup.ctrlPath.readOutPSum :=
    //cGroup.ctrlPath.doMacEn :=
  }
}
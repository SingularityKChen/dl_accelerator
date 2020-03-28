package dla.diplomatic

import chisel3._
import chisel3.util.log2Ceil
import dla.cluster.{ClusterConfig, ClusterGroup, ClusterSRAMConfig}
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
  mbus.coupleTo(name = "EyerissSRAMs") { bus =>
    eyeriss.memInActNode := TLFragmenter(mbus) := bus
    eyeriss.memWeightNode := TLFragmenter(mbus) := bus
    eyeriss.memPSumNode :=* TLFragmenter(mbus) :=* bus // TODO: check
  }
}

case class EyerissParams(address: BigInt, beatBytes: Int)

class LazyEyeriss(params: EyerissParams)(implicit p: Parameters) extends RegisterRouter(
  RegisterRouterParams(
    name = "eyeriss",
    compat = Seq("eyeriss"),
    base = params.address))
  with HasTLControlRegMap
  with HasInterruptSources
  with PESizeConfig with ClusterConfig with ClusterSRAMConfig {

  override def nInterrupts: Int = 1
  /** functions [[eyerissPutNodeParameters]] and [[eyerissGetNodeParameters]] are TLClientParameters*/
  private def eyerissPutNodeParameters(sourceNum: Int) = Seq(TLClientParameters(
    /** write only */
    name = s"EyerissPSumSRAM",
    // TODO: change sourceID
    sourceId = IdRange(0, sourceNum),
    // @todo
    // supportsGet = TransferSizes(1, 4),
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
  val memInActNode: TLClientNode = TLClientNode(
    portParams = Seq(
      TLClientPortParameters(
        eyerissGetNodeParameters(sramName = "inActSRAM", sourceNum = inActRouterNum))))
  val memWeightNode: TLClientNode = TLClientNode(
    portParams = Seq(
      TLClientPortParameters(
        eyerissGetNodeParameters(sramName = "weightSRAM", sourceNum = weightRouterNum))))
  val memPSumNode: TLClientNode = TLClientNode(
    portParams = Seq(
      TLClientPortParameters(
        eyerissPutNodeParameters(sourceNum = pSumRouterNum))))
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
      addressBits = memInActNode.out.head._2.manager.maxAddress.toInt, // TODO: check
      inActSizeBits = 10, // TODO: check
      weightSizeBits = 10,
      pSumSizeBits = log2Ceil(pSumSRAMSize),
      inActIds = inActRouterNum,
      weightIds = weightRouterNum,
      pSumIds = pSumRouterNum
    ))).io
    memCtrl.suggestName("EyerissMemCtrlIO")
    /** */
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
    /** */
    /** interrupts */
    interrupts.head := decoder.valid
    /** */
    /** decoder connections*/
    decoder.instruction := instructionReg
    decoder.calFin := cGroup.ctrlPath.calFin
    /** */
    /** memory get and put */
    private val getInActSourceId = memCtrl.inActIO.sourceAlloc.bits
    private val getInActAddress = memCtrl.inActIO.address
    private val getInActSize = 0.U // TODO: get from decoder
    private val getWeightSourceId = memCtrl.weightIO.sourceAlloc.bits
    private val getWeightAddress = memCtrl.weightIO.address
    private val getWeightSize = 0.U // TODO: get from decoder
    private val pSumSourceId = memCtrl.pSumIO.sourceAlloc.bits
    private val pSumAddress = memCtrl.pSumIO.address
    private val pSumSize = 0.U // TODO: get from decoder
    private val putPSumData = cGroup.dataPath.glbDataPath.pSumIO.head.outIOs.bits // TODO: add mux
    /** get bundles and edges from the three nodes*/
    val (memInActBundle, memInActEdge) = memInActNode.out.head
    val (memWeightBundle, memWeightEdge) = memWeightNode.out.head
    val (memPSumBundle, memPSumEdge) = memPSumNode.out.head
    /** create the necessary put and get signals */
    val (getInActLegal, getInActBits) = memInActEdge.Get(getInActSourceId, getInActAddress, getInActSize)
    val (getWeightLegal, getWeightBits) = memWeightEdge.Get(getWeightSourceId, getWeightAddress, getWeightSize)
    val (putPSumLegal, putPSumBits) = memPSumEdge.Put(pSumSourceId, pSumAddress, pSumSize, putPSumData)
    /** the logic of input activation */
    private val inActLegalDest = memInActEdge.manager.containsSafe(getInActAddress)
    private val inActLegal = inActLegalDest && getInActLegal
    private val (inActReqFirst, inActReqLast, inActReqDone) = memInActEdge.firstlast(memInActBundle.a)
    private val (inActRespFirst, inActRespLast, inActRespDone) = memInActEdge.firstlast(memInActBundle.d)
    memCtrl.inActIO.sourceAlloc.ready := inActLegal && inActReqFirst && memInActBundle.a.ready
    memCtrl.inActIO.sourceFree.valid := inActRespFirst && memInActBundle.d.fire()
    memCtrl.inActIO.sourceFree.bits := memInActBundle.d.bits.source
    memInActBundle.a.bits := getInActBits // TODO: check
    memInActBundle.a.valid := inActLegal && (!inActReqFirst || memCtrl.inActIO.sourceAlloc.valid)
    memInActBundle.d.ready := true.B
    /** the logic of weight */
    private val weightLegalDest = memWeightEdge.manager.containsSafe(getWeightAddress)
    private val weightLegal = weightLegalDest && getWeightLegal
    private val (weightReqFirst, weightReqLast, weightReqDone) = memWeightEdge.firstlast(memWeightBundle.a)
    private val (weightRespFirst, weightRespLast, weightRespDone) = memWeightEdge.firstlast(memWeightBundle.d)
    memCtrl.weightIO.sourceAlloc.ready := weightLegal && weightReqFirst && memWeightBundle.a.ready
    memCtrl.weightIO.sourceFree.valid := weightRespFirst && memWeightBundle.d.fire()
    memCtrl.weightIO.sourceFree.bits := memWeightBundle.d.bits.source
    memWeightBundle.a.bits := getWeightBits // TODO: check
    memWeightBundle.a.valid := weightLegal && (!weightReqFirst || memCtrl.weightIO.sourceAlloc.valid)
    memWeightBundle.d.ready := true.B
    /** the logic of partial sum */
    private val pSumLegalDest = memPSumEdge.manager.containsSafe(pSumAddress)
    private val pSumLegal = pSumLegalDest && putPSumLegal
    private val (pSumReqFirst, pSumReqLast, pSumReqDone) = memPSumEdge.firstlast(memPSumBundle.a)
    private val (pSumRespFirst, pSumRespLast, pSumRespDone) = memPSumEdge.firstlast(memPSumBundle.d)
    memCtrl.pSumIO.sourceAlloc.ready := pSumLegal && pSumReqFirst && memPSumBundle.a.ready
    memCtrl.pSumIO.sourceFree.valid := pSumRespFirst && memPSumBundle.d.fire()
    memCtrl.pSumIO.sourceFree.bits := memPSumBundle.d.bits.source
    memPSumBundle.a.bits := getWeightBits // TODO: check
    memPSumBundle.a.valid := pSumLegal && (!pSumReqFirst || memCtrl.pSumIO.sourceAlloc.valid)
    memPSumBundle.d.ready := true.B
    /** tie off unused channels */
    memInActBundle.b.ready := false.B
    memInActBundle.c.valid := false.B
    memInActBundle.e.valid := false.B
    memWeightBundle.b.ready := false.B
    memWeightBundle.c.valid := false.B
    memWeightBundle.e.valid := false.B
    memPSumBundle.b.ready := false.B
    memPSumBundle.c.valid := false.B
    memPSumBundle.e.valid := false.B
    /** */
    /** cGroup data path*/
    // TODO: add csc switcher here
    //XXX := memInActBundle.d.bits
    private val inActInIOs = cGroup.dataPath.glbDataPath.inActIO.map(x => x.inIOs)
    private val weightInIOs = cGroup.dataPath.glbDataPath.weightIO.map(x => x.inIOs)
    private val pSumOutIOs = cGroup.dataPath.glbDataPath.pSumIO.map(x => x.outIOs)
  }
}
package dla.diplomatic

import chisel3._
import chisel3.util.{DecoupledIO, MuxLookup, log2Ceil}
import dla.ClusterGroupWrapper
import dla.cluster.{ClusterConfig, ClusterSRAMConfig}
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
  private def eyerissPutNodeParameters(sourceNum: Int) = Seq(TLMasterParameters.v1(
    /** write only */
    name = s"EyerissPSumSRAM",
    sourceId = IdRange(0, sourceNum),
    // @todo
    // supportsGet = TransferSizes(1, 4),
    //supportsPutFull = TransferSizes(1, 4) // avoid using partial to avoid mask
  ))
  private def eyerissGetNodeParameters(sramName: String, sourceNum: Int) = Seq(TLMasterParameters.v1(
    /** read only, for inAct and weight */
    name = s"Eyeriss$sramName",
    sourceId = IdRange(0, sourceNum),
    //supportsGet = TransferSizes(1, 4)
  ))
  /** memory access node. */
  val memInActNode: TLClientNode = TLClientNode(
    portParams = Seq(
      TLMasterPortParameters.v1(
        eyerissGetNodeParameters(sramName = "inActSRAM", sourceNum = inActRouterNum))))
  val memWeightNode: TLClientNode = TLClientNode(
    portParams = Seq(
      TLMasterPortParameters.v1(
        eyerissGetNodeParameters(sramName = "weightSRAM", sourceNum = weightRouterNum))))
  val memPSumNode: TLClientNode = TLClientNode(
    portParams = Seq(
      TLMasterPortParameters.v1(
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
    private val cGroup = Module(new ClusterGroupWrapper)
    cGroup.suggestName("ClusterGroupWrapper")
    private val cGroupIO = cGroup.io
    /** Decoder */
    private val decoder = Module(new EyerissDecoder)
    decoder.suggestName("decoderModule")
    private val decoderIO = decoder.io
    private val memCtrl = Module(new EyerissMemCtrlModule()(EyerissMemCtrlParameters(
      addressBits = memInActNode.out.head._2.manager.maxAddress.toInt, // TODO: check
      inActSizeBits = 12, // TODO: check
      weightSizeBits = 12,
      pSumSizeBits = log2Ceil(pSumSRAMSize),
      inActIds = inActRouterNum,
      weightIds = weightRouterNum,
      pSumIds = pSumRouterNum
    )))
    memCtrl.suggestName("EyerissMemCtrlModule")
    private val memCtrlIO = memCtrl.io
    /** */
    /** cGroupIO ctrl path*/
    private val cgCtrlPath = cGroupIO.ctrlPath.cgCtrlPath
    cgCtrlPath.routerClusterCtrl.inActCtrlSel.inDataSel := 0.U // from inAct SRAM bank
    cgCtrlPath.routerClusterCtrl.inActCtrlSel.outDataSel := 0.U // uni-cast
    cgCtrlPath.routerClusterCtrl.weightCtrlSel.inDataSel := false.B // from GLB Cluster
    cgCtrlPath.routerClusterCtrl.weightCtrlSel.outDataSel := false.B // don't send to its neighborhood
    cgCtrlPath.routerClusterCtrl.pSumCtrlSel.inDataSel := true.B // from PSum SRAM bank
    cgCtrlPath.routerClusterCtrl.pSumCtrlSel.outDataSel := true.B // send it to PE Array
    cgCtrlPath.peClusterCtrl.inActSel.inDataSel := false.B // don't broad-cast inAct
    cgCtrlPath.peClusterCtrl.inActSel.outDataSel := DontCare
    cgCtrlPath.peClusterCtrl.pSumInSel := true.B // load PSum from Router
    cgCtrlPath.readOutPSum := decoderIO.pSumIO.pSumLoadEn
    cgCtrlPath.doMacEn := decoderIO.doMacEn
    /** */
    /** interrupts */
    interrupts.head := decoderIO.valid
    /** */
    /** decoderIO connections*/
    decoderIO.instruction := instructionReg
    decoderIO.calFin := cgCtrlPath.calFin
    /** */
    /** memory module address and size */
    memCtrlIO.inActIO.startAdr := decoderIO.inActIO.starAdr
    memCtrlIO.inActIO.reqSize := decoderIO.inActIO.reqSize
    memCtrlIO.weightIO.startAdr := decoderIO.weightIO.starAdr
    memCtrlIO.weightIO.reqSize := decoderIO.weightIO.reqSize
    memCtrlIO.pSumIO.startAdr := decoderIO.pSumIO.starAdr
    memCtrlIO.pSumIO.reqSize := decoderIO.pSumIO.reqSize
    /** memory get and put */
    private val getInActSourceId = memCtrlIO.inActIO.sourceAlloc.bits
    private val getInActAddress = memCtrlIO.inActIO.address
    private val getInActSize = decoderIO.inActIO.reqSize
    private val getWeightSourceId = memCtrlIO.weightIO.sourceAlloc.bits
    private val getWeightAddress = memCtrlIO.weightIO.address
    private val getWeightSize = decoderIO.weightIO.reqSize
    private val pSumSourceId = memCtrlIO.pSumIO.sourceAlloc.bits
    private val pSumAddress = memCtrlIO.pSumIO.address
    private val pSumSize = decoderIO.pSumIO.reqSize
    private val putPSumData = cGroupIO.dataPath.pSumIO.head.outIOs.bits // FIXME: head
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
    memCtrlIO.inActIO.sourceAlloc.ready := inActLegal && inActReqFirst && memInActBundle.a.ready && cgCtrlPath.glbLoadEn
    memCtrlIO.inActIO.sourceFree.valid := inActRespFirst && memInActBundle.d.fire()
    memCtrlIO.inActIO.sourceFree.bits := memInActBundle.d.bits.source
    memInActBundle.a.bits := getInActBits // TODO: check
    memInActBundle.a.valid := inActLegal && (!inActReqFirst || memCtrlIO.inActIO.sourceAlloc.valid && cgCtrlPath.glbLoadEn)
    memInActBundle.d.ready := true.B
    /** the logic of weight */
    private val weightLegalDest = memWeightEdge.manager.containsSafe(getWeightAddress)
    private val weightLegal = weightLegalDest && getWeightLegal
    private val (weightReqFirst, weightReqLast, weightReqDone) = memWeightEdge.firstlast(memWeightBundle.a)
    private val (weightRespFirst, weightRespLast, weightRespDone) = memWeightEdge.firstlast(memWeightBundle.d)
    memCtrlIO.weightIO.sourceAlloc.ready := weightLegal && weightReqFirst &&
      memWeightBundle.a.ready && cgCtrlPath.peLoadEn
    memCtrlIO.weightIO.sourceFree.valid := weightRespFirst && memWeightBundle.d.fire()
    memCtrlIO.weightIO.sourceFree.bits := memWeightBundle.d.bits.source
    memWeightBundle.a.bits := getWeightBits // TODO: check
    memWeightBundle.a.valid := weightLegal &&
      (!weightReqFirst || memCtrlIO.weightIO.sourceAlloc.valid && cgCtrlPath.peLoadEn)
    memWeightBundle.d.ready := true.B
    /** the logic of partial sum */
    private val pSumLegalDest = memPSumEdge.manager.containsSafe(pSumAddress)
    private val pSumLegal = pSumLegalDest && putPSumLegal
    private val (pSumReqFirst, pSumReqLast, pSumReqDone) = memPSumEdge.firstlast(memPSumBundle.a)
    private val (pSumRespFirst, pSumRespLast, pSumRespDone) = memPSumEdge.firstlast(memPSumBundle.d)
    private val pSumCGPutDataValid = Wire(Bool())
    memCtrlIO.pSumIO.sourceAlloc.ready := pSumLegal && pSumReqFirst && memPSumBundle.a.ready
    memCtrlIO.pSumIO.sourceFree.valid := pSumRespFirst && memPSumBundle.d.fire()
    memCtrlIO.pSumIO.sourceFree.bits := memPSumBundle.d.bits.source
    memPSumBundle.a.bits := getWeightBits // TODO: check
    memPSumBundle.a.valid := pSumLegal && ((!pSumReqFirst && pSumCGPutDataValid) || memCtrlIO.pSumIO.sourceAlloc.valid)
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
    /** cGroupIO data path*/
    def sourceDataMux(offChip: DecoupledIO[TLBundleD], onChip: Seq[DecoupledIO[UInt]]) : Unit = {
      onChip.zipWithIndex.foreach({ case (value, i) =>
        value.bits := offChip.bits.data
        value.valid := offChip.bits.source === i.U && offChip.valid
      })
      offChip.ready := MuxLookup(offChip.bits.source, false.B, onChip.zipWithIndex.map({ case (value, i) =>
        i.U -> value.ready
      }))
    }
    private val inActInIOs = cGroupIO.dataPath.inActIO.map(x => x.data)
    sourceDataMux(offChip = memInActBundle.d, onChip = inActInIOs)
    private val weightInIOs = cGroupIO.dataPath.weightIO.map(x => x.data)
    sourceDataMux(offChip = memWeightBundle.d, onChip = weightInIOs)
    private val pSumOutIOs = cGroupIO.dataPath.pSumIO.map(x => x.outIOs)
    pSumCGPutDataValid := pSumOutIOs.head.valid // FIXME: head
    pSumOutIOs.head.ready := memPSumBundle.a.ready
  }
}
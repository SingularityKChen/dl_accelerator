package dla.diplomatic

import chisel3._
import chisel3.util._
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
  val xbar = LazyModule(new TLXbar()(p)).node
  cbus.coupleTo(name = portName) { eyeriss.controlXing(NoCrossing) := TLFragmenter(cbus) := _ }
  /** attach interrupt signal */
  ibus.fromSync := eyeriss.intXing(NoCrossing) // or use fromAsync
  /** rocket<->InclusiveCache */
  /** rocket->xbar->
    * eyeriss */
  sbus.coupleTo(name = "EyerissSRAMs") { bus =>
    eyeriss.memInActNode := xbar
    eyeriss.memWeightNode := xbar
    eyeriss.memPSumNode := xbar
    xbar := bus
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
    private val eyerissTop = Module(new EyerissTop(EyerissTopParam(
      addressBits = memInActNode.out.head._2.manager.maxAddress.toInt,
      inActDataBits = memInActNode.out.head._1.params.dataBits,
      inActSourceBits = memInActNode.out.head._1.params.sourceBits,
      weightDataBits = memWeightNode.out.head._1.params.dataBits,
      weightSourceBits = memWeightNode.out.head._1.params.sourceBits,
      pSumDataBits = memPSumNode.out.head._1.params.dataBits,
      pSumSourceBits = memPSumNode.out.head._1.params.sourceBits
    )))
    /** */
    private val eyerissTopIO = eyerissTop.io
    /** */
    /** interrupts */
    interrupts.head := eyerissTopIO.ctrlPath.interrupts
    /** instructions */
    eyerissTopIO.ctrlPath.instructions := instructionReg
    /** memory get and put */
    /** the logic of input activation */
    private val getInActSourceId = eyerissTopIO.ctrlPath.bundles.memInActBundles.a.bits.source
    private val getInActAddress = eyerissTopIO.ctrlPath.bundles.memInActBundles.address
    private val getInActSize = eyerissTopIO.ctrlPath.bundles.memInActBundles.reqSize
    val (memInActBundle, memInActEdge) = memInActNode.out.head
    private val inActLegalDest = memInActEdge.manager.containsSafe(getInActAddress)
    val (getInActLegal, getInActBits) = memInActEdge.Get(getInActSourceId, getInActAddress, getInActSize)
    private val inActLegal = inActLegalDest && getInActLegal
    private val (inActReqFirst, inActReqLast, inActReqDone) = memInActEdge.firstlast(memInActBundle.a)
    private val (inActRespFirst, inActRespLast, inActRespDone) = memInActEdge.firstlast(memInActBundle.d)
    memInActBundle.a.bits := getInActBits // TODO: check
    memInActBundle.a.valid := eyerissTopIO.ctrlPath.bundles.memInActBundles.a.valid
    memInActBundle.d.ready := true.B
    eyerissTopIO.ctrlPath.bundles.memInActBundles.a.ready := memInActBundle.a.ready
    eyerissTopIO.ctrlPath.bundles.memInActBundles.d.valid := memInActBundle.d.valid
    eyerissTopIO.ctrlPath.bundles.memInActBundles.d.bits.source := memInActBundle.d.bits.source
    eyerissTopIO.ctrlPath.bundles.memInActBundles.d.bits.data := memInActBundle.d.bits.data
    eyerissTopIO.ctrlPath.bundles.memInActBundles.reqFirst := inActReqFirst
    eyerissTopIO.ctrlPath.bundles.memInActBundles.respFirst := inActRespFirst
    eyerissTopIO.ctrlPath.bundles.memInActBundles.legal := inActLegal
    /** the logic of weight */
    private val getWeightSourceId = eyerissTopIO.ctrlPath.bundles.memWeightBundles.a.bits.source
    private val getWeightAddress = eyerissTopIO.ctrlPath.bundles.memWeightBundles.address
    private val getWeightSize = eyerissTopIO.ctrlPath.bundles.memWeightBundles.reqSize
    val (memWeightBundle, memWeightEdge) = memWeightNode.out.head
    private val weightLegalDest = memWeightEdge.manager.containsSafe(getWeightAddress)
    val (getWeightLegal, getWeightBits) = memWeightEdge.Get(getWeightSourceId, getWeightAddress, getWeightSize)
    private val weightLegal = weightLegalDest && getWeightLegal
    private val (weightReqFirst, weightReqLast, weightReqDone) = memWeightEdge.firstlast(memWeightBundle.a)
    private val (weightRespFirst, weightRespLast, weightRespDone) = memWeightEdge.firstlast(memWeightBundle.d)
    memWeightBundle.a.bits := getWeightBits
    memWeightBundle.a.valid := eyerissTopIO.ctrlPath.bundles.memWeightBundles.a.valid
    memWeightBundle.d.ready := true.B
    eyerissTopIO.ctrlPath.bundles.memWeightBundles.a.ready := memWeightBundle.a.ready
    eyerissTopIO.ctrlPath.bundles.memWeightBundles.d.valid := memWeightBundle.d.valid
    eyerissTopIO.ctrlPath.bundles.memWeightBundles.d.bits.source := memWeightBundle.d.bits.source
    eyerissTopIO.ctrlPath.bundles.memWeightBundles.d.bits.data := memWeightBundle.d.bits.data
    eyerissTopIO.ctrlPath.bundles.memWeightBundles.reqFirst := weightReqFirst
    eyerissTopIO.ctrlPath.bundles.memWeightBundles.legal := weightLegal
    eyerissTopIO.ctrlPath.bundles.memWeightBundles.respFirst := weightRespFirst
    /** the logic of partial sum */
    private val pSumSourceId = eyerissTopIO.ctrlPath.bundles.memPSumBundles.a.bits.source
    private val pSumAddress = eyerissTopIO.ctrlPath.bundles.memPSumBundles.address
    private val pSumSize = eyerissTopIO.ctrlPath.bundles.memPSumBundles.reqSize
    private val putPSumData = eyerissTopIO.ctrlPath.bundles.memPSumBundles.a.bits.data
    val (memPSumBundle, memPSumEdge) = memPSumNode.out.head
    private val pSumLegalDest = memPSumEdge.manager.containsSafe(pSumAddress)
    val (putPSumLegal, putPSumBits) = memPSumEdge.Put(pSumSourceId, pSumAddress, pSumSize, putPSumData)
    private val pSumLegal = pSumLegalDest && putPSumLegal
    private val (pSumReqFirst, pSumReqLast, pSumReqDone) = memPSumEdge.firstlast(memPSumBundle.a)
    private val (pSumRespFirst, pSumRespLast, pSumRespDone) = memPSumEdge.firstlast(memPSumBundle.d)
    memPSumBundle.a.bits := putPSumBits
    memPSumBundle.a.valid := eyerissTopIO.ctrlPath.bundles.memPSumBundles.a.valid
    memPSumBundle.d.ready := true.B
    eyerissTopIO.ctrlPath.bundles.memPSumBundles.a.ready := memPSumBundle.a.ready
    eyerissTopIO.ctrlPath.bundles.memPSumBundles.d.valid := memPSumBundle.d.valid
    eyerissTopIO.ctrlPath.bundles.memPSumBundles.d.bits.source := memPSumBundle.d.bits.source
    eyerissTopIO.ctrlPath.bundles.memPSumBundles.d.bits.data := DontCare // will not use this data
    eyerissTopIO.ctrlPath.bundles.memPSumBundles.reqFirst := pSumReqFirst
    eyerissTopIO.ctrlPath.bundles.memPSumBundles.respFirst := pSumRespFirst
    eyerissTopIO.ctrlPath.bundles.memPSumBundles.legal := pSumLegal
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
  }
}

case class EyerissTopParam (
                           addressBits: Int,
                           inActDataBits: Int,
                           inActSourceBits: Int,
                           weightDataBits: Int,
                           weightSourceBits: Int,
                           pSumDataBits: Int,
                           pSumSourceBits: Int
                           )

class SimpleTLDIO(val dataBits: Int, val sourceBits: Int) extends Bundle {
  val data: UInt = UInt(dataBits.W)
  val source: UInt = UInt(sourceBits.W)
}

class EyerissMemCtrlBundle(val dataBits: Int, val sourceBits: Int)(implicit val addressBits: Int) extends Bundle {
  val reqSize: UInt = Output(UInt(12.W))
  val address: UInt = Output(UInt(addressBits.W))
  val reqFirst: Bool = Input(Bool())
  val respFirst: Bool = Input(Bool())
  val legal: Bool = Input(Bool())
  val a: DecoupledIO[SimpleTLDIO] = Decoupled(new SimpleTLDIO(dataBits, sourceBits))
  val d: DecoupledIO[SimpleTLDIO] = Flipped(Decoupled(new SimpleTLDIO(dataBits, sourceBits)))
}

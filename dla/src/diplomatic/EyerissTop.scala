package dla.diplomatic

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import dla.cluster.{ClusterConfig, ClusterSRAMConfig}
import dla.eyerissWrapper.ClusterGroupWrapper
import firrtl.options.TargetDirAnnotation

class EyerissTop(val param: EyerissTopParam) extends Module with ClusterConfig with ClusterSRAMConfig {
  private implicit val addressBits: Int = param.addressBits
  val io = IO(new Bundle {
    val ctrlPath = new Bundle {
      val interrupts: Bool = Output(Bool())
      val instructions: UInt = Input(UInt(32.W))
      val bundles = new Bundle {
        val memInActBundles = new EyerissMemCtrlBundle(dataBits = param.inActDataBits, sourceBits = param.inActSourceBits)
        val memWeightBundles = new EyerissMemCtrlBundle(dataBits = param.weightDataBits, sourceBits = param.weightSourceBits)
        val memPSumBundles = new EyerissMemCtrlBundle(dataBits = param.pSumDataBits, sourceBits = param.pSumSourceBits)
      }
    }
  })
  private val cGroup = Module(new ClusterGroupWrapper)
  cGroup.suggestName("ClusterGroupWrapper")
  private val cGroupIO = cGroup.io
  /** Decoder */
  private val decoder = Module(new EyerissDecoder)
  decoder.suggestName("decoderModule")
  private val decoderIO = decoder.io
  private val memCtrl = Module(new EyerissMemCtrlModule()(EyerissMemCtrlParameters(
    addressBits = param.addressBits, // TODO: check
    inActSizeBits = 12, // TODO: check
    weightSizeBits = 12,
    pSumSizeBits = log2Ceil(pSumSRAMSize),
    inActIds = inActRouterNum,
    weightIds = weightRouterNum,
    pSumIds = pSumRouterNum
  )))
  memCtrl.suggestName("EyerissMemCtrlModule")
  private val memCtrlIO = memCtrl.io
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
  cGroupIO.ctrlPath.cscSwitcherCtrlPath <> decoderIO.cscSwitcherCtrlIO
  /** cGroupIO data path*/
  cGroupIO.dataPath.pSumIO.foreach(x => x.inIOs <> DontCare)
  private def sourceInputDataMux(offChip: DecoupledIO[SimpleTLDIO], onChip: Seq[DecoupledIO[UInt]]) : Unit = {
    onChip.zipWithIndex.foreach({ case (value, i) =>
      value.bits := offChip.bits.data
      value.valid := offChip.bits.source === i.U && offChip.valid
    })
    offChip.ready := MuxLookup(offChip.bits.source, false.B, onChip.zipWithIndex.map({ case (value, i) =>
      i.U -> value.ready
    }))
  }

  private def pSumBundleMux(offChip: DecoupledIO[SimpleTLDIO], onChip: Seq[DecoupledIO[UInt]]) : Unit = {
    val source = memCtrlIO.pSumIO.sourceAlloc.bits
    onChip.zipWithIndex.foreach({ case (value, i) =>
      value.ready := source === i.U && offChip.ready
    })
    val outDataValid = MuxLookup(source, false.B, onChip.zipWithIndex.map({ case (value, i) =>
      i.U -> value.valid
    }))
    offChip.bits.data := MuxLookup(source, false.B, onChip.zipWithIndex.map({ case (value, i) =>
      i.U -> value.bits
    }))
    offChip.bits.source := source
    offChip.valid := io.ctrlPath.bundles.memPSumBundles.legal &&
      ((!io.ctrlPath.bundles.memPSumBundles.reqFirst && outDataValid) ||
        memCtrlIO.pSumIO.sourceAlloc.valid && decoderIO.pSumIO.pSumLoadEn)
  }
  private val inActInIOs = cGroupIO.dataPath.inActIO.map(x => x.data)
  sourceInputDataMux(offChip = io.ctrlPath.bundles.memInActBundles.d, onChip = inActInIOs)
  private val weightInIOs = cGroupIO.dataPath.weightIO.map(x => x.data)
  sourceInputDataMux(offChip = io.ctrlPath.bundles.memWeightBundles.d, onChip = weightInIOs)
  private val pSumOutIOs = cGroupIO.dataPath.pSumIO.map(x => x.outIOs)
  pSumBundleMux(offChip = io.ctrlPath.bundles.memPSumBundles.a, onChip = pSumOutIOs)
  /** memory module address and size */
  memCtrlIO.inActIO.startAdr := decoderIO.inActIO.starAdr
  memCtrlIO.inActIO.reqSize := decoderIO.inActIO.reqSize
  memCtrlIO.weightIO.startAdr := decoderIO.weightIO.starAdr
  memCtrlIO.weightIO.reqSize := decoderIO.weightIO.reqSize
  memCtrlIO.pSumIO.startAdr := decoderIO.pSumIO.starAdr
  memCtrlIO.pSumIO.reqSize := decoderIO.pSumIO.reqSize
  /** only glbLoadEn, then generate source id*/
  memCtrlIO.inActIO.sourceAlloc.ready := io.ctrlPath.bundles.memInActBundles.legal &&
    io.ctrlPath.bundles.memInActBundles.reqFirst &&
    io.ctrlPath.bundles.memInActBundles.a.ready && cgCtrlPath.glbInActLoadEn
  memCtrlIO.inActIO.sourceFree.valid := io.ctrlPath.bundles.memInActBundles.respFirst &&
    io.ctrlPath.bundles.memInActBundles.d.fire()
  memCtrlIO.inActIO.sourceFree.bits := io.ctrlPath.bundles.memInActBundles.d.bits.source
  /** only peLoadEn and haven't finish read (sourceAlloc.valid), then generate source id*/
  memCtrlIO.weightIO.sourceAlloc.ready := io.ctrlPath.bundles.memWeightBundles.legal &&
    io.ctrlPath.bundles.memWeightBundles.reqFirst &&
    io.ctrlPath.bundles.memWeightBundles.a.ready && cgCtrlPath.peWeightLoadEn
  memCtrlIO.weightIO.sourceFree.valid := io.ctrlPath.bundles.memWeightBundles.respFirst &&
    io.ctrlPath.bundles.memWeightBundles.d.fire()
  memCtrlIO.weightIO.sourceFree.bits := io.ctrlPath.bundles.memWeightBundles.d.bits.source
  /** only pSumLoadEn, then generate source id*/
  memCtrlIO.pSumIO.sourceAlloc.ready := io.ctrlPath.bundles.memPSumBundles.legal &&
    io.ctrlPath.bundles.memPSumBundles.reqFirst &&
    io.ctrlPath.bundles.memPSumBundles.a.ready && decoderIO.pSumIO.pSumLoadEn
  memCtrlIO.pSumIO.sourceFree.valid := io.ctrlPath.bundles.memPSumBundles.respFirst &&
    io.ctrlPath.bundles.memPSumBundles.d.fire()
  memCtrlIO.pSumIO.sourceFree.bits := io.ctrlPath.bundles.memPSumBundles.d.bits.source
  /** decoder */
  decoderIO.instruction := io.ctrlPath.instructions
  decoderIO.calFin := cgCtrlPath.calFin
  /** output*/
  io.ctrlPath.interrupts := decoderIO.valid
  /** only glbLoadEn then a.valid is true then can Get data */
  io.ctrlPath.bundles.memInActBundles.a.valid := io.ctrlPath.bundles.memInActBundles.legal &&
    (!io.ctrlPath.bundles.memInActBundles.reqFirst || memCtrlIO.inActIO.sourceAlloc.valid && cgCtrlPath.glbInActLoadEn)
  io.ctrlPath.bundles.memInActBundles.a.bits.source := memCtrlIO.inActIO.sourceAlloc.bits
  /** Don't care memInActBundles.a.bits.data as memInActBundle.a.bits.data will be assigned in LazyImp*/
  io.ctrlPath.bundles.memInActBundles.a.bits.data := DontCare
  io.ctrlPath.bundles.memInActBundles.address := memCtrlIO.inActIO.address
  io.ctrlPath.bundles.memInActBundles.reqSize := decoderIO.inActIO.reqSize
  /** weight */
  io.ctrlPath.bundles.memWeightBundles.a.valid := io.ctrlPath.bundles.memWeightBundles.legal &&
    (!io.ctrlPath.bundles.memWeightBundles.reqFirst || (memCtrlIO.weightIO.sourceAlloc.valid && cgCtrlPath.peWeightLoadEn))
  io.ctrlPath.bundles.memWeightBundles.a.bits.source := memCtrlIO.weightIO.sourceAlloc.bits
  /** Don't care memWeightBundles.a.bits.data as memWeightBundle.a.bits.data will be assigned in LazyImp*/
  io.ctrlPath.bundles.memWeightBundles.a.bits.data := DontCare
  io.ctrlPath.bundles.memWeightBundles.address := memCtrlIO.weightIO.address
  io.ctrlPath.bundles.memWeightBundles.reqSize := decoderIO.weightIO.reqSize
  /** only pSumLoadEn then a.valid is true then can Put */
  io.ctrlPath.bundles.memPSumBundles.address := memCtrlIO.pSumIO.address
  io.ctrlPath.bundles.memPSumBundles.reqSize := decoderIO.pSumIO.reqSize
  io.ctrlPath.bundles.memPSumBundles.d.ready := true.B // always ready to receive Put response
}
object GenEyerissTop extends App {
  (new ChiselStage).run(Seq(
    ChiselGeneratorAnnotation(() => new EyerissTop(EyerissTopParam(
      addressBits = 32,
      inActDataBits = 32,
      inActSourceBits = 3,
      weightDataBits = 32,
      weightSourceBits = 3,
      pSumDataBits = 32,
      pSumSourceBits = 3
    ))),
    TargetDirAnnotation(directory = "test_run_dir/EyerissTop")
  ))
}

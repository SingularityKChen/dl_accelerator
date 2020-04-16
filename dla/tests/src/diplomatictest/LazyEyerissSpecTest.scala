package dla.tests.diplomatictest

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import chiseltest.internal.WriteVcdAnnotation
import diplomatictester._
import diplomatictester.TLEdgeLit._
import dla.diplomatic.{EyerissParams, LazyEyeriss}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

abstract class TLEyerissWrapperBasic(implicit p: Parameters) extends LazyModule {
  val ram: TLRAM = LazyModule(new TLRAM(AddressSet(0x100, 0xff)))
  val ram2: TLRAM = LazyModule(new TLRAM(AddressSet(0, 0xff), beatBytes = 16))
  val xbar: TLXbar = LazyModule(new TLXbar)
  val eyeriss: LazyEyeriss = LazyModule(new LazyEyeriss(params = EyerissParams(
    address = 0x10013000, // TODO: change
    beatBytes = 10
  ))(p))

  xbar.node := eyeriss.memInActNode
  xbar.node := eyeriss.memPSumNode
  ram2.node := TLFragmenter(16, 256) := xbar.node
  ram.node := TLFragmenter(4, 256) := TLWidthWidget(16) := xbar.node

}

class TLEyerissWrapperTLTest(implicit p: Parameters) extends TLEyerissWrapperBasic {
  lazy val module = new LazyModuleImp(this) {
    val monitor: AutoBundle = mockIO(eyeriss.module.auto, "eyerissTLMonitor")
  }
}

class TLEyerissWrapperDutTest(implicit p: Parameters) extends TLEyerissWrapperBasic {
  lazy val module = new LazyModuleImp(this) {
    dutModule(eyeriss.module)
    val eyerissIO: AutoBundle = dutIO(eyeriss.module.auto, "eyerissAuto")
  }
}

object LazyEyerissTLTest extends App {
  implicit val p: Parameters = Parameters((site, here, up) => {
    case MonitorsEnabled => false
  })
  val eyerissWrapper = LazyModule(new TLEyerissWrapperTLTest()(p))
  RawTester.test(eyerissWrapper.module, Seq(WriteVcdAnnotation)) {
    dut =>
      val inActEdges: Edges[TLEdgeIn, TLEdgeOut] = eyerissWrapper.eyeriss.memInActNode.edges
      val pSumEdges: Edges[TLEdgeIn, TLEdgeOut] = eyerissWrapper.eyeriss.memPSumNode.edges
      val inActOutEdge = inActEdges.out.head
      val pSumOutEdge = pSumEdges.out.head
      val size = 1
      val mask = 0xff
      val source = 0
      val address = 0x100 // to ram
      val data = 0xff
      dut.clock.setTimeout(0)
      dut.clock.step()
      /** write 0xff into RAM via PSum edge*/
      // FIXME: correct the monitor's bundle (such as memInActBundle, memPSumBundle)
      dut.monitor.pokePartial(chiselTypeOf(dut.monitor).Lit(
        _.elements("out").asInstanceOf[TLBundle].a.bits -> pSumOutEdge.PutFullData(size, source, address, mask, corrupt = false, data),
        _.elements("out").asInstanceOf[TLBundle].a.valid -> true.B,
        _.elements("out").asInstanceOf[TLBundle].d.ready -> true.B
      ))
      dut.clock.step()
      dut.monitor.pokePartial(chiselTypeOf(dut.monitor).Lit(
        _.elements("out").asInstanceOf[TLBundle].a.valid -> false.B
      ))
      dut.clock.step(5)
      /** read 0xff from RAM via inAct edge*/
      dut.monitor.pokePartial(chiselTypeOf(dut.monitor).Lit(
        _.elements("out").asInstanceOf[TLBundle].a.bits -> inActOutEdge.Get(size, source, address, mask),
        _.elements("out").asInstanceOf[TLBundle].a.valid -> true.B
      ))
      dut.clock.step()
      dut.monitor.pokePartial(chiselTypeOf(dut.monitor).Lit(
        _.elements("out").asInstanceOf[TLBundle].a.valid -> false.B
      ))
      dut.clock.step(5)
      // TODO: add interrupt output
  }
}

object LazyEyerissSpecTest extends App {
  implicit val p: Parameters = Parameters((site, here, up) => {
    case MonitorsEnabled => false
  })
  val eyerissWrapper = LazyModule(new TLEyerissWrapperDutTest()(p))
  RawTester.test(eyerissWrapper.module, Seq(WriteVcdAnnotation)) {
    dut =>
      val inActEdges: Edges[TLEdgeIn, TLEdgeOut] = eyerissWrapper.eyeriss.memInActNode.edges
      val pSumEdges: Edges[TLEdgeIn, TLEdgeOut] = eyerissWrapper.eyeriss.memPSumNode.edges
      val inActOutEdge = inActEdges.out.head
      val pSumOutEdge = pSumEdges.out.head
      val size = 1
      val mask = 0xff
      val source = 0
      val address = 0x100 // to ram
      val data = 0xff
      dut.clock.setTimeout(0)
    //dut.eyerissIO
    // TODO: add instruction input
  }
}

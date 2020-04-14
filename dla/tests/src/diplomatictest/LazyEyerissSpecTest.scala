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

class TLEyerissWrapper(implicit p: Parameters) extends LazyModule {
  val ram = LazyModule(new TLRAM(AddressSet(0x100, 0xff)))
  val ram2 = LazyModule(new TLRAM(AddressSet(0, 0xff), beatBytes = 16))
  val xbar = LazyModule(new TLXbar)
  val eyeriss: LazyEyeriss = LazyModule(new LazyEyeriss(params = EyerissParams(
    address = 0x12,
    beatBytes = 10
  ))(p))

  xbar.node := eyeriss.memInActNode
  ram2.node := TLFragmenter(16, 256) := xbar.node
  ram.node := TLFragmenter(4, 256) := TLWidthWidget(16) := xbar.node

  lazy val module = new LazyModuleImp(this) {
    val monitor: AutoBundle = getIO(eyeriss.module.auto, "monitor")
  }
}

object LazyEyerissSpecTest extends App {
  implicit val p = Parameters((site, here, up) => {
    case MonitorsEnabled => false
  })
  val eyerissWrapper = LazyModule(new TLEyerissWrapper()(p))
  RawTester.test(eyerissWrapper.module, Seq(WriteVcdAnnotation)) {
    dut =>
      val edges: Edges[TLEdgeIn, TLEdgeOut] = eyerissWrapper.eyeriss.memInActNode.edges
      val outEdge = edges.out.head
      val size = 1
      val mask = 0xff
      val source = 0
      val address = 0x100
      val data = 0xff
  }
}

package dla.tests.diplomatictest

import chisel3._
import chisel3.tester._
import dla.cluster.{GNMFCS1Config, GNMFCS2Config}
import dla.diplomatic.{EyerissTop, EyerissTopParam}
import dla.pe.MCRENFConfig
import dla.tests.GenOneStreamData
import org.scalatest._

import scala.util.Random

class EyerissTopSpecTest extends FlatSpec with ChiselScalatestTester with Matchers {
  private val param = EyerissTopParam(
    addressBits = 32,
    inActDataBits = 32,
    inActSourceBits = 3,
    weightDataBits = 32,
    weightSourceBits = 3,
    pSumDataBits = 32,
    pSumSourceBits = 3
  )
  private val decoderSequencer = DecoderSequencer
  private val dataSequencer = new GenOneStreamData
  behavior of "test the spec of EyerissTop"
  it should "work well on cal" in {
    test(new EyerissTop(param = param)) { eyeriss =>
      val theTopIO = eyeriss.io
      val theClock = eyeriss.clock
      eyeriss.reset.poke(true.B)
      theClock.step()
      eyeriss.reset.poke(false.B)
      theTopIO.ctrlPath.instructions.poke(decoderSequencer.loadPart0.U)
      theClock.step(cycles = (new Random).nextInt(15) + 1)
      theTopIO.ctrlPath.instructions.poke(decoderSequencer.loadPart1.U)
      theClock.step(cycles = (new Random).nextInt(15) + 1)
      theTopIO.ctrlPath.instructions.poke(decoderSequencer.loadPart2.U)
      theClock.step(cycles = (new Random).nextInt(15) + 1)
      theTopIO.ctrlPath.instructions.poke(decoderSequencer.loadPart3.U)
      // then it will begins to load GLB
      theTopIO.ctrlPath.bundles.memInActBundles.legal.poke(true.B)
      // TODO: write a poke driver
      theTopIO.ctrlPath.bundles.memInActBundles.d.bits.data.poke(dataSequencer.inActStream.head.head.head.U)
    }
  }
}

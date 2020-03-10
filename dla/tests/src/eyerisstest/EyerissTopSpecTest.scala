package dla.tests.eyerisstest

import chisel3._
import chisel3.tester._
import dla.cluster.{ClusterSRAMConfig, GNMFCS1Config, GNMFCS2Config}
import org.scalatest._
import dla.eyerissTop.{Eyeriss, EyerissTopConfig}
import dla.pe.{MCRENFConfig, SPadSizeConfig}

import scala.util.Random
import scala.math.pow

class EyerissTopSpecTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "test the spec of Eyeriss"
  it should "work well on the basic test of Eyeriss" in {
    test (new Eyeriss(true)) { theEyeriss =>
      val theTopIO = theEyeriss.io
      val theClock = theEyeriss.clock
      theEyeriss.reset.poke(true.B)
      theClock.step()
      theEyeriss.reset.poke(false.B)
    }
  }
}

class CheckConfigs extends FlatSpec with EyerissTopConfig with GNMFCS1Config with GNMFCS2Config with MCRENFConfig with ClusterSRAMConfig with SPadSizeConfig{
  private val onePSumMatrixSize = Seq(M0, F0*N0*E)
  private val oneSPadPSum: Int = onePSumMatrixSize.product // when read counts this, then stop
  assert(C1 < cgRowNum, "C1 should smaller than cgRowNum to accumulate tiling partial sums together")
  assert(G1*N1*M1 < cgColNum*cgRowNum, "should have enough ClusterGroups to be mapped with tiling matrix multiplication")
  assert(oneSPadPSum*N2*M2*F2 < pSumSRAMSize, "pSumSRAM should contain one group of data")
  assert(F1 == 1, "maybe you need to change the logic design")
  assert(S1 == 1, "maybe you need to change the logic design")
  assert(oneSPadPSum <= pSumDataSPadSize, "oneSPadPSum should less than the size of pSum SPad")
}

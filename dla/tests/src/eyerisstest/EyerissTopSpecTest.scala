package dla.tests.eyerisstest

import chisel3._
import chisel3.tester._
import dla.cluster.{ClusterSRAMConfig, GNMFCS1Config, GNMFCS2Config}
import org.scalatest._
import dla.eyerissWrapper.{Eyeriss, EyerissTopConfig}
import dla.pe.{MCRENFConfig, SPadSizeConfig}

import scala.util.Random
import scala.math.pow

class EyerissTopSpecTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "test the spec of Eyeriss"
  //chisel3.Driver.emitVerilog(new Eyeriss(false))
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

class CheckConfigs extends FlatSpec with EyerissTopConfig with GNMFCS1Config with GNMFCS2Config with MCRENFConfig
  with ClusterSRAMConfig with SPadSizeConfig {
  private val onePSumMatrixSize = Seq(M0, F0*N0*E)
  private val oneSPadPSum: Int = onePSumMatrixSize.product // when read counts this, then stop
  assert(G1*(S1/peRowNum)*(F1/peColNum)*C1*N1*M1 == cgRowNum * cgColNum)
  assert(S1 % peRowNum == 0, s"S1 should be a multiple of peRowNum, $S1 % $peRowNum == 0?")
  assert(F1 % peColNum == 0, s"S1 should be a multiple of peRowNum, $F1 % $peColNum == 0?")
  assert(C1*S1 < cgRowNum, "C1*S1 should smaller than cgRowNum to accumulate tiling partial sums together")
  assert(oneSPadPSum*N2*M2*F2 < pSumSRAMSize, "pSumSRAM should contain one group of data")
  assert(C1 == 1 && S1 == peRowNum && F1 == peColNum, "maybe you need to change the logic design") // TODO: remove this
  assert(oneSPadPSum <= pSumDataSPadSize, "oneSPadPSum should less than the size of pSum SPad")
}

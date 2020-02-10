package dla.pe

import scala.math.pow
import chisel3.util.log2Ceil

trait MCRENFConfig extends PESizeConfig { // contains some scala values
  val M0: Int = 4 // weights reuse M0 times
  val C0: Int = 2 // different input feature maps and their weights reuse
  val R: Int = weightHeight //
  val E: Int = ofmapHeight // same row of weights in a PE
  val N0: Int = 3 // the number of partial sum
  val F0: Int = 1 // one row of partial sum
  val MCRENF: List[Int] = List(M0, C0, R, E, N0, F0)
  // C0*R0 < iacAddrSize = 9
  // C0*R0*E0*N0*F0 <
}

trait SPadSizeConfig extends PESizeConfig {
  val pSumDataSPadSize: Int = 32
  val iactDataSPadSize: Int = 16
  val iactAddrSPadSize: Int = 9
  val weightDataSPadSize: Int = 192 // 96 if SIMD
  val weightAddrSPadSize: Int = 16
  val iactAddrIdxWidth: Int = log2Ceil(iactAddrSPadSize)
  val iactDataIdxWidth: Int = log2Ceil(iactDataSPadSize)
  val weightAddrIdxWidth: Int = log2Ceil(weightAddrSPadSize)
  val weightDataIdxWidth: Int = log2Ceil(weightDataSPadSize)
  val pSumDataIdxWidth: Int = log2Ceil(pSumDataSPadSize)
}

trait PESizeConfig {
  val weightHeight: Int = 4
  val ofmapHeight: Int = 2
  val iactDataWidth: Int = 12 // 8-bit data and 4-bit count
  val iactAddrWidth: Int = 4
  val weightDataWidth: Int = 12 // 24 if SIMD
  val weightAddrWidth: Int = 7
  val cscDataWidth: Int = 8 // compressed sparse column data width
  val cscCountWidth: Int = 4 // compressed sparse column count width
  val psDataWidth: Int = 20
  val fifoSize: Int = 4
  val fifoEn: Boolean = true
  val iactZeroColumnCode: Int = pow(2, iactAddrWidth).toInt - 1 // when one address vector's element equals to iactZeroColumnCode, then it is a zero column
  val weightZeroColumnCode: Int = pow(2, weightAddrWidth).toInt - 1
}

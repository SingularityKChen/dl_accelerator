package dla.pe

import scala.math.pow
import chisel3.util.log2Ceil

trait MCRENFConfig { // contains some scala values
  /** this contains the parameters needed in SPad level
    * [[M0]]: weights reuse M0 times
    * [[C0]]: different input feature maps and their weights reuse
    * [[R]]:
    * [[E]]: same row of weights in a PE
    * [[N0]]: the number of partial sum
    * [[F0]]: one row of partial sum
    * [[pSumOneSPadNum]]: the size of one matrix of PSum
    * */
  protected val M0: Int = 4
  protected val C0: Int = 2
  protected val R: Int = 4
  protected val E: Int = 2
  protected val N0: Int = 3
  protected val F0: Int = 1
  protected val pSumOneSPadNum: Int = M0*E*N0*F0
  // C0*R0 < iacAdrSize = 9
  // C0*R0*E0*N0*F0 <
}

trait SPadSizeConfig {
  protected val pSumDataSPadSize: Int = 32
  protected val inActDataSPadSize: Int = 16
  protected val inActAdrSPadSize: Int = 9
  protected val weightDataSPadSize: Int = 192 // 96 if SIMD
  protected val weightAdrSPadSize: Int = 16
  protected val inActAdrIdxWidth: Int = log2Ceil(inActAdrSPadSize)
  protected val inActDataIdxWidth: Int = log2Ceil(inActDataSPadSize)
  protected val weightAdrIdxWidth: Int = log2Ceil(weightAdrSPadSize)
  protected val weightDataIdxWidth: Int = log2Ceil(weightDataSPadSize)
  protected val pSumDataIdxWidth: Int = log2Ceil(pSumDataSPadSize)
}

trait PESizeConfig {
  protected val inActDataWidth: Int = 12 // 8-bit data and 4-bit count
  protected val inActAdrWidth: Int = 4
  protected val weightDataWidth: Int = 12 // 24 if SIMD
  protected val weightAdrWidth: Int = 7
  protected val cscDataWidth: Int = 8 // compressed sparse column data width
  protected val cscCountWidth: Int = 4 // compressed sparse column count width
  protected val psDataWidth: Int = 20
  protected val fifoSize: Int = 4
  protected val fifoEn: Boolean = true
  /** when one address vector's element equals to [[inActZeroColumnCode]], then it is a zero column */
  protected val inActZeroColumnCode: Int = pow(2, inActAdrWidth).toInt - 1
  protected val weightZeroColumnCode: Int = pow(2, weightAdrWidth).toInt - 1
}

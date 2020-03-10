package dla.cluster

import dla.pe.PESizeConfig

trait ClusterConfig extends PESizeConfig { // if any changes, you're supposed to change the data path's connections
  val peColNum: Int = 4
  val peRowNum: Int = 3
  val inActRouterNum: Int = 3
  val inActPortNum: Int = 4
  val weightRouterNum: Int = peRowNum
  val weightPortNum: Int = 2
  val pSumRouterNum: Int = peColNum
  val pSumPortNum: Int = 3
}

trait ClusterSRAMConfig extends ClusterConfig {
  val inActDataSRAMSize: Int = 862 // 1.5KB*1024*8/12-bit
  val inActAdrSRAMSize: Int = 486 // 486/3=162, 162+862=1024=1.5KB*1024*8/12-bit
  val pSumSRAMSize: Int = 768 // 1.875KB*1024*8/20-bit
  val inActSRAMNum: Int = inActRouterNum
  val pSumSRAMNum: Int = pSumRouterNum
}

trait GNMFCS1Config {
  val G1: Int = 1
  val N1: Int = 2
  val M1: Int = 2
  val F1: Int = 1
  val C1: Int = 4
  val S1: Int = 1
}

trait GNMFCS2Config {
  val G2: Int = 1
  val N2: Int = 2
  val M2: Int = 4
  val F2: Int = 3
  val C2: Int = 3
  val S2: Int = 4
  protected val weightStreamNum: Int = G2*M2*C2*S2
  protected val inActStreamNum: Int = G2*N2*C2*(F2 + S2)
  protected val pSumStreamNum: Int = G2*N2*M2*F2
}

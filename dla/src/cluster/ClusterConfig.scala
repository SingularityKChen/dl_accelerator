package dla.cluster

import dla.pe.PESizeConfig

trait ClusterConfig extends PESizeConfig { // if any changes, you're supposed to change the data path's connections
  protected val peColNum: Int = 4
  protected val peRowNum: Int = 3
  protected val inActRouterNum: Int = 3
  protected val inActPortNum: Int = 4
  protected val weightRouterNum: Int = peRowNum
  protected val weightPortNum: Int = 2
  protected val pSumRouterNum: Int = peColNum
  protected val pSumPortNum: Int = 3
}

trait ClusterSRAMConfig extends ClusterConfig {
  protected val inActDataSRAMSize: Int = 862 // 1.5KB*1024*8/12-bit
  protected val inActAdrSRAMSize: Int = 520 // 486/3=162, 162+862=1024=1.5KB*1024*8/12-bit
  protected val pSumSRAMSize: Int = 768 // 1.875KB*1024*8/20-bit
  protected val inActSRAMNum: Int = inActRouterNum
  protected val pSumSRAMNum: Int = pSumRouterNum
}

trait GNMFCS1Config {
  protected val G1: Int = 1
  protected val N1: Int = 4
  protected val M1: Int = 2
  protected val F1: Int = 4
  protected val C1: Int = 1
  protected val S1: Int = 3
  protected val inActParNum: Int = G1*N1*C1*(F1 + S1)
  protected val weightParNum: Int = G1*M1*C1*S1
  protected val pSumParNum: Int = G1*N1*M1*F1
}

trait GNMFCS2Config {
  protected val G2: Int = 1
  protected val N2: Int = 2
  protected val M2: Int = 4
  protected val F2: Int = 3
  protected val C2: Int = 3
  protected val S2: Int = 4
  protected val weightStreamNum: Int = G2*M2*C2*S2
  protected val inActStreamNum: Int = G2*N2*C2*(F2 + S2)
  protected val pSumStreamNum: Int = G2*N2*M2*F2
}

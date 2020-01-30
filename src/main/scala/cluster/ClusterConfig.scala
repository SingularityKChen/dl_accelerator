package dla.cluster

import dla.pe.PESizeConfig

trait ClusterConfig extends PESizeConfig {
  val iactRouterNum: Int = 3
  val iactPortNum: Int = 4
  val weightRouterNum: Int = 3
  val weightPortNum: Int = 2
  val pSumRouterNum: Int = 4
  val pSumPortNum: Int = 3
  val peArrayColumnNum: Int = 4
  val peArrayRowNum: Int = 3
}

trait GNMFCSConfig {
  val G1: Int = 4
  val N1: Int = 2
  val M1: Int = 4
  val F1: Int = 3
  val C1: Int = 3
  val S1: Int = 4
  /*
  val G2: Int = 4
  val N2: Int = 2
  val M2: Int = 4
  val F2: Int = 3
  val C2: Int = 3
  val S2: Int = 4
   */
}

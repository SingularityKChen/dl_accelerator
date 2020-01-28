package dla.cluster

import dla.pe.PESizeConfig

trait ClusterConfig extends PESizeConfig {
  val iactRouterNum: Int = 3
  val iactPortNum: Int = 4
  val weightRouterNum: Int = 3
  val weightPortNum: Int = 2
  val pSumRouterNum: Int = 4
  val pSumPortNum: Int = 3
}

package dla.cluster

import chisel3._
import chisel3.util._

class ClusterGroup(debug: Boolean) extends Module {
  val io = IO(new Bundle {
    val config: ClusterGroupConfigIO = Flipped(new ClusterGroupConfigIO)
  })
  val peCluster = new PECluster(debug)
  val glbCluster = new GLBCluster(debug)
  val routerCluster = new RouterCluster(debug)
  peCluster.io.iactCluster.ctrlPath <> io.config.peClusterCtrl
}

package dla.cluster

import chisel3._
import chisel3.util._

class ClusterGroup(debug: Boolean) extends Module {
  val io = new ClusterGroupIO
  val peCluster = new PECluster(debug)
  val glbCluster = new GLBCluster(debug)
  val routerCluster = new RouterCluster(debug)
  peCluster.io.iactCluster.ctrlPath <> io.ctrlPath.peClusterCtrl
  routerCluster.io.routerIOs.iRIO.map(_.inIOs.ctrlPath <> io.ctrlPath.routerClusterCtrl.iactCtrlSel)
  routerCluster.io.routerIOs.wRIO.map(_.inIOs.ctrlPath <> io.ctrlPath.routerClusterCtrl.weightCtrlSel)
  routerCluster.io.routerIOs.pSRIO.map(_.inIOs.ctrlPath <> io.ctrlPath.routerClusterCtrl.pSumCtrlSel)
}

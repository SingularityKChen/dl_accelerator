package dla.cluster

import chisel3._
import chisel3.util._

class ClusterGroup(debug: Boolean) extends Module {
  val io = IO(new Bundle {
    val ctrlPath: ClusterGroupConfigIO = Flipped(new ClusterGroupConfigIO)
    //val dataPath
  })
  val peCluster = new PECluster(debug)
  val glbCluster = new GLBCluster(debug)
  val routerCluster = new RouterCluster(debug)
  peCluster.io.iactCluster.ctrlPath <> io.ctrlPath.peClusterCtrl
  routerCluster.io.iRIO.map(_.inIOs.ctrlPath <> io.ctrlPath.routerClusterCtrl.iactCtrlSel)
  routerCluster.io.wRIO.map(_.inIOs.ctrlPath <> io.ctrlPath.routerClusterCtrl.weightCtrlSel)
  routerCluster.io.pSRIO.map(_.inIOs.ctrlPath <> io.ctrlPath.routerClusterCtrl.pSumCtrlSel)
}

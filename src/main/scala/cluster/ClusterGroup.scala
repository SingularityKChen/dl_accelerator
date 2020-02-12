package dla.cluster

import chisel3._
import chisel3.util._

class ClusterGroup(debug: Boolean) extends Module with ClusterConfig {
  val io = new ClusterGroupIO
  val peCluster = new PECluster(debug)
  val glbCluster = new GLBCluster(debug)
  val routerCluster = new RouterCluster(debug)
  // connections of control path
  routerCluster.io.ctrlPath.iRIO.map(_ <> io.ctrlPath.routerClusterCtrl.iactCtrlSel)
  routerCluster.io.ctrlPath.wRIO.map(_ <> io.ctrlPath.routerClusterCtrl.weightCtrlSel)
  routerCluster.io.ctrlPath.pSRIO.map(_ <> io.ctrlPath.routerClusterCtrl.pSumCtrlSel)
  // true for broad-cast
  peCluster.io.ctrlPath.iactCtrlSel <> io.ctrlPath.peClusterCtrl
  //peCluster.io.ctrlPath.iactCtrlSel.inDataSel := io.ctrlPath.routerClusterCtrl.iactCtrlSel.outDataSel === 3.U
  //peCluster.io.ctrlPath.pSumCtrlSel.inDataSel :=
  peCluster.io.ctrlPath.pSumCtrlSel.outDataSel := DontCare
  // connections of data path
  // input activations
  for (i <- 0 until iactRouterNum) {

  }
  // weight
  for (i <- 0 until weightRouterNum) {

  }
  // partial sum
  for (i <- 0 until pSumRouterNum) {
    // 0 for PE Cluster, 1 for GLB Cluster, 2 for vertical
    // top connections
    glbCluster.io.dataPath.pSumIO(i).inIOs <> io.dataPath.glbDataPath.pSRIO(i).inIOs
    io.dataPath.glbDataPath.pSRIO(i).outIOs <> glbCluster.io.dataPath.pSumIO(i).outIOs
    routerCluster.io.dataPath.routerData.pSRIO(i).inIOs(2) <> io.dataPath.cgDataPath.pSRIO(i).inIOs
    io.dataPath.cgDataPath.pSRIO(i).outIOs <> routerCluster.io.dataPath.routerData.pSRIO(i).outIOs(2)
    // inner connections
    routerCluster.io.dataPath.routerData.pSRIO(i).inIOs.head <> peCluster.io.dataPath.pSumIO.outIOs(i)
    peCluster.io.dataPath.pSumIO.inIOs(i) <> routerCluster.io.dataPath.routerData.pSRIO(i).outIOs.head
    routerCluster.io.dataPath.routerData.pSRIO(i).inIOs(1) <> glbCluster.io.dataPath.pSumIO(i).outIOs
    glbCluster.io.dataPath.pSumIO(i).inIOs <> routerCluster.io.dataPath.routerData.pSRIO(i).outIOs(1)
  }
}

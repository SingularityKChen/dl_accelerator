package dla.cluster

import chisel3._
import chisel3.util._

class ClusterGroup(debug: Boolean) extends Module with ClusterConfig {
  val io: ClusterGroupIO = IO(new ClusterGroupIO)
  private val peCluster = new PECluster(debug)
  private val glbCluster = new GLBCluster(debug)
  private val routerCluster = new RouterCluster(debug)
  private val doCal: Bool = RegInit(false.B) // true, then they are doing calculation; false, then read in data or read out data
  // connections of control path
  routerCluster.io.ctrlPath.iRIO.foreach(_ <> io.ctrlPath.routerClusterCtrl.inActCtrlSel)
  routerCluster.io.ctrlPath.wRIO.foreach(_ <> io.ctrlPath.routerClusterCtrl.weightCtrlSel)
  routerCluster.io.ctrlPath.pSumRIO.foreach(_ <> io.ctrlPath.routerClusterCtrl.pSumCtrlSel)
  // true for broad-cast
  peCluster.io.ctrlPath.inActCtrlSel <> io.ctrlPath.peClusterCtrl
  //peCluster.io.ctrlPath.pSumCtrlSel.inDataSel := TODO
  peCluster.io.ctrlPath.pSumCtrlSel.outDataSel := DontCare
  // connections of data path
  // input activations
  for (i <- 0 until inActRouterNum) {
    // port 0  for GLB Cluster, 1 for north, 2 for south, 3 for horizontal
    // top connections
    glbCluster.io.dataPath.inActIO(i).inIOs <> io.dataPath.glbDataPath.inActIO(i).inIOs
    io.dataPath.glbDataPath.inActIO(i).outIOs <> DontCare // disable this IO
    for (j <- 1 until inActPortNum) {
      routerCluster.io.dataPath.routerData.iRIO(i).inIOs(j) <> io.dataPath.cgDataPath.iRIO(i).inIOs(j)
      // io.dataPath.cgDataPath.iRIO(i).inIOs.head <> DontCare
      io.dataPath.cgDataPath.iRIO(i).outIOs(j) <> routerCluster.io.dataPath.routerData.iRIO(i).outIOs(j)
    }
    io.dataPath.cgDataPath.iRIO(i).outIOs.head <> DontCare
    // inner connections
    routerCluster.io.dataPath.routerData.iRIO(i).inIOs.head <> glbCluster.io.dataPath.inActIO(i).outIOs
    peCluster.io.dataPath.inActIO(i) <> routerCluster.io.dataPath.routerData.iRIO(i).outIOs.head
  }
  // weight
  for (i <- 0 until weightRouterNum) {
    // input: port 0 from GLB Cluster, 1 from neighboring weight router
    // output: port 0 to PE Cluster, 1 to neighboring weight router
    // top connections
    glbCluster.io.dataPath.weightIO(i).inIOs <> io.dataPath.glbDataPath.weightIO(i).inIOs
    io.dataPath.glbDataPath.weightIO(i).outIOs <> DontCare // disable this IO
    routerCluster.io.dataPath.routerData.wRIO(i).inIOs.last <> io.dataPath.cgDataPath.wRIO(i).inIOs.last
    // io.dataPath.cgDataPath.wRIO(i).inIOs.head <> DontCare
    io.dataPath.cgDataPath.wRIO(i).outIOs.last <> routerCluster.io.dataPath.routerData.wRIO(i).outIOs.last
    io.dataPath.cgDataPath.wRIO(i).outIOs.head <> DontCare
    // inner connections
    routerCluster.io.dataPath.routerData.wRIO(i).inIOs.head <> glbCluster.io.dataPath.weightIO(i).outIOs
    peCluster.io.dataPath.weightIO(i) <> routerCluster.io.dataPath.routerData.wRIO(i).outIOs.head
  }
  // partial sum
  for (i <- 0 until pSumRouterNum) {
    // 0 for PE Cluster, 1 for GLB Cluster, 2 for vertical
    // top connections
    io.dataPath.glbDataPath.pSumIO(i).outIOs <> Mux(doCal, DontCare, glbCluster.io.dataPath.pSumIO(i).outIOs)
    routerCluster.io.dataPath.routerData.pSumRIO(i).inIOs.last <> io.dataPath.cgDataPath.pSumRIO(i).inIOs.last
    // io.dataPath.cgDataPath.pSumRIO(i).inIOs.take(2).foreach(_ <> DontCare)
    io.dataPath.cgDataPath.pSumRIO(i).outIOs.last <> routerCluster.io.dataPath.routerData.pSumRIO(i).outIOs.last
    io.dataPath.cgDataPath.pSumRIO(i).outIOs.take(2).foreach(_ <> DontCare)
    io.dataPath.routerPSumToPEIOs.outIOs(i) <> peCluster.io.dataPath.pSumIO.outIOs(i)
    peCluster.io.dataPath.routerInPSumToPEIO(i) <> io.dataPath.routerPSumToPEIOs.inIOs(i)
    // inner connections
    routerCluster.io.dataPath.routerData.pSumRIO(i).inIOs.head <> peCluster.io.dataPath.pSumIO.outIOs(i)
    peCluster.io.dataPath.pSumIO.inIOs(i) <> routerCluster.io.dataPath.routerData.pSumRIO(i).outIOs.head
    routerCluster.io.dataPath.routerData.pSumRIO(i).inIOs(1) <> Mux(doCal, glbCluster.io.dataPath.pSumIO(i).outIOs, DontCare)
    glbCluster.io.dataPath.pSumIO(i).inIOs <> Mux(doCal, routerCluster.io.dataPath.routerData.pSumRIO(i).outIOs(1), io.dataPath.glbDataPath.pSumIO(i).inIOs)
  }
}

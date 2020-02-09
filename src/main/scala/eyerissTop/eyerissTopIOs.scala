package dla.eyerissTop

import chisel3._
import chisel3.util._
import dla.cluster.ClusterCommonCtrlIO

class EyerissTopIOs extends Bundle with EyerissTopConfig {
  val configIOs = new EyerissConfigIOs
}
class EyerissConfigIOs extends Bundle with EyerissTopConfig {
  val iactCtrlSel: Vec[ClusterCommonCtrlIO[UInt, UInt]] = Vec(clusterColNum*clusterRowNum, new ClusterCommonCtrlIO[UInt, UInt](UInt(2.W), UInt(2.W)))
  //
  val weightCtrlSel: Vec[ClusterCommonCtrlIO[Bool, Bool]] = Vec(clusterRowNum, new ClusterCommonCtrlIO[Bool, Bool](Bool(), Bool()))
  // weightCtrlSel.inSel indicates whether the cluster group need to translate the data to other side;
  // weightCtrlSel.outSel true, then left one send data to right one;
  val pSumCtrlSel: Vec[ClusterCommonCtrlIO[UInt, UInt]] = Vec(clusterColNum, new ClusterCommonCtrlIO[UInt, UInt](UInt(2.W), UInt(2.W)))
  // pSumCtrlSel.inSel
  // pSumCtrlSel.OutSel
}

trait EyerissTopConfig {
  val clusterColNum: Int = 2
  val clusterRowNum: Int = 8
}
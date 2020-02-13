package dla.eyerissTop

import chisel3._
import chisel3.util._
import dla.cluster.{ClusterGroup, ClusterGroupIO, CommonClusterCtrlIO, GLBClusterDataIO}

class Eyeriss(debug: Boolean) extends Module with EyerissTopConfig {
  val io: EyerissIO = IO(new EyerissIO)
  private val cgRow: Vec[ClusterGroupIO] = Vec(cgColNum, Module(new ClusterGroup(debug)).io)
  private val cgArray: Vec[Vec[ClusterGroupIO]] = Vec(cgRowNum, cgRow)
  for (i <- 0 until cgRowNum) {
    for (j <- 0 until cgColNum) {
      cgArray(i)(j).ctrlPath.peClusterCtrl.inDataSel := io.ctrlPath.inActCtrlSel(i)(j).outDataSel === 3.U
      cgArray(i)(j).dataPath.glbDataPath <> io.dataPath.glbdataPath(i)(j)
      // disable the unused IOs
      cgArray(i)(j).dataPath.cgDataPath.iRIO.foreach(_.inIOs.head <> DontCare)
      cgArray(i)(j).dataPath.cgDataPath.wRIO.foreach(_.inIOs.head <> DontCare)
      cgArray(i)(j).dataPath.cgDataPath.pSumRIO.foreach(_.inIOs.take(2).foreach(_ <> DontCare))
    }
  }
}

class EyerissIO extends Bundle with EyerissTopConfig {
  val ctrlPath = new EyerissCtrlIO
  val dataPath = new EyerissDataIO
}

class EyerissDataIO extends Bundle with EyerissTopConfig {
  val glbdataPath: Vec[Vec[GLBClusterDataIO]] = Vec(cgRowNum, Vec(cgColNum, new GLBClusterDataIO))
}

class EyerissCtrlIO extends Bundle with EyerissTopConfig {
  val inActCtrlSel: Vec[Vec[CommonClusterCtrlIO[UInt, UInt]]] = Vec(cgRowNum, Vec(cgColNum, new CommonClusterCtrlIO[UInt, UInt](UInt(2.W), UInt(2.W))))
  //
  val weightCtrlSel: Vec[CommonClusterCtrlIO[Bool, Bool]] = Vec(cgRowNum, new CommonClusterCtrlIO[Bool, Bool](Bool(), Bool()))
  // weightCtrlSel.inSel indicates whether the cluster group need to translate the data to other side;
  // weightCtrlSel.outSel true, then left one send data to right one;
  val pSumCtrlSel: Vec[CommonClusterCtrlIO[UInt, UInt]] = Vec(cgColNum, new CommonClusterCtrlIO[UInt, UInt](UInt(2.W), UInt(2.W)))
  // pSumCtrlSel.inSel
  // pSumCtrlSel.OutSel
}

trait EyerissTopConfig {
  val cgColNum: Int = 2
  val cgRowNum: Int = 8
}
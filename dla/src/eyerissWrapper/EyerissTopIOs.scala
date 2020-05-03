package dla.eyerissWrapper

import chisel3._
import chisel3.util._
import dla.cluster.{ClusterGroup, ClusterGroupIO, CommonClusterCtrlTwoBoolIO, CommonClusterCtrlTwoUIntIO, GLBClusterDataIO}

class Eyeriss(debug: Boolean) extends Module with EyerissTopConfig {
  require(cgColNum == 2, "the default design is based on 2 columns, you need to change the connections now")
  //require(cgRowNum == 8)
  val io: EyerissIO = IO(new EyerissIO)
  protected val cgArray: Seq[Seq[ClusterGroupIO]] = Seq.fill(cgRowNum, cgColNum){Module(new ClusterGroup(debug)).io}
  protected val layerType: UInt = RegInit(0.U(2.W)) // 0: CONY layers, 1: depth-wise CONV layers, 2: fully-connected layers
  protected val inActMode: UInt = RegInit(0.U(2.W)) // 0: uni-cast, 1: horizontal, 2: vertical, 3: broadcast
  protected val weightMode: UInt = RegInit(0.U(2.W)) // 0: uni-cast, 1: horizontal multi-cast, 2: broadcast
  inActMode := MuxLookup(layerType, 0.U, Seq({
    0.asUInt -> 1.U // TODO: check when should be vertical multi-cast when should be horizontal
    1.asUInt -> 0.U
    2.asUInt -> 3.U
  }))
  weightMode := MuxLookup(layerType, 0.U, Seq({
    0.asUInt -> 1.U
    1.asUInt -> 2.U
    2.asUInt -> 0.U
  }))
  for (i <- 0 until cgRowNum) {
    for (j <- 0 until cgColNum) {
      cgArray(i)(j).ctrlPath.peClusterCtrl.inActSel.inDataSel := io.ctrlPath.inActCtrlSel(i)(j).outDataSel === 3.U
      cgArray(i)(j).dataPath.glbDataPath <> io.dataPath.glbDataPath(i)(j)
      // disable the unused IOs
      cgArray(i)(j).dataPath.cgDataPath.iRIO.foreach(_.inIOs.head <> DontCare)
    }
    // Input Activation Horizontal Connections
    cgArray(i).head.dataPath.cgDataPath.iRIO.zipWithIndex.foreach({ case (o, idx) =>
      o.inIOs(3) <> cgArray(i)(1).dataPath.cgDataPath.iRIO(idx).outIOs(3)
    })
    // Weight
    cgArray(i).head.dataPath.cgDataPath.wRIO.zipWithIndex.foreach({ case (o, idx) =>
      o.inIOs.last <> cgArray(i)(1).dataPath.cgDataPath.wRIO(idx).outIOs.last
      cgArray(i)(1).dataPath.cgDataPath.wRIO(idx).inIOs.last <> o.outIOs.last
    })
    cgArray(i).foreach(_.dataPath.cgDataPath.wRIO.foreach(_.inIOs.head <> DontCare))
    // Input Activation Vertical Connections and Partial Sum
    // whether last row?
    if (i < cgRowNum - 1) {
      cgArray(i).zip(cgArray(i+1)).foreach({ case (cg0, cg1) =>
        // input activation router south port (see inIOs(2))
        cg0.dataPath.cgDataPath.iRIO.zipWithIndex.foreach({ case (o, idx) =>
          o.inIOs(2) <> cg1.dataPath.cgDataPath.iRIO(idx).outIOs(1)
        })
        // connections of routers to PE Clusters partial sum
        cg0.dataPath.pSumDataVerticalIOs.inIOs <> cg1.dataPath.pSumDataVerticalIOs.outIOs
      })
    } else { // at the last row
      cgArray(i).foreach({ x =>
        x.dataPath.cgDataPath.iRIO.foreach(_.inIOs(2) <> DontCare)
        x.dataPath.pSumDataVerticalIOs.inIOs <> DontCare
      })
    }
    // whether first row?
    if (i == 0) { // at the first row
      cgArray(i).foreach({ x =>
        x.dataPath.cgDataPath.iRIO.foreach(_.inIOs(1) <> DontCare)
        x.dataPath.cgDataPath.pSumRIO.foreach(_.inIOs <> DontCare)
      })
    } else {
      cgArray(i).zip(cgArray(i-1)).foreach({ case (cg0, cg1) =>
        // input activation router north port (see inIOs(1))
        cg0.dataPath.cgDataPath.iRIO.zipWithIndex.foreach({ case (o, idx) =>
          o.inIOs(1) <> cg1.dataPath.cgDataPath.iRIO(idx).outIOs(2)
        })
        // connection of vertical partial sum routers
        cg0.dataPath.cgDataPath.pSumRIO.zipWithIndex.foreach({ case (o, idx) =>
          o.inIOs(2) <> cg1.dataPath.cgDataPath.pSumRIO(idx).outIOs(2)
          o.inIOs.take(2).foreach(_ <> DontCare)
        })
      })
    }
  }
}

class EyerissIO extends Bundle with EyerissTopConfig {
  val ctrlPath = new EyerissCtrlIO
  val dataPath = new EyerissDataIO
}

class EyerissDataIO extends Bundle with EyerissTopConfig {
  val glbDataPath: Vec[Vec[GLBClusterDataIO]] = Vec(cgRowNum, Vec(cgColNum, new GLBClusterDataIO))
}

class EyerissCtrlIO extends Bundle with EyerissTopConfig {
  val inActCtrlSel: Vec[Vec[CommonClusterCtrlTwoUIntIO]] = Vec(cgRowNum, Vec(cgColNum, Flipped(new CommonClusterCtrlTwoUIntIO)))
  //
  val weightCtrlSel: Vec[CommonClusterCtrlTwoBoolIO] = Vec(cgRowNum, Flipped(new CommonClusterCtrlTwoBoolIO))
  // weightCtrlSel.inSel indicates whether the cluster group need to translate the data to other side;
  // weightCtrlSel.outSel true, then left one send data to right one;
  val pSumCtrlSel: Vec[CommonClusterCtrlTwoUIntIO] = Vec(cgColNum, Flipped(new CommonClusterCtrlTwoUIntIO))
  // pSumCtrlSel.inSel
  // pSumCtrlSel.OutSel
}

trait EyerissTopConfig { // if the column number or row number changes, then groups connections will change
  val cgColNum: Int = 2
  val cgRowNum: Int = 8
}

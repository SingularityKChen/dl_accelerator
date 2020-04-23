package dla.eyerissTop

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import dla.cluster.{ClusterGroup, ClusterGroupCtrlIO, ClusterSRAMConfig, GNMFCS2Config, PSumSRAMDataIO}
import dla.pe.StreamBitsIO
import firrtl.options.TargetDirAnnotation

class ClusterGroupWrapper extends Module with ClusterSRAMConfig {
  /** This is a wrapper for Cluster Group.
    * As the Cluster Group module only needs the input activation and weight from Memory,
    * doesn't need the data from other cluster group, nor need the output ports of inAct and weight.
    * */
  val io = IO(new Bundle {
    val dataPath = new Bundle {
      val inActIO: Vec[StreamBitsIO] = Vec(inActSRAMNum, Flipped(new StreamBitsIO(dataWidth = cscDataWidth))) // input only
      val weightIO: Vec[StreamBitsIO] = Vec(weightRouterNum, Flipped(new StreamBitsIO(dataWidth = cscDataWidth)))
      val pSumIO: Vec[PSumSRAMDataIO] = Vec(pSumSRAMNum, new PSumSRAMDataIO(psDataWidth))
    }
    val ctrlPath = new Bundle {
      val cgCtrlPath: ClusterGroupCtrlIO = new ClusterGroupCtrlIO // now, input them
      val cscSwitcherCtrlPath = new CSCSwitcherCtrlPath
    }
  })
  private val cgModule = Module(new ClusterGroup(debug = false)).io
  private val inActCSCSwitchersModule = Seq.fill(inActSRAMNum){
    Module(new CSCSwitcher(debug = false, adrWidth = inActAdrWidth))
  }
  private val weightCSCSwitchersModule = Seq.fill(weightRouterNum){
    Module(new CSCSwitcher(adrWidth = weightAdrWidth, debug = false))
  }
  private val inActCSCSwitchersIO = inActCSCSwitchersModule.map(x => x.io)
  private val weightCSCSwitchersIO = weightCSCSwitchersModule.map(x => x.io)
  cgModule.ctrlPath <> io.ctrlPath.cgCtrlPath
  cgModule.dataPath.glbDataPath.pSumIO <> io.dataPath.pSumIO
  /** connections between cscSwitcher and cgModule*/
  for (i <- 0 until inActSRAMNum) {
    inActCSCSwitchersIO(i).ctrlPath <> io.ctrlPath.cscSwitcherCtrlPath.inActCSCSwitcher
    inActCSCSwitchersIO(i).inData <> io.dataPath.inActIO(i).data
    cgModule.dataPath.glbDataPath.inActIO(i).inIOs <> inActCSCSwitchersIO(i).outData
  }
  for (i <- 0 until weightRouterNum) {
    weightCSCSwitchersIO(i).ctrlPath <> io.ctrlPath.cscSwitcherCtrlPath.weightCSCSwitcher
    weightCSCSwitchersIO(i).inData <> io.dataPath.weightIO(i).data
    cgModule.dataPath.glbDataPath.weightIO(i).inIOs <> weightCSCSwitchersIO(i).outData
  }
  /** disable the unused ports*/
  cgModule.dataPath.glbDataPath.inActIO.foreach(_.outIOs <> DontCare)
  cgModule.dataPath.glbDataPath.weightIO.foreach(_.outIOs <> DontCare)
  cgModule.dataPath.pSumDataVerticalIOs <> DontCare
  cgModule.dataPath.cgDataPath <> DontCare
  /** add some suggest name*/
  inActCSCSwitchersModule.zipWithIndex.foreach({ case (switcher, i) => switcher.suggestName(s"inActSwitcher$i")})
  weightCSCSwitchersModule.zipWithIndex.foreach({ case (switcher, i) => switcher.suggestName(s"weightSwitcher$i")})
}

class CSCSwitcherCtrlPath extends Bundle with GNMFCS2Config {
  val inActCSCSwitcher = new CSCSwitcherCtrlIO(lgVectorNum = log2Ceil(inActStreamNum))
  val weightCSCSwitcher = new CSCSwitcherCtrlIO(lgVectorNum = log2Ceil(weightStreamNum))
}

object GenClusterGroupWithWrapper extends App {
  (new ChiselStage).run(Seq(
    ChiselGeneratorAnnotation(() => new ClusterGroupWrapper),
    TargetDirAnnotation(directory = "test_run_dir/ClusterGroupWrapper")
  ))
}

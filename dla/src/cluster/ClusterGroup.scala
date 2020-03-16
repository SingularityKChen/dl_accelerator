package dla.cluster

import chisel3._
import chisel3.experimental.{DataMirror, Direction}
import chisel3.util._
import dla.pe.SPadSizeConfig

class ClusterGroup(debug: Boolean) extends HasConnectAllExpRdModule with ClusterConfig {
  val io: ClusterGroupIO = IO(new ClusterGroupIO)
  private val peCluster = Module(new PECluster(debug)).io
  private val glbCluster = Module(new GLBCluster(debug)).io
  private val routerCluster = Module(new RouterCluster(debug)).io
  private val clusterCtrl = Module(new ClusterGroupController(debug = debug)).io
  // suggest name
  // connections of control path
  require(DataMirror.directionOf(io.ctrlPath.routerClusterCtrl.inActCtrlSel.inDataSel) == Direction.Input, "it should be input")
  // routerCluster control path
  routerCluster.ctrlPath.iRIO.foreach(_ <> io.ctrlPath.routerClusterCtrl.inActCtrlSel)
  routerCluster.ctrlPath.wRIO.foreach(_ <> io.ctrlPath.routerClusterCtrl.weightCtrlSel)
  routerCluster.ctrlPath.pSumRIO.foreach(_ <> io.ctrlPath.routerClusterCtrl.pSumCtrlSel)
  // peCluster control path
  // true for broad-cast
  peCluster.ctrlPath.inActCtrlSel <> io.ctrlPath.peClusterCtrl.inActSel
  peCluster.ctrlPath.pSumCtrlSel.inDataSel := io.ctrlPath.peClusterCtrl.pSumInSel
  // we can disable the outDataSel, as output of pSum is connected directly to router and outside
  peCluster.ctrlPath.pSumCtrlSel.outDataSel := DontCare // unused
  // doEn
  peCluster.ctrlPath.doEn := clusterCtrl.peCtrlIO.peLoadEn // to load inAct and weight // TODO: check
  peCluster.ctrlPath.pSumLoadEn := clusterCtrl.peCtrlIO.pSumLoadEn
  // GLB control path
  glbCluster.ctrlPath.pSumIO.zip(clusterCtrl.glbPSumCtrlIOs).foreach({ case (o, o1) => o <> o1})
  // glbCluster.ctrlPath.pSumIO.writeOrRead: true when glbLoad or write back from PEArray
  glbCluster.ctrlPath.inActIO.zip(clusterCtrl.glbInActCtrlIOs).foreach({ case (o, o1) => o <> o1})
  // clusterCtrl
  clusterCtrl.topIO.readOutPSum := io.ctrlPath.readOutPSum
  clusterCtrl.topIO.cgEnable := io.ctrlPath.readOutPSum // FIXME
  clusterCtrl.allPSumAddFin := peCluster.ctrlPath.allPSumAddFin
  clusterCtrl.allCalFin := peCluster.ctrlPath.allCalFin
  // connections of data path
  // input activations
  for (i <- 0 until inActRouterNum) {
    // port 0  for GLB Cluster, 1 for north, 2 for south, 3 for horizontal
    // top connections
    glbCluster.dataPath.inActIO(i).inIOs <> io.dataPath.glbDataPath.inActIO(i).inIOs
    io.dataPath.glbDataPath.inActIO(i).outIOs <> DontCare // disable this IO
    for (j <- 1 until inActPortNum) {
      routerCluster.dataPath.routerData.iRIO(i).inIOs(j) <> io.dataPath.cgDataPath.iRIO(i).inIOs(j)
      io.dataPath.cgDataPath.iRIO(i).outIOs(j) <> routerCluster.dataPath.routerData.iRIO(i).outIOs(j)
    }
    // the inActRouter in port zero comes from GLB, so we can disable the GroupIO's inAct port zero
    io.dataPath.cgDataPath.iRIO(i).inIOs.head <> DontCare
    // the inActRouter out port zero connected to peCluster, so disable the GroupIO's inAct port zero
    io.dataPath.cgDataPath.iRIO(i).outIOs.head <> DontCare
    // inner connections
    routerCluster.dataPath.routerData.iRIO(i).inIOs.head <> glbCluster.dataPath.inActIO(i).outIOs
    peCluster.dataPath.inActIO(i) <> routerCluster.dataPath.routerData.iRIO(i).outIOs.head
  }
  // weight
  require(io.dataPath.cgDataPath.wRIO.head.inIOs.length == 2, "the number of ports in weight router should be 2 " +
    "or you should correct the code")
  for (i <- 0 until weightRouterNum) {
    // input: port 0 from GLB Cluster, 1 from neighboring weight router
    // output: port 0 to PE Cluster, 1 to neighboring weight router
    // top connections
    // GLB Cluster
    glbCluster.dataPath.weightIO(i).inIOs <> io.dataPath.glbDataPath.weightIO(i).inIOs
    io.dataPath.glbDataPath.weightIO(i).outIOs <> DontCare // disable it, as we don't output weight via GLB
    routerCluster.dataPath.routerData.wRIO(i).inIOs.last <> io.dataPath.cgDataPath.wRIO(i).inIOs.last
    // Router Cluster
    // the corresponding port of weightRouter connected GLB, so disable it
    io.dataPath.cgDataPath.wRIO(i).inIOs.head <> DontCare
    io.dataPath.cgDataPath.wRIO(i).outIOs.last <> routerCluster.dataPath.routerData.wRIO(i).outIOs.last
    // the corresponding port of weightRouter connected GLB, so disable it
    io.dataPath.cgDataPath.wRIO(i).outIOs.head <> DontCare
    // inner connections
    routerCluster.dataPath.routerData.wRIO(i).inIOs.head <> glbCluster.dataPath.weightIO(i).outIOs
    peCluster.dataPath.weightIO(i) <> routerCluster.dataPath.routerData.wRIO(i).outIOs.head
  }
  // partial sum
  require(io.dataPath.cgDataPath.pSumRIO.head.inIOs.length == 3, "the number of ports " +
    "in partial sum router should be 3 or you should correct the code")
  for (i <- 0 until pSumRouterNum) {
    // 0 for PE Cluster, 1 for GLB Cluster, 2 for vertical
    // top connections
    io.dataPath.glbDataPath.pSumIO(i).outIOs.bits := glbCluster.dataPath.pSumIO(i).outIOs.bits
    io.dataPath.glbDataPath.pSumIO(i).outIOs.valid := glbCluster.dataPath.pSumIO(i).outIOs.valid &&
      io.ctrlPath.readOutPSum // when need read out PSum from GLB to outside
    routerCluster.dataPath.routerData.pSumRIO(i).inIOs(1).bits := glbCluster.dataPath.pSumIO(i).outIOs.bits
    routerCluster.dataPath.routerData.pSumRIO(i).inIOs(1).valid := glbCluster.dataPath.pSumIO(i).outIOs.valid &&
      clusterCtrl.pSumAdd // when need accumulate pSum, then we need to read out pSum from GLB
    glbCluster.dataPath.pSumIO(i).outIOs.ready := (routerCluster.dataPath.routerData.pSumRIO(i).inIOs(1).ready &&
      clusterCtrl.pSumAdd) || (io.dataPath.glbDataPath.pSumIO(i).outIOs.ready && io.ctrlPath.readOutPSum)

    when (!clusterCtrl.glbLoadEn) { // FIXME
      glbCluster.dataPath.pSumIO(i).inIOs <> routerCluster.dataPath.routerData.pSumRIO(i).outIOs(1)
      io.dataPath.glbDataPath.pSumIO(i).inIOs.ready := false.B
    } .otherwise {
      glbCluster.dataPath.pSumIO(i).inIOs <> io.dataPath.glbDataPath.pSumIO(i).inIOs
      routerCluster.dataPath.routerData.pSumRIO(i).outIOs(1).ready := false.B
    }
    routerCluster.dataPath.routerData.pSumRIO(i).inIOs.last <> io.dataPath.cgDataPath.pSumRIO(i).inIOs.last
    // only need the final one to receive data outside, other two comes from inner
    io.dataPath.cgDataPath.pSumRIO(i).inIOs.take(2).foreach(_ <> DontCare)
    io.dataPath.cgDataPath.pSumRIO(i).outIOs.last <> routerCluster.dataPath.routerData.pSumRIO(i).outIOs.last
    // only need the final one to receive data outside, other two comes from inner
    io.dataPath.cgDataPath.pSumRIO(i).outIOs.take(2).foreach(_ <> DontCare)
    // pSumData from or to vertical PE Arrays
    io.dataPath.pSumDataVerticalIOs.outIOs(i) <> peCluster.dataPath.pSumIO.outIOs(i)
    peCluster.dataPath.pSumDataFromSouthernIO(i) <> io.dataPath.pSumDataVerticalIOs.inIOs(i)
    // inner connections
    // peCluster.dataPath.pSumIO.outIOs(i) transfer partial sum not only to outside cluster group, but also to router
    routerCluster.dataPath.routerData.pSumRIO(i).inIOs.head <> peCluster.dataPath.pSumIO.outIOs(i)
    // peCluster.dataPath.pSumIO.inIOs(i) receive data directly from pSumRouter
    peCluster.dataPath.pSumIO.inIOs(i) <> routerCluster.dataPath.routerData.pSumRIO(i).outIOs.head
  }
}

class ClusterGroupController(debug: Boolean) extends Module with GNMFCS2Config with ClusterSRAMConfig with SPadSizeConfig{
  val io: ClusterGroupControllerIO = IO(new ClusterGroupControllerIO)
  private val configFixValSeq = Seq(G2, N2, M2, F2, C2, S2)
  private val configIncWireSeq = Seq.fill(6){Wire(Bool())}
  configIncWireSeq.zipWithIndex.foreach({ case (bool, i) => bool.suggestName(s"configIncWire$i")})
  private val (configG2Val, configG2Wrap) = Counter(configIncWireSeq.head, configFixValSeq.head)
  private val (configN2Val, configN2Wrap) = Counter(configIncWireSeq(1), configFixValSeq(1))
  private val (configM2Val, configM2Wrap) = Counter(configIncWireSeq(2), configFixValSeq(2))
  private val (configF2Val, configF2Wrap) = Counter(configIncWireSeq(3), configFixValSeq(3))
  private val (configC2Val, configC2Wrap) = Counter(configIncWireSeq(4), configFixValSeq(4))
  private val (configS2Val, configS2Wrap) = Counter(configIncWireSeq(5), configFixValSeq(5))
  private val configWarpWireSeq = Seq(configG2Wrap, configN2Wrap, configM2Wrap, configF2Wrap, configC2Wrap, configS2Wrap)
  private val configValWireVec = Seq(configG2Val, configN2Val, configM2Val, configF2Val, configC2Val, configS2Val)
  // 0g, 1n, 2m, 3f, 4c, 5s
  private val glbInActWriteFinReg = Seq.fill(inActSRAMNum){RegInit(false.B)}
  private val glbPSumWriteFinReg = Seq.fill(pSumSRAMNum){RegInit(false.B)}
  private val glbWriteFinWire = Wire(Bool()) // true then both inAct and PSum have loaded into GLB
  glbInActWriteFinReg.zip(io.glbInActCtrlIOs.map(x => x.writeIO.done)).foreach({ case (reg, doneIO) =>
    reg := Mux(glbWriteFinWire, false.B, Mux(doneIO, true.B, reg))
  })
  glbPSumWriteFinReg.zip(io.glbPSumCtrlIOs.map(x => x.writeIO.done)).foreach({case (reg, doneIO) =>
    reg := Mux(glbWriteFinWire, false.B, Mux(doneIO, true.B, reg))
  })
  glbWriteFinWire := glbPSumWriteFinReg.reduce(_ && _) && glbInActWriteFinReg.reduce(_ && _)
  // cluster group state machine
  // cgLoadGLB: load inAct and PSum from outside CG into GLBCluster
  // cgLoadPE: load inAct, weight from outside PECluster (from GLB and outside CG)
  // cgCal: PE doing computations
  // cgRead: PE read PSum into the tails of PEArray to accumulate them and get out put PSum
  private val cgIdle :: cgLoadGLB :: cgLoadPE :: cgCal :: cgRead :: Nil = Enum(5)
  private val cgStateReg = RegInit(cgIdle)
  switch (cgStateReg) {
    is (cgIdle) {
      when (io.topIO.cgEnable) {  // when doEn ClusterGroup
        cgStateReg := cgLoadGLB
      }
    }
    is (cgLoadGLB) {
      when (glbWriteFinWire) {
        cgStateReg := cgLoadPE
      }
    }
    is (cgLoadPE) {
      when (true.B) { // when all write finish
        cgStateReg := cgCal
      }
    }
    is (cgCal) {
      when (configIncWireSeq(5)) { // every time s2Inc, that means cal finish
        when (configWarpWireSeq(4)) { // when c2Wrap, we need to read out PSum without change the values of f2, etc.
          cgStateReg := cgRead
        } .otherwise {// or we need to load new InAct and Weight BUT PSum
          cgStateReg := cgLoadPE
        }
      }
    }
    is (cgRead) {
      when (true.B) { // after read out all PSum from the head of each column
        when (configG2Wrap) { // when g2 wrap, then finish current takes
          cgStateReg := cgIdle
        } .otherwise { // or , load next c2*s2
          cgStateReg := cgLoadPE
        }
      }
    }
  }
  // logic
  private val c2WrapReg = RegInit(false.B)
  when (configWarpWireSeq(4)) { // c2 warp, then f2 will increase
    c2WrapReg := true.B
  } .elsewhen (configIncWireSeq(3)) { // after use this value
    c2WrapReg := false.B
  } .otherwise {
    c2WrapReg := c2WrapReg
  }
  configIncWireSeq(5) := io.allCalFin
  configIncWireSeq(4) := configWarpWireSeq(5) // s2Wrap, c2Inc
  configIncWireSeq(3) := c2WrapReg && io.allPSumAddFin // c2Wrap and read finish all PSum
  for (i <- 1 until 4) {
    configIncWireSeq(i - 1) := configWarpWireSeq(i)
  }
  // Outputs
  private val pSumAdrL2 = configValWireVec.head*N2.U*M2.U*F2.U +
    configValWireVec(1)*M2.U*F2.U + configValWireVec(2)*F2.U + configValWireVec(3)
  private val inActReadAdrL2 = configValWireVec.head // FIXME
  private val pSumWriteAdrL4Reg = RegInit(0.U(log2Ceil(pSumDataSPadSize).W))
  private val pSumReadAdrL4Reg = RegInit(0.U(log2Ceil(pSumDataSPadSize).W))
  private val cgLoadGLBWire = cgStateReg === cgLoadGLB
  private val cgLoadPEWire = cgStateReg === cgLoadPE
  private val cgReadWire = cgStateReg === cgRead
  io.glbPSumCtrlIOs.foreach({x =>
    x.writeIO.adr := pSumAdrL2 + pSumWriteAdrL4Reg
    x.writeIO.enable := cgLoadGLBWire || cgReadWire
    pSumWriteAdrL4Reg := Mux(x.writeIO.done, pSumWriteAdrL4Reg + 1.U, pSumWriteAdrL4Reg) // FIXME: need reset
    x.readIO.adr := pSumAdrL2 + pSumReadAdrL4Reg
    x.readIO.enable := cgReadWire || io.topIO.readOutPSum
    pSumReadAdrL4Reg := Mux(x.readIO.done, pSumReadAdrL4Reg + 1.U, pSumReadAdrL4Reg) // FIXME: need reset
  })
  io.glbInActCtrlIOs.foreach({x =>
    x.writeIO.enable := cgLoadGLBWire
    x.writeIO.adr := DontCare
    x.readIO.adr := inActReadAdrL2
    x.readIO.enable := cgLoadPEWire
  })
  io.peCtrlIO.pSumLoadEn := configIncWireSeq(3)
  io.peCtrlIO.peLoadEn := cgLoadPEWire
  io.pSumAdd := cgReadWire
  io.peCal := cgStateReg === cgCal
  io.glbLoadEn := cgLoadGLB
}

class ClusterGroupControllerIO extends Bundle with ClusterSRAMConfig with GNMFCS2Config {
  val glbPSumCtrlIOs: Vec[SRAMCommonCtrlIO] = Vec(pSumSRAMNum, Flipped(new SRAMCommonCtrlIO(theMemSize = pSumSRAMSize)))
  val glbInActCtrlIOs: Vec[SRAMCommonCtrlIO] = Vec(inActSRAMNum, Flipped(new SRAMCommonCtrlIO(theMemSize = inActStreamNum)))
  val peCtrlIO = new Bundle {
    val peLoadEn: Bool = Output(Bool())
    val pSumLoadEn: Bool = Output(Bool())
  }
  val allPSumAddFin: Bool = Input(Bool())
  val allCalFin: Bool = Input(Bool())
  val pSumAdd: Bool = Output(Bool())
  val peCal: Bool = Output(Bool())
  val topIO = new Bundle {
    val readOutPSum: Bool = Input(Bool())
    val cgEnable: Bool = Input(Bool())
  }
  val glbLoadEn: Bool = Output(Bool())
}

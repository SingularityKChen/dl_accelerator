package dla.tests

import dla.cluster.{ClusterSRAMConfig, GNMFCS1Config, GNMFCS2Config}
import dla.pe.{MCRENFConfig, PESizeConfig, SPadSizeConfig}
import org.scalatest._
import Console.{MAGENTA, RESET}
import scala.math.max
import chisel3.util.log2Ceil

case class EyerissModelParam(
                              G2: Int = 1, N2: Int = 2, M2: Int = 4, F2: Int = 3, C2: Int = 3, S2: Int = 4,
                              G1: Int = 1, N1: Int = 4, M1: Int = 2, F1: Int = 4, C1: Int = 2, S1: Int = 3,
                              M0: Int = 4, C0: Int = 2, R: Int = 4, E: Int = 2, N0: Int = 3, F0: Int = 1,
                              cgRow: Int = 8, cgCol: Int = 2, peRow: Int = 3, peCol: Int = 4, inActSRAMNum: Int = 3
) {
  /** true if Eyeriss can read from memory parallel,
    * false while it only sent one memory read requirement a time*/
  val parallelMemRead: Boolean = false
  object physicalInfo {
    val clusterNum: Int = cgRow * cgCol
    private val peArraySize = peRow * peCol
    val peNum: Int = peArraySize * clusterNum
    // mapping <> physical
    require(G1 == 1, "G1 has to be one, or you need to change the following requirements")
    require(N1*C1*S1/peRow == cgRow, s"dose ${N1*C1*S1/peRow} equals to $cgRow")
    require(M1*F1/peCol == cgCol, s"dose ${M1*F1/peCol} equals to $cgCol")
    require(S1 % peRow == 0, s"S1 = $S1 should be multiple of peRow = $peRow") // TODO: change to S1 >= peRow // by pSum
    require(F1 % peCol == 0, s"F1 = $F1 should be multiple of peCol = $peCol") // TODO: change to F1 >= peCol // by weight
    def printlnPhysicalInfo(): Unit = {
      println(s"[${MAGENTA}Info$RESET] the Eyeriss physical info")
      println(s"             |CGRow\t|CGCol\t|peRow\t|peCol\t|")
      println(s"             |$cgRow\t\t|$cgCol\t\t|$peRow\t\t|$peCol\t\t|")
    }
  }
  object mappingInfo {
    /**the number of different inAct in one PE array*/
    private val inActNumInOnePEArray: Int = peRow + peCol - 1 //TODO: make it more fine grain
    /** the read time for one SRAM to send all the PE their inAct*/
    val inActSRAMReadTimes: Int = inActNumInOnePEArray/inActSRAMNum
    require(inActNumInOnePEArray % inActSRAMNum == 0, s"inActNumInOnePEArray = $inActNumInOnePEArray should be" +
      s"multiple of inActSRAMNum = $inActSRAMNum")
    val pSumOneSPadNum: Int = M0*E*N0*F0
    val inActMatrixWidth: Int = F0*N0*E // column
    val inActMatrixHeight: Int = R*C0 // row
    val weightMatrixWidth: Int = inActMatrixHeight // column
    val weightMatrixHeight: Int = M0 // row
    val inActNoCNum: Int = G1*N1*C1*(F1 + S1)
    val weightNoCNum: Int = G1*M1*C1*S1
    val pSumNoCNum: Int = G1*N1*M1*F1
    val weightGLBNum: Int = G2*M2*C2*S2
    val inActGLBNum: Int = G2*N2*C2*(F2 + S2)
    val pSumGLBNum: Int = G2*N2*M2*F2
  }
  object nnShape {
    object inAct {
      val number: Int = N2*N1*N0
      val channel: Int = G2*G1*C2*C1*C0 //TODO: check G
      /** although the width of inAct in RS+ data flow is (S2 + F2)*(S1 + F1)*F0,
        * which is much greater than this width. It's caused by the overlap.*/
      val height: Int = R + E
      val width: Int = S2*S1 + F2*F1*F0
      //require(height == width, s"inAct's height doesn't equal to width, $height == $width ?")
    }
    object weight {
      val number: Int = M2*M1*M0
      val channel: Int = G2*G1*C2*C1*C0
      val height: Int = R
      val width: Int = S2*S1
      //require(height == width, s"weight's height doesn't equal to width, $height == $width ?")
    }
    object pSum {
      val number: Int = N2*N1*N0
      val channel: Int = G2*G1*M2*M1*M0
      val height: Int = E
      val width: Int = F2*F1*F0
      //require(height == width, s"pSum's height doesn't equal to width, $height == $width ?")
    }
    require(inAct.number == pSum.number)
    require(inAct.channel == weight.channel)
    require(weight.number == pSum.channel)
    def printNNShapeInfo(): Unit = {
      println(s"[${MAGENTA}Info$RESET] the NN shape")
      println(s"                   |\ttype\t|\tnum\t|\tchn\t|\th\t|\tw\t|")
      println(s"                   |\tweight\t|\t${weight.number}\t|\t${weight.channel}\t|\t${weight.height}\t|\t${weight.width}\t|")
      println(s"                   |\tinAct\t|\t${inAct.number}\t|\t${inAct.channel}\t|\t${inAct.height}\t|\t${inAct.width}\t|")
      println(s"                   |\tpSum\t|\t${pSum.number}\t|\t${pSum.channel}\t|\t${pSum.height}\t|\t${pSum.width}\t|")
    }
  }
}

class EyerissModel(sequencer: GenFunc, monitor: CompareMonitor, p: EyerissModelParam, printDetails: Boolean = true) extends PESizeConfig with SPadSizeConfig
  with MCRENFConfig with GNMFCS1Config with GNMFCS2Config with ClusterSRAMConfig {
  /** use monitor to compare the info between different test*/
  private val scoreBoard = new ScoreBoard
  // TODO: change the
  val pSumResult: Array[Array[Array[Array[Int]]]] = Array.fill(
    p.nnShape.pSum.number,
    p.nnShape.pSum.channel,
    p.nnShape.pSum.height,
    p.nnShape.pSum.width
  ) {0}
  private var parallelCycle = 0
  /** the first dimension is NoC level index, value true means have read this from mem, false means haven't read*/
  private val weightMemReadRecord: Array[Boolean] = Array.fill(p.mappingInfo.weightNoCNum) {false}
  /** the first dimension is NoC level index, value true means have read this from mem, false means haven't read*/
  private val inActMemReadRecord: Array[Boolean] = Array.fill(p.mappingInfo.inActNoCNum) {false}
  /** assume the data stored in Mem is pre-processed */
  /** the first dimension is cgRow idx, the second is cgCol idx, the third is inActSRAMIdx, inside is a list */
  private val inActAdrSRAM: Array[Array[Array[List[Int]]]] =
    Array.fill(p.cgRow, p.cgCol, p.inActSRAMNum) {Nil}
  /** the first dimension is cgRow idx, the second is cgCol idx, the third is inActSRAMIdx, inside is a list */
  private val inActDataSRAM: Array[Array[Array[List[Int]]]] =
    Array.fill(p.cgRow, p.cgCol, p.inActSRAMNum) {Nil}
  /** the first dimension is cgRow idx, the second is cgCol idx, the third is inActSRAMIdx,
    * the fourth is inActSRAMReadIdx. true when has written this data into inActSRAM*/
  private val inActSRAMWriteRecord: Array[Array[Array[Array[Boolean]]]] =
    Array.fill(p.cgRow, p.cgCol, p.inActSRAMNum, p.mappingInfo.inActSRAMReadTimes) {false}
  /** the first dimension is cgRow idx, the second is cgCol idx, the third is inActSRAMIdx,
    * the fourth is inActSRAMReadIdx. true when has read this data from inActSRAM*/
  private val inActSRAMReadRecord: Array[Array[Array[Array[Boolean]]]] =
    Array.fill(p.cgRow, p.cgCol, p.inActSRAMNum, p.mappingInfo.inActSRAMReadTimes) {false}
  //private var inActSRAMBankWriteRecord: List[Seq[Int]] = Nil
  /** NoC level:
    * the `for loops` of NoC level is the task mapping for each PE*/
  for (g1 <- 0 until p.G1) {
    for (n1 <- 0 until p.N1) {
      for (m1 <- 0 until p.M1) {
        for (f1 <- 0 until p.F1) {
          for (c1 <- 0 until p.C1) {
            for (s1 <- 0 until p.S1) {
              /** current physical info*/
              object cPhyInfo {
                /**current cluster group row idx*/
                val cr: Int = n1*p.C1*p.S1/p.peRow + c1*p.S1/p.peRow + s1/p.peRow
                /**current cluster group column idx*/
                val cc: Int = m1*p.F1/p.peCol + f1/p.peCol
                /**current pe row idx*/
                val pr: Int = s1%p.peRow
                /**current pe column idx*/
                val pc: Int = f1%p.peCol
                /**current pe's corresponding inAct SRAM idx in the GLB Cluster*/
                val inActSRAMIdx: Int = (pr + pc) % p.inActSRAMNum
                /** current SRAM's read/write times*/
                val inActReadTimeIdx: Int = (pr + pc) / p.inActSRAMNum
              }
              val weightNoCLevelIdx = g1*p.M1*p.C1*p.S1 + m1*p.C1*p.S1 + c1*p.S1 + s1
              val inActNoCLevelIdx = g1*p.N1*p.C1*(p.F1+p.S1) + n1*p.C1*(p.F1+p.S1) + c1*(p.F1+p.S1) + (f1+s1)
              /** read inAct from main memory*/
              if (!inActMemReadRecord(inActNoCLevelIdx)) {
                val inActMemReadNum = sequencer.dataSequencer.glb.inAct(inActNoCLevelIdx).flatten.flatten.length
                monitor.inActRead.mem += inActMemReadNum
                if (p.parallelMemRead) {
                  monitor.cycle += inActMemReadNum*scoreBoard.accessCost.mem/(p.G1*p.N1*p.C1*(p.F1+p.S1))
                } else {
                  /** if Eyeriss can only send one memory requirement at a time */
                  monitor.cycle += inActMemReadNum*scoreBoard.accessCost.mem
                }
                inActMemReadRecord(inActNoCLevelIdx) = true
              }
              /** write inAct SRAM */
              //println(s"current Read Times: ${cPhyInfo.inActReadTimeIdx}")
              if (!inActSRAMWriteRecord(cPhyInfo.cr)(cPhyInfo.cc)(cPhyInfo.inActSRAMIdx)(cPhyInfo.inActReadTimeIdx)){
                inActAdrSRAM(cPhyInfo.cr)(cPhyInfo.cc)(cPhyInfo.inActSRAMIdx) ++=
                  sequencer.dataSequencer.glb.cscData.inActAdr(inActNoCLevelIdx)
                inActDataSRAM(cPhyInfo.cr)(cPhyInfo.cc)(cPhyInfo.inActSRAMIdx) ++=
                  sequencer.dataSequencer.glb.cscData.inActData(inActNoCLevelIdx)
                val inActAdrGLBWriteNum = sequencer.dataSequencer.glb.cscData.inActAdr(inActNoCLevelIdx).length
                val inActDataGLBWriteNum = sequencer.dataSequencer.glb.cscData.inActData(inActNoCLevelIdx).length
                monitor.inActWrite.adr.glb += inActAdrGLBWriteNum
                monitor.inActWrite.data.glb += inActDataGLBWriteNum
                inActSRAMWriteRecord(cPhyInfo.cr)(cPhyInfo.cc)(cPhyInfo.inActSRAMIdx)(cPhyInfo.inActReadTimeIdx) = true
              }
              /** GLB level */
              for (g2 <- 0 until p.G2) {
                for (n2 <- 0 until p.N2) {
                  for (m2 <- 0 until p.M2) {
                    for (f2 <- 0 until p.F2) {
                      for (c2 <- 0 until p.C2) {
                        for (s2 <- 0 until p.S2) {
                          val weightGLBLevelIdx = g2*p.M2*p.C2*p.S2 + m2*p.C2*p.S2 + c2*p.S2 + s2
                          val inActGLBLevelIdx = g2*p.N2*p.C2*(p.F2 + p.S2) + n2*p.C2*(p.F2+p.S2) + c2*(p.F2+p.S2) + f2+s2
                          /** SPad level */
                          val inActAdrSPad = sequencer.dataSequencer.glb.separatedSPadCSCData.
                            inActAdr(inActNoCLevelIdx)(inActGLBLevelIdx)
                          val inActDataSPad = sequencer.dataSequencer.glb.separatedSPadCSCData.
                            inActData(inActNoCLevelIdx)(inActGLBLevelIdx)
                          val weightAdrSPad = sequencer.dataSequencer.glb.separatedSPadCSCData.
                            weightAdr(weightNoCLevelIdx)(weightGLBLevelIdx)
                          val weightDataSPad = sequencer.dataSequencer.glb.separatedSPadCSCData.
                            weightData(weightNoCLevelIdx)(weightGLBLevelIdx)
                          val pSumSPad: Array[Array[Int]] = Array.fill(p.F0*p.N0*p.E, p.M0) {0}
                          val inActGLBReadNum = max(inActAdrSPad.length, inActDataSPad.length)
                          val weightMemReadNum = sequencer.dataSequencer.glb.
                            weight(weightNoCLevelIdx)(weightGLBLevelIdx).flatten.length
                          /** the same weight value only be read once from Mem*/
                          if (!weightMemReadRecord(weightNoCLevelIdx)) {
                            monitor.weightRead.mem += weightMemReadNum
                            if (!p.parallelMemRead) {
                              /**if Eyeriss can not read parallel, then assume that read weight from memory
                                * for GLB levels times will cost more than read inAct from SRAM*/
                              monitor.cycle += weightMemReadNum*scoreBoard.accessCost.mem
                            }
                          }
                          if (p.parallelMemRead) {
                            parallelCycle += max(inActGLBReadNum*scoreBoard.accessCost.glb,
                              weightMemReadNum*scoreBoard.accessCost.mem)
                          }
                          /** read from GLB once and send into diagonal PEs
                            * and if this has been read, then other clusters can receive inAct via Router */
                          if (!inActSRAMReadRecord(cPhyInfo.cr)(cPhyInfo.cc)(cPhyInfo.inActSRAMIdx)(cPhyInfo.inActReadTimeIdx)) {
                            monitor.inActRead.adr.glb += inActAdrSPad.length
                            monitor.inActRead.data.glb += inActDataSPad.length
                          }
                          monitor.inActWrite.adr.sPad += inActAdrSPad.length
                          monitor.inActWrite.data.sPad += inActDataSPad.length
                          monitor.weightWrite.adr.sPad += weightAdrSPad.length
                          monitor.weightWrite.data.sPad += weightDataSPad.length
                          var inActDataSPadIdx = 0
                          for (inActAdrSPadIdx <- inActAdrSPad.indices) {
                            /** padInActAdr: read each column of current inAct Matrix */
                            val inActAdr = inActAdrSPad(inActAdrSPadIdx)
                            monitor.inActRead.adr.sPad += 1
                            parallelCycle += 1
                            if (inActAdr != inActZeroColumnCode || inActAdr == 0) {
                              /** padInActData: read each row of current column */
                              while (inActDataSPadIdx < inActAdr) {
                                val inActDataRead = inActDataSPad(inActDataSPadIdx)
                                val inActData = BigInt(inActDataRead.toBinaryString.take(cscDataWidth), 2).toInt
                                val inActRow = BigInt(inActDataRead.toBinaryString.takeRight(cscCountWidth), 2).toInt
                                monitor.inActRead.data.sPad += 1
                                parallelCycle += 1
                                if (inActDataRead != 0) {
                                  /** padWeightAdr */
                                  val weightAdr = weightAdrSPad(inActRow)
                                  val weightDataSPadStartIdx = if (inActRow == 0) 0 else weightAdrSPad(inActRow - 1)
                                  monitor.weightRead.adr.sPad += 1
                                  parallelCycle += 1
                                  if (weightAdr != weightZeroColumnCode || weightAdr == 0) {
                                    /** padWeightData */
                                    for (weightDataSPadIdx <- weightDataSPadStartIdx until weightAdr) {
                                      val weightDataRead = weightDataSPad(weightDataSPadIdx)
                                      val weightData = BigInt(weightDataRead.toBinaryString.take(cscDataWidth), 2).toInt
                                      val weightRow = BigInt(weightDataRead.toBinaryString.takeRight(cscCountWidth), 2).toInt
                                      monitor.weightRead.data.sPad += 1
                                      parallelCycle += 2 // need 2 cycles to read from SRAM
                                      pSumSPad(inActAdrSPadIdx)(weightRow) += weightData * inActData
                                      monitor.macNum += 1
                                      parallelCycle += 2 // one for mpy, one for write back
                                    }
                                  }
                                }
                                inActDataSPadIdx += 1
                              }
                            }
                          }
                          //print(".") // finish SPad Level
                          /** accumulate PSum vertically */
                          // TODO
                        }
                      }
                    }
                  }
                }
              }
              //print("*\n") // finish GLB Level
              inActSRAMReadRecord(cPhyInfo.cr)(cPhyInfo.cc)(cPhyInfo.inActSRAMIdx)(cPhyInfo.inActReadTimeIdx) = true
              weightMemReadRecord(weightNoCLevelIdx) = true
            }
          }
        }
      }
    }
  }
  monitor.cycle += parallelCycle/p.physicalInfo.peNum
  if (printDetails) {
    monitor.printMonitorInfo(p.physicalInfo.peNum)
    p.nnShape.printNNShapeInfo()
  }
}

class CommonModel(sequencer: GenFunc, monitor: CompareMonitor, p: EyerissModelParam,
                  printDetails: Boolean = true, needPSum: Boolean)
  extends PESizeConfig with SPadSizeConfig with MCRENFConfig with GNMFCS1Config with GNMFCS2Config with ClusterSRAMConfig {
  private val scoreBoard = new ScoreBoard
  val pSumResult: Array[Array[Array[Array[Int]]]] = Array.fill(
    p.nnShape.pSum.number,
    p.nnShape.pSum.channel,
    p.nnShape.pSum.height,
    p.nnShape.pSum.width
  ) {0}
  if (needPSum) {
    /** each PSum number */
    for (n <- 0 until p.nnShape.pSum.number) {
      /** each PSum channel*/
      for (m <- 0 until p.nnShape.pSum.channel) {
        /** PSum height */
        for (f <- 0 until p.nnShape.pSum.width) {
          /** PSum width*/
          for (e <- 0 until p.nnShape.pSum.height) {
            /** inside this for loop, do mac, for the size of weight matrix */
            /** weight channel */
            for (c <- 0 until p.nnShape.weight.channel) {
              /** weight height */
              for (s <- 0 until p.nnShape.weight.width) {
                /** weight width */
                for (r <- 0 until p.nnShape.weight.height) {
                  pSumResult(n)(m)(e)(f) += sequencer.dataSequencer.dram.weight(m)(c)(r)(s) *
                    sequencer.dataSequencer.dram.inAct(n)(c)(r+e)(s+f)
                  monitor.inActRead.mem += 1
                  monitor.weightRead.mem += 1
                  monitor.macNum += 1
                }
              }
            }
            //print(".") // finish one PSum
          }
        }
        //print("*") // finish one PSum matrix
      }
      /*println("\n[INFO] finish one batch of PSum " +
        f"${((n + 1).toFloat/p.nnShape.pSum.number.toFloat)*100}%.2f%%")*/
    }
  } else {
    val totalNum = p.nnShape.pSum.number * p.nnShape.pSum.channel * p.nnShape.pSum.width * p.nnShape.pSum.height *
      p.nnShape.weight.channel * p.nnShape.weight.width * p.nnShape.weight.height
    monitor.inActRead.mem = totalNum
    monitor.weightRead.mem = totalNum
    monitor.macNum = totalNum
  }
  monitor.cycle = scoreBoard.totalCycles(monitor.macNum, p.physicalInfo.peNum, monitor.inActRead.mem, 0, 0)
  if (printDetails) {
    monitor.printMonitorInfo(p.physicalInfo.peNum)
    p.nnShape.printNNShapeInfo()
  }
}

class ScalaModelTest extends FlatSpec {
  behavior of "compare the efficiency of Eyeriss"
  /** model the behavior of Eyeriss cluster group */
  it should "changing mapping parameters" in {
    object peRow {
      val start = 1
      val end = 4
      val during: Int = end - start
    }
    object peCol {
      val start = 1
      val end = 4
      val during: Int = end - start
    }
    val monitorSeq = Seq.fill(peRow.during, peCol.during, 2) {new CompareMonitor}
    /** the min size of inAct adr SPad and data SPad to meet the requirement.
      * [[SPadSizeConfig]].[[inActAdrSPadSize]] and [[SPadSizeConfig]].[[inActDataSPadSize]]*/
    val inActSPadSizeNeed: Array[Array[Array[Int]]] = Array.fill(peRow.during, peCol.during, 2) {0}
    /** the min size of inAct adr SRAM and data SRAM to meet the requirement.
      * [[ClusterSRAMConfig]].[[inActAdrSRAMSize]] and [[ClusterSRAMConfig]].[[inActDataSRAMSize]]*/
    val inActSRAMSizeNeed: Array[Array[Array[Int]]] = Array.fill(peRow.during, peCol.during, 2) {0}
    /** the min bits of inAct adr to meet the requirement. [[PESizeConfig]].[[inActAdrWidth]]*/
    val inActAdrWidthNeed: Array[Array[Int]] = Array.fill(peRow.during, peCol.during) {0}
    /** the min bits of inAct data to meet the requirement. [[PESizeConfig]].[[inActDataWidth]]*/
    val inActDataWidthNeed: Array[Array[Int]] = Array.fill(peRow.during, peCol.during) {0}
    /** the min size of weight adr SPad and data SPad to meet the requirement.
      * [[SPadSizeConfig]].[[weightAdrSPadSize]] and [[SPadSizeConfig]].[[weightDataSPadSize]]*/
    val weightSPadSizeNeed: Array[Array[Array[Int]]] = Array.fill(peRow.during, peCol.during, 2) {0}
    /** the min bits of weight adr to meet the requirement. [[PESizeConfig]].[[weightAdrWidth]]*/
    val weightAdrWidthNeed: Array[Array[Int]] = Array.fill(peRow.during, peCol.during) {0}
    /** the min bits of weight data to meet the requirement. [[PESizeConfig]].[[weightDataWidth]]*/
    val weightDataWidthNeed: Array[Array[Int]] = Array.fill(peRow.during, peCol.during) {0}
    for (peRowFactor <- peRow.start until peRow.end) {
      for (peColFactor <- peCol.start until peCol.end) {
        val peRowIdx = peRowFactor - peRow.start
        val peColIdx = peColFactor - peCol.start
        val peRowNum = 3*peRowFactor
        val peColNum = 4*peColFactor
        var readTime = 2
        while ((peRowNum + peColNum - 1) % readTime != 0) {
          readTime += 1
        }
        val inActSRAMNum = (peRowNum + peColNum - 1) / readTime
        val param = EyerissModelParam(peRow = peRowNum, peCol = peColNum,
          S1 = peRowNum, F1 = peColNum, inActSRAMNum = inActSRAMNum)
        val sequencer = new GenFunc(inActSparseRatio = 0.6, weightSparseRatio = 0.6, p = param)
        val eyerissModel = new EyerissModel(sequencer = sequencer,
          monitor = monitorSeq(peRowIdx)(peColIdx).head,
          p = param, printDetails = false)
        val common = new CommonModel(sequencer = sequencer,
          monitor = monitorSeq(peRowIdx)(peColIdx)(1),
          p = param, printDetails = false, needPSum = false)
        inActSPadSizeNeed(peRowIdx)(peColIdx)(0) =
          sequencer.dataSequencer.glb.separatedSPadCSCData.inActAdr.map(x => x.map(y => y.length).max).max
        inActSPadSizeNeed(peRowIdx)(peColIdx)(1) =
          sequencer.dataSequencer.glb.separatedSPadCSCData.inActData.map(x => x.map(y => y.length).max).max
        inActSRAMSizeNeed(peRowIdx)(peColIdx)(0) =
          sequencer.dataSequencer.glb.cscData.inActAdr.map(x => x.length).max
        inActSRAMSizeNeed(peRowIdx)(peColIdx)(1) =
          sequencer.dataSequencer.glb.cscData.inActData.map(x => x.length).max
        inActAdrWidthNeed(peRowIdx)(peColIdx) =
          log2Ceil(sequencer.dataSequencer.glb.cscData.inActAdr.flatten.filter(x => x != scala.math.pow(2,7)-1).max)
        inActDataWidthNeed(peRowIdx)(peColIdx) =
          log2Ceil(sequencer.dataSequencer.glb.cscData.inActData.flatten.filter(x => x != scala.math.pow(2,12)-1).max)
        weightSPadSizeNeed(peRowIdx)(peColIdx)(0) =
          sequencer.dataSequencer.glb.separatedSPadCSCData.weightAdr.map(x => x.map(y => y.length).max).max
        weightSPadSizeNeed(peRowIdx)(peColIdx)(1) =
          sequencer.dataSequencer.glb.separatedSPadCSCData.weightData.map(x => x.map(y => y.length).max).max
        weightAdrWidthNeed(peRowIdx)(peColIdx) =
          log2Ceil(sequencer.dataSequencer.glb.cscData.weightAdr.flatten.filter(x => x != scala.math.pow(2,7)-1).max)
        weightDataWidthNeed(peRowIdx)(peColIdx) =
          log2Ceil(sequencer.dataSequencer.glb.cscData.weightData.flatten.filter(x => x != scala.math.pow(2,12)-1).max)
        println(s"[${MAGENTA}Info$RESET] current peRow = $peRowNum, peCol = $peColNum, inActSRAM = $inActSRAMNum")
        if (peRowFactor == peRow.end - 1 && peColFactor == peCol.end - 1) {
          param.nnShape.printNNShapeInfo()
          param.physicalInfo.printlnPhysicalInfo()
        }
      }
    }
    println("|pRow\t|pCol\t|cycle%\t\t|mac%\t\t|iMem%\t\t|wMem%\t\t|iGLB%\t\t|iSPad%\t\t|")
    for (peRowFactor <- peRow.start until peRow.end) {
      for (peColFactor <- peCol.start until peCol.end) {
        val peRowIdx = peRowFactor - peRow.start
        val peColIdx = peColFactor - peCol.start
        val eyerissMonitor = monitorSeq(peRowIdx)(peColIdx).head
        val commonMonitor = monitorSeq(peRowIdx)(peColIdx).last
        val inActGLBWriteTotal = eyerissMonitor.inActWrite.adr.glb + eyerissMonitor.inActWrite.data.glb
        val inActGLBReadTotal = eyerissMonitor.inActRead.adr.glb + eyerissMonitor.inActRead.data.glb
        val inActSPadWriteTotal = eyerissMonitor.inActWrite.adr.sPad + eyerissMonitor.inActWrite.data.sPad
        val inActSPadReadTotal = eyerissMonitor.inActRead.adr.sPad + eyerissMonitor.inActRead.data.sPad
        /**inAct GLB  R / GLB W */
        val eyerissInActGLBRW = f"${(inActGLBReadTotal.toFloat/inActGLBWriteTotal.toFloat)*100}%.2f%%"
        /**inAct SPad W / GLB R */
        val eyerissInActSPadReuse = f"${(inActSPadWriteTotal.toFloat/inActGLBReadTotal.toFloat)*100}%.2f%%"
        val cycleEfficiency = f"${(eyerissMonitor.cycle.toFloat / commonMonitor.cycle.toFloat)*100}%.4f%%"
        val macEfficiency = f"${(eyerissMonitor.macNum.toFloat / commonMonitor.macNum.toFloat)*100}%.4f%%"
        val inActMemReadEfficiency =
          f"${(eyerissMonitor.inActRead.mem.toFloat / commonMonitor.inActRead.mem.toFloat)*100}%.4f%%"
        val weightMemReadEfficiency =
          f"${(eyerissMonitor.weightRead.mem.toFloat / commonMonitor.weightRead.mem.toFloat)*100}%.4f%%"
        println(s"|${peRowFactor*3}\t\t|${peColFactor*4}\t\t|$cycleEfficiency\t|" +
          s"$macEfficiency\t|$inActMemReadEfficiency\t|$weightMemReadEfficiency\t|" +
          s"$eyerissInActGLBRW\t|$eyerissInActSPadReuse\t|")
      }
    }
    println("\n|pRow\t|pCol\t|inActAdrSPad\t|inActDataSPad\t|weightAdrSPad\t|weightDataSPad\t|inActAdrSRAM\t|inActDataSRAM\t|")
    for (peRowFactor <- peRow.start until peRow.end) {
      for (peColFactor <- peCol.start until peCol.end) {
        val peRowIdx = peRowFactor - peRow.start
        val peColIdx = peColFactor - peCol.start
        println(s"|${peRowFactor*3}\t\t|${peColFactor*4}\t\t|" +
          s"${inActSPadSizeNeed(peRowIdx)(peColIdx)(0)}\t${inActAdrWidthNeed(peRowIdx)(peColIdx)}-bit\t|" +
          s"${inActSPadSizeNeed(peRowIdx)(peColIdx)(1)}\t${inActDataWidthNeed(peRowIdx)(peColIdx)}-bit\t|" +
          s"${weightSPadSizeNeed(peRowIdx)(peColIdx)(0)}\t${weightAdrWidthNeed(peRowIdx)(peColIdx)}-bit\t|" +
          s"${weightSPadSizeNeed(peRowIdx)(peColIdx)(1)}\t${weightDataWidthNeed(peRowIdx)(peColIdx)}-bit\t|" +
          s"${inActSRAMSizeNeed(peRowIdx)(peColIdx)(0)}\t|" +
          s"${inActSRAMSizeNeed(peRowIdx)(peColIdx)(1)}\t|"
        )
      }
    }
  }

  it should "compare the info across sparse ratio between eyeriss and common device" in {
    val param = EyerissModelParam()
    object inActRatioSeq {
      val start = 4
      val end = 10
      val during: Int = end - start
    }
    object weightRatioSeq {
      val start = 4
      val end = 10
      val during: Int = end - start
    }
    val monitorSeq = Seq.fill(inActRatioSeq.during, weightRatioSeq.during, 2){new CompareMonitor}
    /** the min size of inAct adr SPad and data SPad to meet the requirement.
      * [[SPadSizeConfig]].[[inActAdrSPadSize]] and [[SPadSizeConfig]].[[inActDataSPadSize]]*/
    val inActSPadSizeNeed: Array[Array[Array[Int]]] = Array.fill(inActRatioSeq.during, weightRatioSeq.during, 2) {0}
    /** the min size of inAct adr SRAM and data SRAM to meet the requirement.
      * [[ClusterSRAMConfig]].[[inActAdrSRAMSize]] and [[ClusterSRAMConfig]].[[inActDataSRAMSize]]*/
    val inActSRAMSizeNeed: Array[Array[Array[Int]]] = Array.fill(inActRatioSeq.during, weightRatioSeq.during, 2) {0}
    /** the min bits of inAct adr to meet the requirement. [[PESizeConfig]].[[inActAdrWidth]]*/
    val inActAdrWidthNeed: Array[Array[Int]] = Array.fill(inActRatioSeq.during, weightRatioSeq.during) {0}
    /** the min bits of inAct data to meet the requirement. [[PESizeConfig]].[[inActDataWidth]]*/
    val inActDataWidthNeed: Array[Array[Int]] = Array.fill(inActRatioSeq.during, weightRatioSeq.during) {0}
    /** the min size of weight adr SPad and data SPad to meet the requirement.
      * [[SPadSizeConfig]].[[weightAdrSPadSize]] and [[SPadSizeConfig]].[[weightDataSPadSize]]*/
    val weightSPadSizeNeed: Array[Array[Array[Int]]] = Array.fill(inActRatioSeq.during, weightRatioSeq.during, 2) {0}
    /** the min bits of weight adr to meet the requirement. [[PESizeConfig]].[[weightAdrWidth]]*/
    val weightAdrWidthNeed: Array[Array[Int]] = Array.fill(inActRatioSeq.during, weightRatioSeq.during) {0}
    /** the min bits of weight data to meet the requirement. [[PESizeConfig]].[[weightDataWidth]]*/
    val weightDataWidthNeed: Array[Array[Int]] = Array.fill(inActRatioSeq.during, weightRatioSeq.during) {0}
    for (inActRatio <- inActRatioSeq.start until inActRatioSeq.end) {
      for (weightRatio <- weightRatioSeq.start until weightRatioSeq.end) {
        val sequencer = new GenFunc(inActSparseRatio = inActRatio.toDouble/10,
          weightSparseRatio = weightRatio.toDouble/10, p = param)
        val inActRatioIdx = inActRatio - inActRatioSeq.start
        val weightRatioIdx = weightRatio - weightRatioSeq.start
        val eyerissModel = new EyerissModel(sequencer = sequencer,
          monitor = monitorSeq(inActRatioIdx)(weightRatioIdx).head,
          p = param, printDetails = false)
        val common = new CommonModel(sequencer = sequencer,
          monitor = monitorSeq(inActRatioIdx)(weightRatioIdx)(1),
          p = param, printDetails = false, needPSum = false)
        inActSPadSizeNeed(inActRatioIdx)(weightRatioIdx)(0) =
          sequencer.dataSequencer.glb.separatedSPadCSCData.inActAdr.map(x => x.map(y => y.length).max).max
        inActSPadSizeNeed(inActRatioIdx)(weightRatioIdx)(1) =
          sequencer.dataSequencer.glb.separatedSPadCSCData.inActData.map(x => x.map(y => y.length).max).max
        inActSRAMSizeNeed(inActRatioIdx)(weightRatioIdx)(0) =
          sequencer.dataSequencer.glb.cscData.inActAdr.map(x => x.length).max
        inActSRAMSizeNeed(inActRatioIdx)(weightRatioIdx)(1) =
          sequencer.dataSequencer.glb.cscData.inActData.map(x => x.length).max
        inActAdrWidthNeed(inActRatioIdx)(weightRatioIdx) =
          log2Ceil(sequencer.dataSequencer.glb.cscData.inActAdr.flatten.filter(x => x != scala.math.pow(2,7)-1).max)
        inActDataWidthNeed(inActRatioIdx)(weightRatioIdx) =
          log2Ceil(sequencer.dataSequencer.glb.cscData.inActData.flatten.filter(x => x != scala.math.pow(2,12)-1).max)
        weightSPadSizeNeed(inActRatioIdx)(weightRatioIdx)(0) =
          sequencer.dataSequencer.glb.separatedSPadCSCData.weightAdr.map(x => x.map(y => y.length).max).max
        weightSPadSizeNeed(inActRatioIdx)(weightRatioIdx)(1) =
          sequencer.dataSequencer.glb.separatedSPadCSCData.weightData.map(x => x.map(y => y.length).max).max
        weightAdrWidthNeed(inActRatioIdx)(weightRatioIdx) =
          log2Ceil(sequencer.dataSequencer.glb.cscData.weightAdr.flatten.filter(x => x != scala.math.pow(2,7)-1).max)
        weightDataWidthNeed(inActRatioIdx)(weightRatioIdx) =
          log2Ceil(sequencer.dataSequencer.glb.cscData.weightData.flatten.filter(x => x != scala.math.pow(2,12)-1).max)
        println(s"[${MAGENTA}Info$RESET] current sparse ratio: 0.$inActRatio, 0.$weightRatio")
        if (inActRatio == inActRatioSeq.end - 1 && weightRatio == weightRatioSeq.end - 1) {
          param.nnShape.printNNShapeInfo()
          param.physicalInfo.printlnPhysicalInfo()
        }
      }
    }
    println("|iRa\t|wRa\t|cycle%\t\t|mac%\t\t|iMem%\t\t|wMem%\t\t|iGLB%\t\t|iSPad%\t\t|")
    for (inActRatio <- inActRatioSeq.start until inActRatioSeq.end) {
      for (weightRatio <- weightRatioSeq.start until weightRatioSeq.end) {
        val inActRatioIdx = inActRatio - inActRatioSeq.start
        val weightRatioIdx = weightRatio - weightRatioSeq.start
        val eyerissMonitor = monitorSeq(inActRatioIdx)(weightRatioIdx).head
        val commonMonitor = monitorSeq(inActRatioIdx)(weightRatioIdx).last
        val inActGLBWriteTotal = eyerissMonitor.inActWrite.adr.glb + eyerissMonitor.inActWrite.data.glb
        val inActGLBReadTotal = eyerissMonitor.inActRead.adr.glb + eyerissMonitor.inActRead.data.glb
        val inActSPadWriteTotal = eyerissMonitor.inActWrite.adr.sPad + eyerissMonitor.inActWrite.data.sPad
        val inActSPadReadTotal = eyerissMonitor.inActRead.adr.sPad + eyerissMonitor.inActRead.data.sPad
        /**inAct GLB  R / GLB W */
        val eyerissInActGLBRW = f"${(inActGLBReadTotal.toFloat/inActGLBWriteTotal.toFloat)*100}%.2f%%"
        /**inAct SPad W / GLB R */
        val eyerissInActSPadReuse = f"${(inActSPadWriteTotal.toFloat/inActGLBReadTotal.toFloat)*100}%.2f%%"
        val cycleEfficiency = f"${(eyerissMonitor.cycle.toFloat / commonMonitor.cycle.toFloat)*100}%.4f%%"
        val macEfficiency = f"${(eyerissMonitor.macNum.toFloat / commonMonitor.macNum.toFloat)*100}%.4f%%"
        val inActMemReadEfficiency =
          f"${(eyerissMonitor.inActRead.mem.toFloat / commonMonitor.inActRead.mem.toFloat)*100}%.4f%%"
        val weightMemReadEfficiency =
          f"${(eyerissMonitor.weightRead.mem.toFloat / commonMonitor.weightRead.mem.toFloat)*100}%.4f%%"
        println(s"|${inActRatio.toDouble/10}\t|${weightRatio.toDouble/10}\t|$cycleEfficiency\t|" +
          s"$macEfficiency\t|$inActMemReadEfficiency\t|$weightMemReadEfficiency\t|" +
          s"$eyerissInActGLBRW\t|$eyerissInActSPadReuse\t|")
      }
    }
    println("\n|iRa\t|wRa\t|inActAdrSPad\t|inActDataSPad\t|weightAdrSPad\t|weightDataSPad\t|inActAdrSRAM\t|inActDataSRAM\t|")
    for (inActRatio <- inActRatioSeq.start until inActRatioSeq.end) {
      for (weightRatio <- weightRatioSeq.start until weightRatioSeq.end) {
        val inActRatioIdx = inActRatio - inActRatioSeq.start
        val weightRatioIdx = weightRatio - weightRatioSeq.start
        println(s"|0.$inActRatio\t|0.$weightRatio\t|" +
          s"${inActSPadSizeNeed(inActRatioIdx)(weightRatioIdx)(0)}\t${inActAdrWidthNeed(inActRatioIdx)(weightRatioIdx)}-bit\t|" +
          s"${inActSPadSizeNeed(inActRatioIdx)(weightRatioIdx)(1)}\t${inActDataWidthNeed(inActRatioIdx)(weightRatioIdx)}-bit\t|" +
          s"${weightSPadSizeNeed(inActRatioIdx)(weightRatioIdx)(0)}\t${weightAdrWidthNeed(inActRatioIdx)(weightRatioIdx)}-bit\t|" +
          s"${weightSPadSizeNeed(inActRatioIdx)(weightRatioIdx)(1)}\t${weightDataWidthNeed(inActRatioIdx)(weightRatioIdx)}-bit\t|" +
          s"${inActSRAMSizeNeed(inActRatioIdx)(weightRatioIdx)(0)}\t|" +
          s"${inActSRAMSizeNeed(inActRatioIdx)(weightRatioIdx)(1)}\t|"
        )
      }
    }
  }
}

class CompareMonitor extends ClusterSRAMConfig {
  var cycle: BigInt = 0 // the number of clock cycles
  var macNum: BigInt = 0 // the number of mac
  class CSCAccess {
    var glb: BigInt = 0 // the times to access glb sram
    var sPad: BigInt = 0 // the times to access sPad register
  }
  class MemHierarchyAccess {
    var mem: BigInt = 0 // the times to access memory
    val adr = new CSCAccess
    val data = new CSCAccess
  }
  val inActRead = new MemHierarchyAccess
  val inActWrite = new MemHierarchyAccess
  val weightRead = new MemHierarchyAccess
  val weightWrite = new MemHierarchyAccess
  def printMonitorInfo(peNum: Int): Unit = {
    val inActGLBWriteTotal = inActWrite.adr.glb + inActWrite.data.glb
    val inActGLBReadTotal = inActRead.adr.glb + inActRead.data.glb
    val inActSPadWriteTotal = inActWrite.adr.sPad + inActWrite.data.sPad
    val inActSPadReadTotal = inActRead.adr.sPad + inActRead.data.sPad
    println(s"[${MAGENTA}Info$RESET] computation finishes, using $peNum PEs")
    println(s"------ time = $cycle cycles")
    println(s"------ mac num = $macNum")
    println(s"------ inActAccess ")
    println(s"                   | memRead: ${inActRead.mem}")
    println(s"                   | glb")
    println(s"                        | glbWrite: $inActGLBWriteTotal")
    println(s"                                   | glbAdrWrite: ${inActWrite.adr.glb}")
    println(s"                                   | glbDataWrite: ${inActWrite.data.glb}")
    println(s"                        | glbRead: $inActGLBReadTotal")
    println(s"                                   | glbAdrRead: ${inActRead.adr.glb}")
    println(s"                                   | glbDataRead: ${inActRead.data.glb}")
    println(s"                   | sPad")
    println(s"                        | sPadWrite: $inActSPadWriteTotal")
    println(s"                                   | sPadAdrWrite: ${inActWrite.adr.sPad}")
    println(s"                                   | sPadDataWrite: ${inActWrite.data.sPad}")
    println(s"                        | sPadRead: $inActSPadReadTotal")
    println(s"                                   | sPadAdrRead: ${inActRead.adr.sPad}")
    println(s"                                   | sPadDataRead: ${inActRead.data.sPad}")
    println(s"------ weightAccess")
    println(s"                   | mem: ${weightRead.mem}")
    println(s"                   | sPad")
    println(s"                        | sPadWrite: ${weightWrite.adr.sPad + weightWrite.data.sPad}")
    println(s"                                   | sPadAdrWrite: ${weightWrite.adr.sPad}")
    println(s"                                   | sPadDataWrite: ${weightWrite.data.sPad}")
    println(s"                        | sPadRead: ${weightRead.adr.sPad + weightRead.data.sPad}")
    println(s"                                   | sPadAdrRead: ${weightRead.adr.sPad}")
    println(s"                                   | sPadDataRead: ${weightRead.data.sPad}")
    println(s"------ dataReuse")
    println("                | inAct GLB  R / GLB W: " +
      f"${(inActGLBReadTotal.toFloat/inActGLBWriteTotal.toFloat)*100}%.2f%%")
    println("                | inAct SPad W / GLB R: " +
      f"${(inActSPadWriteTotal.toFloat/inActGLBReadTotal.toFloat)*100}%.2f%%")
  }
}

class ScoreBoard {
  /** [[macCost]]: every mac will cost 3 clock cycle
    * 0: multiply
    * 1: accumulate
    * 2: write back*/
  val macCost: Int = 3
  object accessCost { // every access will cost ? clock cycles
    val mem: Int = 60
    val glb: Int = 2
    val sPad: Int = 1
  }
  def totalCycles(macNum: BigInt, peNum: Int, memNum: BigInt, glbNum: BigInt, sPadNum: BigInt ): BigInt = {
    val cycles: BigInt = (macNum/peNum) * macCost + memNum * accessCost.mem +
      glbNum * accessCost.glb + sPadNum * accessCost.sPad
    cycles
  }
}
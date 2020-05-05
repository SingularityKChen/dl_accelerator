package dla.tests

import dla.cluster.{ClusterSRAMConfig, GNMFCS1Config, GNMFCS2Config}
import dla.pe.{MCRENFConfig, PESizeConfig, SPadSizeConfig}
import org.scalatest._
import scala.math.max
import chisel3.util.log2Ceil

case class NNModelMapping (
                          G2: Int = 1, N2: Int = 2, M2: Int = 4, F2: Int = 3, C2: Int = 3, S2: Int = 4,
                          G1: Int = 1, N1: Int = 4, M1: Int = 2, F1: Int = 4, C1: Int = 2, S1: Int = 3,
                          M0: Int = 4, C0: Int = 2, R: Int = 4, E: Int = 2, N0: Int = 3, F0: Int = 1
                          ) {
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
      println(s"[INFO] the NN shape")
      println(s"                   |\ttype\t|\tnum\t|\tchn\t|\th\t|\tw\t|")
      println(s"                   |\tweight\t|\t${weight.number}\t|\t${weight.channel}\t|\t${weight.height}\t|\t${weight.width}\t|")
      println(s"                   |\tinAct\t|\t${inAct.number}\t|\t${inAct.channel}\t|\t${inAct.height}\t|\t${inAct.width}\t|")
      println(s"                   |\tpSum\t|\t${pSum.number}\t|\t${pSum.channel}\t|\t${pSum.height}\t|\t${pSum.width}\t|")
    }
  }
}

class EyerissModel(sequencer: GenFunc, monitor: CompareMonitor, p: NNModelMapping, printDetails: Boolean = true) extends PESizeConfig with SPadSizeConfig
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
  /** assume the data stored in Mem is pre-processed */
  private val inActAdrSRAMBanks = Array.fill(monitor.clusterNum, inActSRAMNum, inActAdrSRAMSize) {0}
  private val inActDataSRAMBanks = Array.fill(monitor.clusterNum, 3, inActDataSRAMSize) {0}
  private var parallelCycle = 0
  require(p.G1 == 1)
  // mapping <> physical
  require(p.S1 == peRowNum)
  require(p.F1 == peColNum) // TODO: use %peColNum == 0 instead
  require(p.M1*p.C1*p.N1*p.G1 == monitor.clusterNum)
  /** NoC level */
  private var inActSRAMBankWriteRecord: List[Seq[Int]] = Nil
  for (g1 <- 0 until p.G1) {
    for (n1 <- 0 until p.N1) {
      for (m1 <- 0 until p.M1) {
        for (f1 <- 0 until p.F1) {
          for (c1 <- 0 until p.C1) {
            for (s1 <- 0 until p.S1) {
              val weightNoCLevelIdx = g1*p.M1*p.C1*p.S1 + m1*p.C1*p.S1 + c1*p.S1 + s1
              val inActNoCLevelIdx = g1*p.N1*p.C1*(p.F1+p.S1) + n1*p.C1*(p.F1+p.S1) + c1*(p.F1+p.S1) + (f1+s1)
              val clusterIdx = n1*p.M1*p.C1 + m1*p.C1 + c1
              val bankIdx = (s1+f1)%3
              val inActSRAMBankWriteRecordSeq = Seq(clusterIdx, bankIdx)
              val formerOrLater = (s1+f1) < inActSRAMNum
              if (!inActSRAMBankWriteRecord.contains(inActSRAMBankWriteRecordSeq)) {
                /** as it needs write into SRAM Bank for former and later, so we need
                  * another NoCIdx*/
                val anotherNoCIdx = if (formerOrLater) inActNoCLevelIdx + inActSRAMNum else
                  inActNoCLevelIdx - inActSRAMNum
                if (formerOrLater) {
                  inActAdrSRAMBanks(clusterIdx)(bankIdx) =
                    (sequencer.dataSequencer.glb.cscData.inActAdr(inActNoCLevelIdx):::
                      sequencer.dataSequencer.glb.cscData.inActAdr(anotherNoCIdx)).toArray
                  inActDataSRAMBanks(clusterIdx)(bankIdx) =
                    (sequencer.dataSequencer.glb.cscData.inActData(inActNoCLevelIdx):::
                      sequencer.dataSequencer.glb.cscData.inActData(anotherNoCIdx)).toArray
                } else {
                  inActAdrSRAMBanks(clusterIdx)(bankIdx) =
                    (sequencer.dataSequencer.glb.cscData.inActAdr(anotherNoCIdx):::
                      sequencer.dataSequencer.glb.cscData.inActAdr(inActNoCLevelIdx)).toArray
                  inActDataSRAMBanks(clusterIdx)(bankIdx) =
                    (sequencer.dataSequencer.glb.cscData.inActData(anotherNoCIdx):::
                      sequencer.dataSequencer.glb.cscData.inActData(inActNoCLevelIdx)).toArray
                }
                // update scoreBoard
                /** the times read directly from mem in uncompressed data format */
                val inActMemReadNum = sequencer.dataSequencer.glb.inAct(inActNoCLevelIdx).flatten.flatten.length +
                  sequencer.dataSequencer.glb.inAct(anotherNoCIdx).flatten.flatten.length
                monitor.inActRead.mem += inActMemReadNum
                monitor.cycle += inActMemReadNum*scoreBoard.accessCost.mem
                val inActAdrGLBWriteNum = (sequencer.dataSequencer.glb.cscData.inActAdr(anotherNoCIdx).length
                  + sequencer.dataSequencer.glb.cscData.inActAdr(inActNoCLevelIdx).length)
                val inActDataGLBWriteNum = (sequencer.dataSequencer.glb.cscData.inActData(anotherNoCIdx).length
                  + sequencer.dataSequencer.glb.cscData.inActData(inActNoCLevelIdx).length)
                require(p.M1 == 2 || p.M1 == 1, "M1 needs equals to 2 or 1, or shouldn't divide M1 directly")
                /** divide M1, as inAct could be shared horizontally */
                monitor.inActWrite.adr.glb += inActAdrGLBWriteNum/p.M1
                monitor.inActWrite.data.glb += inActDataGLBWriteNum/p.M1
                inActSRAMBankWriteRecord = inActSRAMBankWriteRecord:::List(inActSRAMBankWriteRecordSeq)
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
                          /** only read once from Mem and send into several PEs */
                          if (f1 == 0 && n1 == 0) {
                            monitor.weightRead.mem += weightMemReadNum
                            parallelCycle += max(inActGLBReadNum*scoreBoard.accessCost.glb,
                              weightMemReadNum*scoreBoard.accessCost.mem)
                          } else {
                            parallelCycle += inActGLBReadNum*scoreBoard.accessCost.glb
                          }
                          /** read from GLB once and send into diagonal PEs
                            * and if m1 = 1 or more, then other cluster can receive inAct via Router */
                          if (formerOrLater) {
                            if (f1 == 0 && m1 == 0) {
                              monitor.inActRead.adr.glb += inActAdrSPad.length
                              monitor.inActRead.data.glb += inActDataSPad.length
                            }
                          } else {
                            if (f1 == inActSRAMNum && m1 == 0) {
                              monitor.inActRead.adr.glb += inActAdrSPad.length
                              monitor.inActRead.data.glb += inActDataSPad.length
                            }
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
                        }
                      }
                    }
                  }
                }
              }
              //print("*\n") // finish GLB Level
            }
          }
        }
      }
    }
  }
  monitor.cycle += parallelCycle/monitor.peNum
  if (printDetails) {
    monitor.printMonitorInfo()
    p.nnShape.printNNShapeInfo()
  }
}

class CommonModel(sequencer: GenFunc, monitor: CompareMonitor, p: NNModelMapping,
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
  monitor.cycle = scoreBoard.totalCycles(monitor.macNum, monitor.peNum, monitor.inActRead.mem, 0, 0)
  if (printDetails) {
    monitor.printMonitorInfo()
    p.nnShape.printNNShapeInfo()
  }
}

class ScalaModel extends FlatSpec {
  behavior of "compare the efficiency of Eyeriss"
  /** model the behavior of Eyeriss cluster group */
  it should "compare the info across sparse ratio between eyeriss and common device" in {
    val param = NNModelMapping()
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
        println(s"[INFO] current sparse ratio: 0.$inActRatio, 0.$weightRatio")
        if (inActRatio == inActRatioSeq.end - 1 && weightRatio == weightRatioSeq.end - 1)
          param.nnShape.printNNShapeInfo()
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
  val clusterNum: Int = 8 * 2 // 8 rows, 2 columns
  private val peArraySize = peRowNum * peColNum
  val peNum: Int = peArraySize * clusterNum
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
  def printMonitorInfo(): Unit = {
    val inActGLBWriteTotal = inActWrite.adr.glb + inActWrite.data.glb
    val inActGLBReadTotal = inActRead.adr.glb + inActRead.data.glb
    val inActSPadWriteTotal = inActWrite.adr.sPad + inActWrite.data.sPad
    val inActSPadReadTotal = inActRead.adr.sPad + inActRead.data.sPad
    println(s"[INFO] computation finishes, using $peNum PEs")
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
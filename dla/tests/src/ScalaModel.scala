package dla.tests

import dla.cluster.{ClusterSRAMConfig, GNMFCS1Config, GNMFCS2Config}
import dla.pe.{MCRENFConfig, PESizeConfig, SPadSizeConfig}
import org.scalatest._
import scala.math.max

class EyerissModel(sequencer: GenFunc, monitor: CompareMonitor, printDetails: Boolean = true) extends PESizeConfig with SPadSizeConfig
  with MCRENFConfig with GNMFCS1Config with GNMFCS2Config with ClusterSRAMConfig {
  /** use monitor to compare the info between different test*/
  private val scoreBoard = new ScoreBoard
  val pSumResult: Array[Array[Array[Array[Int]]]] = Array.fill(
    sequencer.nnShape.pSum.number,
    sequencer.nnShape.pSum.channel,
    sequencer.nnShape.pSum.height,
    sequencer.nnShape.pSum.width
  ) {0}
  /** assume the data stored in Mem is pre-processed */
  private val inActAdrSRAMBanks = Array.fill(monitor.clusterNum, inActSRAMNum, inActAdrSRAMSize) {0}
  private val inActDataSRAMBanks = Array.fill(monitor.clusterNum, 3, inActDataSRAMSize) {0}
  private var parallelCycle = 0
  require(G1 == 1)
  // mapping <> physical
  require(S1 == peRowNum)
  require(F1 == peColNum) // TODO: use %peColNum == 0 instead
  require(M1*C1*N1*G1 == monitor.clusterNum)
  /** NoC level */
  private var inActSRAMBankWriteRecord: List[Seq[Int]] = Nil
  for (g1 <- 0 until G1) {
    for (n1 <- 0 until N1) {
      for (m1 <- 0 until M1) {
        for (f1 <- 0 until F1) {
          for (c1 <- 0 until C1) {
            for (s1 <- 0 until S1) {
              val weightNoCLevelIdx = g1*M1*C1*S1 + m1*C1*S1 + c1*S1 + s1
              val inActNoCLevelIdx = g1*N1*C1*(F1+S1) + n1*C1*(F1+S1) + c1*(F1+S1) + (f1+s1)
              val clusterIdx = n1*M1*C1 + m1*C1 + c1
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
                require(M1 == 2 || M1 == 1, "M1 needs equals to 2 or 1, or shouldn't divide M1 directly")
                /** divide M1, as inAct could be shared horizontally */
                monitor.inActWrite.adr.glb += inActAdrGLBWriteNum/M1
                monitor.inActWrite.data.glb += inActDataGLBWriteNum/M1
                inActSRAMBankWriteRecord = inActSRAMBankWriteRecord:::List(inActSRAMBankWriteRecordSeq)
              }
              /** GLB level */
              for (g2 <- 0 until G2) {
                for (n2 <- 0 until N2) {
                  for (m2 <- 0 until M2) {
                    for (f2 <- 0 until F2) {
                      for (c2 <- 0 until C2) {
                        for (s2 <- 0 until S2) {
                          val weightGLBLevelIdx = g2*M2*C2*S2 + m2*C2*S2 + c2*S2 + s2
                          val inActGLBLevelIdx = g2*N2*C2*(F2 + S2) + n2*C2*(F2+S2) + c2*(F2+S2) + f2+s2
                          /** SPad level */
                          val inActAdrSPad = sequencer.dataSequencer.glb.separatedSPadCSCData.
                            inActAdr(inActNoCLevelIdx)(inActGLBLevelIdx)
                          val inActDataSPad = sequencer.dataSequencer.glb.separatedSPadCSCData.
                            inActData(inActNoCLevelIdx)(inActGLBLevelIdx)
                          val weightAdrSPad = sequencer.dataSequencer.glb.separatedSPadCSCData.
                            weightAdr(weightNoCLevelIdx)(weightGLBLevelIdx)
                          val weightDataSPad = sequencer.dataSequencer.glb.separatedSPadCSCData.
                            weightData(weightNoCLevelIdx)(weightGLBLevelIdx)
                          val pSumSPad: Array[Array[Int]] = Array.fill(F0*N0*E, M0) {0}
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
    sequencer.nnShape.printNNShapeInfo()
  }
}

class CommonModel(sequencer: GenFunc, monitor: CompareMonitor, printDetails: Boolean = true) extends PESizeConfig with SPadSizeConfig
  with MCRENFConfig with GNMFCS1Config with GNMFCS2Config with ClusterSRAMConfig {
  private val scoreBoard = new ScoreBoard
  val pSumResult: Array[Array[Array[Array[Int]]]] = Array.fill(
    sequencer.nnShape.pSum.number,
    sequencer.nnShape.pSum.channel,
    sequencer.nnShape.pSum.height,
    sequencer.nnShape.pSum.width
  ) {0}
  /** each PSum number */
  for (n <- 0 until sequencer.nnShape.pSum.number) {
    /** each PSum channel*/
    for (m <- 0 until sequencer.nnShape.pSum.channel) {
      /** PSum height */
      for (f <- 0 until sequencer.nnShape.pSum.width) {
        /** PSum width*/
        for (e <- 0 until sequencer.nnShape.pSum.height) {
          /** inside this for loop, do mac, for the size of weight matrix */
          /** weight channel */
          for (c <- 0 until sequencer.nnShape.weight.channel) {
            /** weight height */
            for (s <- 0 until sequencer.nnShape.weight.width) {
              /** weight width */
              for (r <- 0 until sequencer.nnShape.weight.height) {
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
      f"${((n + 1).toFloat/sequencer.nnShape.pSum.number.toFloat)*100}%.2f%%")*/
  }
  monitor.cycle = scoreBoard.totalCycles(monitor.macNum, monitor.peNum, monitor.inActRead.mem, 0, 0)
  if (printDetails) {
    monitor.printMonitorInfo()
    sequencer.nnShape.printNNShapeInfo()
  }
}

class ScalaModel extends FlatSpec {
  behavior of "compare the efficiency of Eyeriss"
  /** model the behavior of Eyeriss cluster group */
  it should "compare the info across sparse ratio between eyeriss and common device" in {
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
    for (inActRatio <- inActRatioSeq.start until inActRatioSeq.end) {
      for (weightRatio <- weightRatioSeq.start until weightRatioSeq.end) {
        val sequencer = new GenFunc(inActSparseRatio = inActRatio.toDouble/10,
          weightSparseRatio = weightRatio.toDouble/10)
        val eyerissModel = new EyerissModel(sequencer,
          monitorSeq(inActRatio - inActRatioSeq.start)(weightRatio - weightRatioSeq.start).head,
          printDetails = false)
        val common = new CommonModel(sequencer,
          monitorSeq(inActRatio - inActRatioSeq.start)(weightRatio - weightRatioSeq.start)(1),
          printDetails = false)
        println(s"[INFO] current sparse ratio: 0.$inActRatio, 0.$weightRatio")
      }
    }
    println("|iRa\t|wRa\t|cycle%\t\t|mac%\t\t|iMem%\t\t|wMem%\t\t|iGLB%\t\t|iSPad%\t\t|")
    for (inActRatio <- inActRatioSeq.start until inActRatioSeq.end) {
      for (weightRatio <- weightRatioSeq.start until weightRatioSeq.end) {
        val eyerissMonitor = monitorSeq(inActRatio - inActRatioSeq.start)(weightRatio - weightRatioSeq.start).head
        val commonMonitor = monitorSeq(inActRatio - inActRatioSeq.start)(weightRatio - weightRatioSeq.start).last
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
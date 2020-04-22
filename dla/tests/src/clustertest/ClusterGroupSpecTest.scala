package dla.tests.clustertest

import chisel3._
import chisel3.tester._
import chisel3.util.DecoupledIO
import chisel3.tester.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import dla.cluster._

import scala.util.Random

class ClusterGroupSpecTest extends ClusterSpecTestBasic {
  override val printLogDetails = false
  val theWeightAdrLookup: Seq[List[Int]] = theWeightAdrStreams.map(x => getStreamLookUp(x))
  val theWeightDataLookup: Seq[List[Int]] = theWeightDataStreams.map(x => getStreamLookUp(x))
  private def getSPadData(fullData: List[Int], theLookup: List[Int], idx: Int): List[Int] = {
    var currentSPad: List[Int] = Nil
    var endIdx = fullData.length
    if (idx < theLookup.length - 1) endIdx = theLookup(idx + 1)
    for (i <- theLookup(idx) until endIdx) {
      currentSPad = currentSPad:::List(fullData(i))
    }
    currentSPad
  }
  behavior of "test the spec of Cluster Group"
  it should "work well on Cluster Group Controller" in {
    test (new ClusterGroupController(debug = true)).withAnnotations(Seq(WriteVcdAnnotation)) { theCGCtrl =>
      val theTop = theCGCtrl.io
      val theDebugIO = theTop.debugIO
      val theClock = theCGCtrl.clock
      var inActReadAdr: List[Int] = Nil
      var pSumWriteAdr: List[Int] = Nil
      var pSumReadAdr: List[Int] = Nil
      def printCGState(stateInt: Int): Unit = {
        var state: String = "idle"
        if (stateInt == 0) state = "cgIdle"
        if (stateInt == 1) state = "cgLoadGLB"
        if (stateInt == 2) state = "cgLoadPE"
        if (stateInt == 3) state = "cgCal"
        if (stateInt == 4) state = "cgRead"
        if(printLogDetails) println(s"CG State   =  $state")
      }
      def randomGiveFin(pokeIO: Seq[Bool], expectIO: Seq[Bool], idx: Int, prefix: String): Unit = {
        theClock.step((new Random).nextInt(15))
        pokeIO(idx).poke(true.B)
        //println(s"[$prefix$idx] poke true now")
        theClock.step()
        pokeIO(idx).poke(false.B)
        expectIO(idx).expect(true.B, s"[$prefix$idx] one cycle later, the reg should be true")
        if (printLogDetails) {
          print(s"[$prefix$idx]")
          printCGState(theDebugIO.cgState.peek().litValue().toInt)
        }
      }
      def inActReadHelper(pokeIO: Seq[Bool], expectIO: Seq[Bool], idx: Int, prefix: String): Unit = {
        //println(s"[$prefix] inActReadAdr = ${theTop.glbInActCtrlIOs(idx).readIO.adr.peek()}")
        val currentAdr: Int = theTop.glbInActCtrlIOs(idx).readIO.adr.peek().litValue().toInt
        if (!inActReadAdr.contains(currentAdr)) {
          inActReadAdr = inActReadAdr:::List(currentAdr)
        }
        //theTop.glbInActCtrlIOs(idx).readIO.adr.expect() // TODO: check the address
        randomGiveFin(pokeIO, expectIO, idx, prefix)
        theTop.glbInActCtrlIOs(idx).readIO.enable.expect(false.B, s"as inAct $idx has been read from GLB")
      }
      def pSumRWHelper(pokeIO: Vec[SRAMCommonCtrlIO], expectIO: Seq[Bool], idx: Int, prefix: String): Unit = {
        //println(s"[$prefix] pSumReadAdr = ${theTop.glbPSumCtrlIOs(idx).readIO.adr.peek()}")
        //println(s"[$prefix] pSumWriteAdr = ${theTop.glbPSumCtrlIOs(idx).writeIO.adr.peek()}")
        val readAdr: Int = theTop.glbPSumCtrlIOs(idx).readIO.adr.peek().litValue().toInt
        val writeAdr: Int = theTop.glbPSumCtrlIOs(idx).writeIO.adr.peek().litValue().toInt
        if (!pSumReadAdr.contains(readAdr)) {
          pSumReadAdr = pSumReadAdr:::List(readAdr)
        }
        if (!pSumWriteAdr.contains(writeAdr)) {
          pSumWriteAdr = pSumWriteAdr:::List(writeAdr)
        }
        val writeDoneIO = pokeIO.map(x => x.writeIO.done)
        val readDoneIO = pokeIO.map(x => x.readIO.done)
        theClock.step((new Random).nextInt(15))
        readDoneIO(idx).poke(true.B)
        theClock.step()
        readDoneIO(idx).poke(false.B)
        theClock.step((new Random).nextInt(5))
        writeDoneIO(idx).poke(true.B)
        if(printLogDetails) println(s"[${prefix}Write$idx] poke true now")
        theClock.step()
        writeDoneIO(idx).poke(false.B)
        expectIO(idx).expect(true.B, s"[${prefix}Write$idx] one cycle later, the reg should be true")
        theTop.glbPSumCtrlIOs(idx).writeIO.enable.expect(false.B, s"as pSum $idx has been write into GLB")
        if (printLogDetails) println(s"[$prefix$idx] cgState = ${theDebugIO.cgState.peek().litValue()}")
      }
      theCGCtrl.reset.poke(true.B)
      theClock.step()
      theCGCtrl.reset.poke(false.B)
      println("----------------- test begin -----------------")
      println("-------- ClusterGroup Controller Spec --------")
      println("----------- test basic functions -------------")
      for (g2 <- 0 until G2) {
        theTop.allPSumAddFin.poke(false.B)
        theTop.allCalFin.poke(false.B)
        theTop.topIO.cgEnable.poke(true.B)
        theClock.step()
        theTop.topIO.cgEnable.poke(false.B)
        /** cgState = 1, load inAct into GLB from outside*/
        theDebugIO.cgState.expect(1.U, s"[$g2] cgState should be 1 to load data from outside into GLB")
        theTop.glbInActCtrlIOs.foreach(_.writeIO.enable.expect(true.B, "the GLB inAct write should enable now"))
        (1 until inActSRAMNum).foldLeft( fork {randomGiveFin(
          pokeIO = theTop.glbInActCtrlIOs.map(x => x.writeIO.done),
          expectIO = theDebugIO.inActWriteFinVecIO, idx = 0, prefix =  s"inActWrite")
        }) {
          case (left, right) =>
            left.fork {randomGiveFin(
              pokeIO = theTop.glbInActCtrlIOs.map(x => x.writeIO.done),
              expectIO = theDebugIO.inActWriteFinVecIO, idx = right , prefix = s"inActWrite")
            }
        }.joinAndStep(theClock) // wait for state machine
        for (n2 <- 0 until N2) {
          for (m2 <- 0 until M2) {
            for (f2 <- 0 until F2) {
              for (c2 <- 0 until C2) {
                for (s2 <- 0 until S2) {
                  /** cgState = 2, load inAct and weight into PE*/
                  if (printLogDetails) {
                    print(s"[$g2, $n2, $m2, $f2, $c2, $s2]")
                    printCGState(theDebugIO.cgState.peek().litValue().toInt)
                  }
                  theDebugIO.cgState.expect(2.U, s"[$c2,$s2] cgState should be 2 to load PE")
                  theTop.peCtrlIO.peLoadEn.expect(true.B, s"[$c2,$s2] should load data into PE")
                  theTop.glbInActCtrlIOs.foreach({x =>
                    x.writeIO.enable.expect(false.B, s"[$c2,$s2] should not write data into GLB")
                    x.readIO.enable.expect(true.B, s"[$c2,$s2] should read data from GLB")
                  })
                  (1 until inActSRAMNum).foldLeft( fork {
                    inActReadHelper( pokeIO = theTop.glbInActCtrlIOs.map(x =>x.readIO.done),
                      expectIO = theDebugIO.inActReadFinVecIO, idx = 0, prefix = s"($c2,$s2)@inActRead")
                  }) {
                    case (left, right) =>
                      left.fork {
                        inActReadHelper( pokeIO = theTop.glbInActCtrlIOs.map(x =>x.readIO.done),
                          expectIO = theDebugIO.inActReadFinVecIO, idx = right, prefix = s"$c2$s2@inActRead")
                      }
                  }.joinAndStep(theClock)
                  theDebugIO.inActReadFinVecIO.foreach(x => x.expect(false.B,
                    "need reset FinReg every time read finish"))
                  theTop.glbInActCtrlIOs.foreach(x => x.readIO.enable.expect(true.B,
                    "as inAct has only been read once, need read again"))
                  /** then read inAct again*/
                  (1 until inActSRAMNum).foldLeft( fork {
                    inActReadHelper( pokeIO = theTop.glbInActCtrlIOs.map(x =>x.readIO.done),
                      expectIO = theDebugIO.inActReadFinVecIO, idx = 0, prefix = s"($c2,$s2)@inActRead")
                  }) {
                    case (left, right) =>
                      left.fork {
                        inActReadHelper( pokeIO = theTop.glbInActCtrlIOs.map(x =>x.readIO.done),
                          expectIO = theDebugIO.inActReadFinVecIO, idx = right, prefix = s"$c2$s2@inActRead")
                      }
                  }.joinAndStep(theClock)
                  theDebugIO.inActReadFinVecIO.foreach(x => x.expect(false.B,
                    "needs reset FinReg every time read finish"))
                  theTop.glbInActCtrlIOs.foreach(x => x.readIO.enable.expect(false.B,
                    "as inAct has been read twice"))
                  /** cgState = 3, do computations inside PE*/
                  theDebugIO.cgState.expect(3.U, s"[$c2,$s2] after read inAct from GLB, it should do computation")
                  theClock.step((new Random).nextInt(15))
                  theTop.allCalFin.poke(true.B)
                  theClock.step()
                  theTop.allCalFin.poke(false.B)
                } // end of S2 loop
              } // end of C2 loop
              /** cgState = 4, read PSum from PE to GLB PSumSRAM */
              printCGState(theDebugIO.cgState.peek().litValue().toInt)
              theDebugIO.cgState.expect(4.U, s"after S2 = $S2 computations, it should read PSum Now")
              theTop.glbPSumCtrlIOs.foreach(_.readIO.enable.expect(true.B,
                s"[$g2, $n2, $m2, $f2] Should read PSum out from GLB now"))
              (1 until pSumSRAMNum).foldLeft( fork {
                pSumRWHelper( pokeIO = theTop.glbPSumCtrlIOs,
                  expectIO = theDebugIO.pSumWriteFinVecIO, idx = 0, prefix = s"@pSumWrite")
              }) {
                case (left, right) =>
                  left.fork {
                    pSumRWHelper( pokeIO = theTop.glbPSumCtrlIOs,
                      expectIO = theDebugIO.pSumWriteFinVecIO, idx = right, prefix = s"@pSumWrite")
                    theDebugIO.cgState.expect(4.U, s"[$g2, $n2, $m2, $f2] it should be cgRead")
                  }
              }.join()
              theTop.allPSumAddFin.poke(true.B)
              theClock.step() // wait for state machine
              theTop.allPSumAddFin.poke(false.B)
            } // end of F2 loop
          } // end of M2 loop
        } // end of N2 loop
        theDebugIO.cgState.expect(0.U, s" g2 = $g2, it should be cgIdle again")
      } // end of G2 loop
      println(s"inActReadAdr = \n $inActReadAdr")
      println(s"pSumWriteAdr = \n $pSumWriteAdr")
      println(s"pSumReadAdr = \n $pSumReadAdr")
      println("----------------- test success -----------------")
    }
  }

  it should "work well on reading and writing via inner SRAM" in {
    test (new ClusterGroup(true)).withAnnotations(Seq(WriteVcdAnnotation)) { theCG =>
      val theTop = theCG.io
      val theClock = theCG.clock
      def pokeData(pokeIO: DecoupledIO[UInt], pokeData: List[Int], prefix: String): Unit = {
        var pokeIdx = 0
        while (pokeIdx < pokeData.length) {
          pokeIO.bits.poke(pokeData(pokeIdx).U)
          pokeIO.valid.poke(true.B)
          if (pokeIO.ready.peek().litToBoolean) {
            if (printLogDetails) println(s"[$prefix@$pokeIdx] poked ${pokeData(pokeIdx)}")
            pokeIdx += 1
          } else {
            println(Console.RED + s"[WARING] [$prefix@$pokeIdx] not ready now" + Console.RESET)
          }
          theClock.step()
        }
        pokeIO.valid.poke(false.B)
      }
      theCG.reset.poke(true.B)
      theClock.step()
      theCG.reset.poke(false.B)
      println("----------------- test begin -----------------")
      println("---------- Cluster Group Top Spec ------------")
      println("----------- test basic functions -------------")
      theTop.ctrlPath.routerClusterCtrl.inActCtrlSel.inDataSel.poke(0.U) // from inAct SRAM bank
      theTop.ctrlPath.routerClusterCtrl.inActCtrlSel.outDataSel.poke(0.U) // uni-cast
      theTop.ctrlPath.routerClusterCtrl.weightCtrlSel.inDataSel.poke(false.B) // from GLB Cluster
      theTop.ctrlPath.routerClusterCtrl.weightCtrlSel.outDataSel.poke(false.B) // don't send to its neighborhood
      theTop.ctrlPath.routerClusterCtrl.pSumCtrlSel.inDataSel.poke(true.B) // from PSum SRAM bank
      theTop.ctrlPath.routerClusterCtrl.pSumCtrlSel.outDataSel.poke(true.B) // send it to PE Array
      theTop.ctrlPath.peClusterCtrl.inActSel.inDataSel.poke(false.B) // don't broad-cast inAct
      theTop.ctrlPath.peClusterCtrl.inActSel.outDataSel.poke(0.U) // Don't care
      theTop.ctrlPath.peClusterCtrl.pSumInSel.poke(true.B) // load PSum from Router
      theClock.step()
      theTop.ctrlPath.doMacEn.poke(true.B)
      theClock.step()
      theTop.ctrlPath.doMacEn.poke(false.B)
      theClock.step()
      /** it should poke with signal thread as it read data via memory bus */
      /** each inAct SRAM needs store two streams of data
        * TODO: remove the 00 at the end of first stream so that it can poke later stream */
      fork.withName("pokeInActAdr") {
        (1 until inActSRAMNum).foldLeft(
          fork {
            val prefix: String = s"inActAdr0"
            val pokeIO = theTop.dataPath.glbDataPath.inActIO.head.inIOs.adrIOs.data
            pokeData(pokeIO, theInActAdrStreams.head, prefix)
            println(s"[$prefix] poke later")
            //pokeData(pokeIO, theInActAdrStreams(inActRouterNum), s"${prefix}Later")
          }) {
          case (left, right) =>
            left.fork {
              val prefix: String = s"inActAdr$right"
              val pokeIO = theTop.dataPath.glbDataPath.inActIO(right).inIOs.adrIOs.data
              pokeData(pokeIO, theInActAdrStreams(right), prefix)
              println(s"[$prefix] poke later")
              //pokeData(pokeIO, theInActAdrStreams(right+inActRouterNum), s"${prefix}Later")
            }
        }.join()
      } .fork.withName("pokeInActData") {
        (1 until inActSRAMNum).foldLeft(
          fork {
            val prefix: String = s"inActData0"
            val pokeIO = theTop.dataPath.glbDataPath.inActIO.head.inIOs.dataIOs.data
            pokeData(pokeIO, theInActDataStreams.head, prefix)
            println(s"[$prefix] poke later")
            //pokeData(pokeIO, theInActDataStreams(inActRouterNum), prefix)
          }) {
          case (left, right) =>
            left.fork {
              val prefix: String = s"inActData$right"
              val pokeIO = theTop.dataPath.glbDataPath.inActIO(right).inIOs.dataIOs.data
              pokeData(pokeIO, theInActDataStreams(right), prefix)
              println(s"[$prefix] poke later")
              //pokeData(pokeIO, theInActDataStreams(right+inActRouterNum), prefix)
            }
        }.join()
      } .joinAndStep(theClock)
      println("when it begins to cal, when it will need weight")
      theClock.step()
      theTop.ctrlPath.peWeightLoadEn.expect(true.B, "peLoadEn should be true to load weight")
      var weightReadFin = false
      var weightReadAdr = 0
      var weightReadTimes = 0
      for (g2 <- 0 until G2) {
        for (n2 <- 0 until N2) {
          for (m2 <- 0 until M2) {
            for (f2 <- 0 until F2) {
              for (c2 <- 0 until C2) {
                for (s2 <- 0 until S2) {
                  weightReadAdr = g2*M2*C2*S2 + m2*C2*S2 + c2*S2 + s2
                  while (!theTop.ctrlPath.peWeightLoadEn.peek().litToBoolean || weightReadFin) {
                    /** while not enable or has read current weight */
                    theClock.step()
                    if (!theTop.ctrlPath.peWeightLoadEn.peek().litToBoolean) weightReadFin = false
                  }
                  val weightAdrOneSPad = theWeightAdrStreams.zip(theWeightAdrLookup).map({ case (ints, ints1) =>
                    getSPadData(ints, ints1, idx = weightReadAdr)
                  })
                  val weightDataOneSPad = theWeightDataStreams.zip(theWeightDataLookup).map({ case (ints, ints1) =>
                    getSPadData(ints, ints1, idx = weightReadAdr)
                  })
                  fork.withName("pokeWeightAdr") {
                    (1 until weightRouterNum).foldLeft(
                      fork {
                        val prefix: String = s"${weightReadTimes}weightAdr0@$weightReadAdr"
                        val pokeIO = theTop.dataPath.glbDataPath.weightIO.head.inIOs.adrIOs.data
                        pokeData(pokeIO, weightAdrOneSPad.head, prefix)
                      }) {
                      case (left, right) =>
                        left.fork {
                          val prefix: String = s"${weightReadTimes}weightAdr$right@$weightReadAdr"
                          val pokeIO = theTop.dataPath.glbDataPath.weightIO(right).inIOs.adrIOs.data
                          pokeData(pokeIO, weightAdrOneSPad(right), prefix)
                        }
                    }.join()
                  } .fork.withName("pokeWeightData") {
                    (1 until weightRouterNum).foldLeft(
                      fork {
                        val prefix: String = s"${weightReadTimes}weightData0@$weightReadAdr"
                        val pokeIO = theTop.dataPath.glbDataPath.weightIO.head.inIOs.dataIOs.data
                        pokeData(pokeIO, weightDataOneSPad.head, prefix)
                      }) {
                      case (left, right) =>
                        left.fork {
                          val prefix: String = s"${weightReadTimes}weightData$right@$weightReadAdr"
                          val pokeIO = theTop.dataPath.glbDataPath.weightIO(right).inIOs.dataIOs.data
                          pokeData(pokeIO, weightDataOneSPad(right), prefix)
                        }
                    }.join()
                  } .join()
                  println(s"[$weightReadTimes@$weightReadAdr] now finish one pe load. " +
                    s"($g2/$G2, $n2/$N2, $m2/$M2, $f2/$F2, $c2/$C2, $s2/$S2)")
                  weightReadFin = true
                  weightReadTimes += 1
                }
              }
            }
          }
        }
      }
      println("now wait for calFin")
      while (!theTop.ctrlPath.calFin.peek().litToBoolean) {
        theClock.step()
      }
      println("All cal finish now")
      theClock.step(100)
    }
  }
}

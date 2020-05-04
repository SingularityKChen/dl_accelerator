package dla.tests.clustertest

import chisel3._
import chisel3.tester._
import chisel3.util.DecoupledIO
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}
import chisel3.tester.experimental.TestOptionBuilder._
import dla.cluster._
import dla.pe.StreamBitsIO

import scala.util.Random
import scala.util.matching.Regex

class PEClusterSpecTest extends ClusterSpecTestBasic {
  override val printLogDetails = false
  behavior of "test the spec of PE Cluster"
  it should "work well on PE inAct Controller" in {
    test (new PEClusterInAct(debug = true))
      .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation))
      { thePEAct =>
      val theTopIO = thePEAct.io
      val theClock = thePEAct.clock
      val theTopToCtrlDataIO = theTopIO.inActToArrayData.inActIO // inActRouter number
      val theCtrlToPEDataIO = theTopIO.inActToArrayData.muxInActData // (peRow, peCol)
      val theCtrlIO = theTopIO.inActCtrlSel
      val theDoneIO = theTopIO.inActWriteFinVec // (peRow, peCol)
      val theDebugIO = theTopIO.debugIO
      def singleThreadPokePeek(adrOrData: Int, peekCon: (Int, Int) => Boolean): Unit = {
        val prefix: Seq[String] = Seq("adr", "data")
        for (i <- 0 until inActRouterNum) {
          println(s"[${prefix(adrOrData)}] poke inActPort $i now")
          val pokeIO = Seq(theTopToCtrlDataIO(i).adrIOs, theTopToCtrlDataIO(i).dataIOs)
          pokeIO(adrOrData).data.bits.poke(i.U)
          pokeIO(adrOrData).data.valid.poke(true.B)
        }
        for (row <- 0 until peRowNum) {
          for (col <- 0 until peColNum) {
            val inActIdx = (row + col) % inActRouterNum
            val peekIO = Seq(theCtrlToPEDataIO(row)(col).adrIOs, theCtrlToPEDataIO(row)(col).dataIOs)
            if (peekCon(row, col)) {
              println(s"[${prefix(adrOrData)}] peek pe[$row][$col] now with data from inAct $inActIdx")
              peekIO(adrOrData).data.bits.expect(inActIdx.U)
              peekIO(adrOrData).data.valid.expect(true.B)
              peekIO(adrOrData).data.ready.poke(true.B)
            } else {
              peekIO(adrOrData).data.valid.expect(false.B)
            }
          }
        }
        println(s"[${prefix(adrOrData)}] peek ready signal now")
        theTopToCtrlDataIO.foreach(x => x.adrIOs.data.ready.expect(true.B))
        theClock.step()
      }
      def formerCon(row: Int, col: Int): Boolean = {
        val condition = (row + col) < inActRouterNum
        condition
      }
      def laterCon(row: Int, col: Int): Boolean = {
        !formerCon(row, col)
      }
      println("----------------- test begin -----------------")
      println("----------- PE Cluster Ctrl Spec -------------")
      println("---------- test basic connections-------------")
      thePEAct.reset.poke(true.B)
      theClock.step()
      thePEAct.reset.poke(false.B)
      theClock.step()
      println("--------------- begin to load ----------------")
      theCtrlIO.inDataSel.poke(false.B) // don't broad-cast
      // test inAct
      theDebugIO.inActDataIOState.foreach( x => x.expect(0.U, "inAct should be former now"))
      singleThreadPokePeek(0, formerCon)
      singleThreadPokePeek(1, formerCon)
      // assume that the first group has been finished
      for (row <- 0 until peRowNum) {
        for (col <- 0 until peColNum) {
          if (formerCon(row, col)) {
            println(s"pe[$row][$col] finishes now")
            theDoneIO(row)(col).adrWriteFin.poke(true.B)
            theDoneIO(row)(col).dataWriteFin.poke(true.B)
          }
        }
      }
      theDebugIO.inActDataIOState.foreach( x => x.expect(0.U, "inAct should be former now"))
      theClock.step() // inActWriteDoneRegVec from false to true, and inActDataStateJumpWires is true now
      theClock.step() // inActDataIOStateRegs jump from inActLoadFormer to inActLoadLater
      theDebugIO.inActDataIOState.foreach( x => x.expect(1.U, "inAct should be later now"))
      singleThreadPokePeek(0, laterCon)
      singleThreadPokePeek(1, laterCon)
      for (row <- 0 until peRowNum) {
        for (col <- 0 until peColNum) {
          if (laterCon(row, col)) {
            println(s"pe[$row][$col] finishes now")
            theDoneIO(row)(col).adrWriteFin.poke(true.B)
            theDoneIO(row)(col).dataWriteFin.poke(true.B)
          }
        }
      }
    }
  }

  it should "work well on PE Cluster" in {
    test (new PECluster(true))
      .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation))
    { thePECluster =>
      val theTopIO = thePECluster.io
      val theClock = thePECluster.clock
      val theCtrlIO = theTopIO.ctrlPath
      val theDebugIO = theTopIO.debugIO
      val pSumDataIO = theTopIO.dataPath.pSumIO
      val inActDataIO = theTopIO.dataPath.inActIO
      val weightDataIO = theTopIO.dataPath.weightIO
      val theInActAdrLookup: Seq[List[Int]] = theInActAdrStreams.map(x => getStreamLookUp(x))
      val theInActDataLookup: Seq[List[Int]] = theInActDataStreams.map(x => getStreamLookUp(x))
      val theWeightAdrLookup: Seq[List[Int]] = theWeightAdrStreams.map(x => getStreamLookUp(x))
      val theWeightDataLookup: Seq[List[Int]] = theWeightDataStreams.map(x => getStreamLookUp(x))
      val inActAdrReadIdx: Array[Int] = Array.fill(inActRouterNum*2){0}
      val inActDataReadIdx: Array[Int] = Array.fill(inActRouterNum*2){0}
      val weightAdrReadIdx: Array[Int] = Array.fill(weightRouterNum){0}
      val weightDataReadIdx: Array[Int] = Array.fill(weightRouterNum){0}
      /** whetherInActDone: first six for former, last six for later
        * and the half three represent adr, next half represent data*/
      val whetherInActDone = Array.fill(inActRouterNum*4){false} // used to see whether each router inAct finishes
      def writeCSCPECluster(theInIO: StreamBitsIO, theStream: List[Int]): Unit = {
        var idx = 0
        while (theStream(idx) != 0) {
          println(s"------------- write cycle $idx --------------")
          theInIO.data.bits.poke(theStream(idx).U)
          theInIO.data.valid.poke(true.B)
          theInIO.data.ready.expect(false.B)
          theClock.step()
          idx = idx + 1
        }
        println(s"------------- write cycle $idx --------------")
        theInIO.data.bits.poke(theStream(idx).U)
        theInIO.data.valid.poke(true.B)
        theInIO.data.ready.expect(false.B)
        theClock.step()
        // then should be in cal
      }
      def forkPSumHelper(idx: Int): Unit = {
        theClock.step((new Random).nextInt(10) + 1)
        var pSumList: List[Int] = Nil
        for (pSumRdIdx <- 0 until pSumOneSPadNum) {
          pSumDataIO.inIOs(idx).valid.poke(true.B)
          pSumDataIO.inIOs(idx).bits.poke(addendRand(idx)(pSumRdIdx).U)
          pSumDataIO.outIOs(idx).ready.poke(true.B)
          //println(s"[pSumOut@Router$idx]readOutData$pSumRdIdx = ${pSumDataIO.outIOs(idx).bits.peek()}")
          pSumList = pSumList:::List(pSumDataIO.outIOs(idx).bits.peek().litValue().toInt)
          theClock.step()
        }
        println(s"pSum$idx = $pSumList")
        println(s"-------- $idx-th Column PEs receive all inPSum")
      }
      def singleThreadWriteOneSCS(thePokeIO: IndexedSeq[DecoupledIO[UInt]], theLookup: Seq[List[Int]],
                                  theStreamReadIdx: Array[Int], thePokeStream: Seq[List[Int]],
                                  theRouterNumber: Int, conFunc: (Int, Int) => Boolean,
                                  thePrefix: String): Unit = {
        val regInActOrWeight = new Regex("(I|i)nAct")
        val regDataOrAdr = new Regex("(D|d)ata")
        val itIsInAct = regInActOrWeight.pattern.matcher(thePrefix).find()
        val itIsData = regDataOrAdr.pattern.matcher(thePrefix).find()
        /** inActAdr, writeFinRegVec's idx = 0, inActData, idx = 1, weightAdr, idx = 2, weightData, idx = 3*/
        val writeFinRegIdx: Int = (if (itIsInAct) 0 else 1)*2 + (if (itIsData) 1 else 0)
        val formerOrLater: Boolean = conFunc(0, 0) // if 0 + 0 ? InActRouter == true, then it's former
        /** use x(1).max to make sure it can poke all the data */
        for (_ <- 0 until theLookup.map(x => x(1)).max) {
          /** expect the inActIO state*/
          if (itIsInAct) {
            theDebugIO.inActDataIOState.zipWithIndex.foreach({ case (int, i) =>
              if (formerOrLater) {
                if (!(whetherInActDone(i) && whetherInActDone(i+inActRouterNum))) {
                  int.expect(0.U, s"[$thePrefix@R$i] should it be former or later?\n " +
                    s"adr = ${whetherInActDone(i)}\n" +
                    s"data = ${whetherInActDone(i+inActRouterNum)}")
                } else {
                  /*timescope {
                    theClock.step()
                    println(s"[$thePrefix@R$i] t1 inActState = ${int.peek()}")
                    int.expect(1.U, s"[$thePrefix@R$i] should it be former or later?\n " +
                      s"adr = ${whetherInActDone(i)}\n" +
                      s"data = ${whetherInActDone(i+inActRouterNum)}")
                  }*/
                }
              } else {
                if (!(whetherInActDone(i+inActRouterNum*2) && whetherInActDone(i+inActRouterNum*3))) {
                  int.expect(1.U, s"[$thePrefix@R$i] should it be former or later?\n " +
                    s"adr = ${whetherInActDone(i+inActRouterNum*2)}\n" +
                    s"data = ${whetherInActDone(i+inActRouterNum*3)}")
                } else {
                  /*timescope {
                    theClock.step()
                    println(s"[$thePrefix@R$i] t1 inActState = ${int.peek()}")
                    int.expect(0.U, s"[$thePrefix@R$i] should it be former or later?\n " +
                      s"adr = ${whetherInActDone(i+inActRouterNum*2)}\n" +
                      s"data = ${whetherInActDone(i+inActRouterNum*3)}")
                  }*/
                }
              }
            })
          }
          /** poke and check*/
          for (routerIdx <- 0 until theRouterNumber) {
            val lastPoke: Boolean = thePokeStream(routerIdx)(theStreamReadIdx(routerIdx)) == 0
            val prefix: String = s"$thePrefix@Router$routerIdx@Idx${theStreamReadIdx(routerIdx)}"
            if (lastPoke && itIsInAct) {
              if (formerOrLater) {
                whetherInActDone(if (!itIsData) routerIdx else routerIdx + inActRouterNum) = true
              } else {
                whetherInActDone(if (!itIsData) routerIdx+inActRouterNum*2 else routerIdx+inActRouterNum*3) = true
              }
            }
            /**  theLookup(routerIdx)(1) means the start idx of second stream,
              * then it can poke data until the end of first stream*/
            if (theStreamReadIdx(routerIdx) < theLookup(routerIdx)(1)) {
              if (printLogDetails)
                println(s"[$prefix] poke bits = ${thePokeStream(routerIdx)(theStreamReadIdx(routerIdx))}")
              thePokeIO(routerIdx).bits.poke(thePokeStream(routerIdx)(theStreamReadIdx(routerIdx)).U)
              thePokeIO(routerIdx).valid.poke(true.B)
              thePokeIO(routerIdx).ready.expect(true.B)
              theStreamReadIdx(routerIdx) += 1
            } else {
              thePokeIO(routerIdx).valid.poke(false.B)
              theDebugIO.eachPETopDebug.zipWithIndex.foreach({ case (os, row) =>
                os.zipWithIndex.foreach({ case (o, col) =>
                  if (row + col % inActRouterNum == routerIdx && itIsInAct) {
                    /*if (formerOrLater) {
                      o.writeFinishRegVec(writeFinRegIdx).expect(conFunc(row, col).B,
                        s"[$prefix] should $writeFinRegIdx done?")
                    } else {
                      o.writeFinishRegVec(writeFinRegIdx).expect(true.B,
                        s"[$prefix] should $writeFinRegIdx done?")
                    }*/
                  }
                })})
              println(s"[$thePrefix@Router$routerIdx] have poked" +
                s" ${theStreamReadIdx(routerIdx) - 1} data, the last one is " +
                s"${thePokeStream(routerIdx)(theStreamReadIdx(routerIdx) - 1)}")
            }
          }
          /** check finish reg*/
          if (itIsInAct) {
            for (row <- 0 until peRowNum) {
              for (col <- 0 until peColNum) {
                val routerIdx = (row + col) % inActRouterNum
                val lastPoke: Boolean = thePokeStream(routerIdx)(theStreamReadIdx(routerIdx) - 1) == 0
                val validIdx: Int = if (itIsData) 1 else 0
                val whetherValid: Boolean = conFunc(row, col) &&
                  !theDebugIO.eachPETopDebug(row)(col).writeFinishRegVec(validIdx).peek().litToBoolean
                if (lastPoke) {
                  /*if (formerOrLater) {
                    theDebugIO.eachPETopDebug(row)(col).writeFinishRegVec(validIdx).expect(conFunc(row, col).B,
                      s"[$thePrefix@Router$routerIdx@Row$row@Col$col] should it finish?")
                  } else {
                    theDebugIO.eachPETopDebug(row)(col).writeFinishRegVec(validIdx).expect(true.B,
                      s"[$thePrefix@Router$routerIdx@Row$row@Col$col] should it finish?")
                  }*/
                  if (printLogDetails) println(s"[$thePrefix@Router$routerIdx@Row$row@Col$col] " +
                    s"DataValid ${theDebugIO.eachPEInActValid(validIdx)(row)(col).peek()}\n" +
                    s"[$thePrefix@Router$routerIdx@Row$row@Col$col] " +
                    s"writeFinReg = ${theDebugIO.eachPETopDebug(row)(col).writeFinishRegVec(validIdx).peek()}")
                  /*theDebugIO.eachPEInActValid(validIdx)(row)(col).expect(whetherValid.B,
                    s"[$thePrefix@Router$routerIdx@Row$row@Col$col] should it Valid now? " +
                      s"writeFinReg = ${theDebugIO.eachPETopDebug(row)(col).writeFinishRegVec(validIdx).peek()}")*/
                  println(s"[$thePrefix@Router$routerIdx] ioState = ${theDebugIO.inActDataIOState(routerIdx).peek()}")
                } else {
                  theDebugIO.eachPEInActValid(validIdx)(row)(col).expect(conFunc(row, col).B,
                    s"[$thePrefix@Router$routerIdx@Row$row@Col$col] should it Valid now? " +
                      s"writeFinReg = ${theDebugIO.eachPETopDebug(row)(col).writeFinishRegVec(validIdx).peek()}")
                }
              }
            }
          }
          theClock.step()
          thePokeIO.foreach(x => x.valid.poke(false.B))
        }
        println(s"[$thePrefix] poke finishes now")
      }
      def expectInActWF(theRFRegVecIdx: Int, conFunc: (Int, Int) => Boolean,
                        conMessage: String, elseMessage: String): Unit = {
        val regDataOrAdr = new Regex("(D|d)ata")
        val itIsData = regDataOrAdr.pattern.matcher(conMessage).find()
        val prefix: String = if (itIsData) "data" else "adr"
        for (row <- 0 until peRowNum) {
          for (col <- 0 until peColNum) {
            if(printLogDetails)
              println(s"[inAct$prefix@$row$col] " +
                s"${theDebugIO.eachPETopDebug(row)(col).writeFinishRegVec(theRFRegVecIdx).peek()}")
            /*if (conFunc(row, col)) {
              theDebugIO.eachPETopDebug(row)(col).writeFinishRegVec(theRFRegVecIdx).expect(true.B, conMessage)
            } else {
              theDebugIO.eachPETopDebug(row)(col).writeFinishRegVec(theRFRegVecIdx).expect(false.B, elseMessage)
            }*/
          }
        }
      }
      def formerCon(row: Int, col: Int): Boolean = {
        val condition = (row + col) < inActRouterNum
        condition
      }
      def laterCon(row: Int, col: Int): Boolean = {
        !formerCon(row, col)
      }
      println("----------------- test begin -----------------")
      println("------------ PE Cluster Top Spec -------------")
      println("----------- test basic functions -------------")
      thePECluster.reset.poke(true.B)
      theClock.step()
      thePECluster.reset.poke(false.B)
      theClock.step()
      println("--------------- begin to load ----------------")
      theCtrlIO.pSumCtrlSel.inDataSel.poke(true.B) // receive data from PSum Router
      theCtrlIO.inActCtrlSel.inDataSel.poke(false.B) // not broad-cast
      theCtrlIO.doEn.poke(true.B) // begin to load inAct and weight, then cal
      theClock.step()
      fork.withName("inActLoadThread") { // inActLoad
        val prefixNeedContainsInAct = new Regex("(I|i)nAct") // used for require statements
        fork.withName("inActAdrLoadThread") { // address
          val theAdrIO = inActDataIO.map(x => x.adrIOs.data)
          val thePrefix = "inActAdr"
          require(prefixNeedContainsInAct.pattern.matcher(thePrefix).find(), "the prefix need contains " +
            s"'inAct' or 'InAct', but $thePrefix.")
          singleThreadWriteOneSCS(theAdrIO, theInActAdrLookup.take(inActRouterNum),
            inActAdrReadIdx.take(inActRouterNum), theInActAdrStreams.take(inActRouterNum),
            inActRouterNum, conFunc = formerCon, thePrefix = thePrefix)
          expectInActWF(theRFRegVecIdx = 0, formerCon,
            conMessage = "former PEs have finished address", elseMessage = "later PEs haven't begin poke address yet")
        } .fork.withName("inActDataLoadThread") { //data
          val theDataIO = inActDataIO.map(x => x.dataIOs.data)
          val thePrefix = "inActData"
          require(prefixNeedContainsInAct.pattern.matcher(thePrefix).find(), "the prefix need contains " +
            s"'inAct' or 'InAct', but $thePrefix.")
          singleThreadWriteOneSCS(theDataIO, theInActDataLookup.take(inActRouterNum),
            inActDataReadIdx.take(inActRouterNum), theInActDataStreams.take(inActRouterNum),
            inActRouterNum, conFunc = formerCon, thePrefix = thePrefix)
          expectInActWF(theRFRegVecIdx = 1, formerCon,
            conMessage = "former PEs have finished data", elseMessage = "later PEs haven't begin poke data yet")
        } .joinAndStep(theClock) // wait a cycle for inActState jump to later
        theDebugIO.eachPETopDebug.flatten.zipWithIndex.foreach({ case (o, i) =>
          println(s"[inActAdr$i] writeFin = ${o.writeFinishRegVec.head.peek()}")
          println(s"[inActData$i] writeFin = ${o.writeFinishRegVec(1).peek()}")
        })
        /** now poke and test later ones of inAct */
        fork {
          val theAdrIO = inActDataIO.map(x => x.adrIOs.data)
          val thePrefix = "inActAdr"
          println("------------ inActAdr Poke Later -------------")
          singleThreadWriteOneSCS(theAdrIO, theInActAdrLookup.takeRight(inActRouterNum),
            inActAdrReadIdx.takeRight(inActRouterNum), theInActAdrStreams.takeRight(inActRouterNum),
            inActRouterNum, conFunc = laterCon, thePrefix = thePrefix)
        } .fork {
          val theDataIO = inActDataIO.map(x => x.dataIOs.data)
          val thePrefix = "inActData"
          println("------------ inActData Poke Later ------------")
          singleThreadWriteOneSCS(theDataIO, theInActDataLookup.takeRight(inActRouterNum),
            inActDataReadIdx.takeRight(inActRouterNum), theInActDataStreams.takeRight(inActRouterNum),
            inActRouterNum, conFunc = laterCon, thePrefix = thePrefix)
        } .join()
      } .fork.withName("weightLoadThread") { // weightLoad
        theClock.step((new Random).nextInt(20))
        fork.withName("weightAdrLoadThread") {
          val theAdrIO = weightDataIO.map(x => x.adrIOs.data)
          singleThreadWriteOneSCS(theAdrIO, theWeightAdrLookup, weightAdrReadIdx,
            theWeightAdrStreams, weightRouterNum, conFunc = formerCon, thePrefix = "weightAdr")
        } .fork.withName("weightDataLoadThread") {
          val theDataIO = weightDataIO.map(x => x.dataIOs.data)
          singleThreadWriteOneSCS(theDataIO, theWeightDataLookup, weightDataReadIdx,
            theWeightDataStreams, weightRouterNum, conFunc = formerCon, thePrefix = "weightData")
        } .join()
      } .joinAndStep(theClock) // wait one cycle for pe state jump to cal
      theDebugIO.eachPETopDebug.flatten.zipWithIndex.foreach({ case (o, i) =>
        println(s"[inActAdr$i] writeFin = ${o.writeFinishRegVec.head.peek()}")
        println(s"[inActData$i] writeFin = ${o.writeFinishRegVec(1).peek()}")
        println(s"[weightAdr$i] writeFin = ${o.writeFinishRegVec(2).peek()}")
        println(s"[weightData$i] writeFin = ${o.writeFinishRegVec(3).peek()}")
      })
      theDebugIO.eachPETopDebug.flatten.foreach(x => x.peControlDebugIO.peState.expect(2.U,
        "after finish, each pe should do computing now"))
      while (!theCtrlIO.allCalFin.peek().litToBoolean) {
        theDebugIO.eachPETopDebug.zipWithIndex.foreach({ case (ios, row) =>
          ios.zipWithIndex.foreach({ case (debugIO, col) =>
            val prefix: String = s"pe@Row$row@Col$col"
            println(s"[$prefix] SPad State = ${debugIO.peSPadDebugIO.sPadState.peek()}")
            println(s"[$prefix] pSumResult = ${debugIO.peSPadDebugIO.pSumResult.peek()}")
          })})
        theClock.step()
      }
      /** After all the PEs finish computation, then begin to read out pSum*/
      println("----------- begin to readout PSum ------------")
      theCtrlIO.pSumLoadEn.poke(true.B) // begin to accumulate PSum
      (1 until peColNum).foldLeft(fork(forkPSumHelper(0))) {
        case (left, right) => left.fork(forkPSumHelper(idx = right))
      }.join()
      val theOrWeights = oneStreamData.weightStreamGLBOrder
      val theOrInActs = oneStreamData.inActStreamGLBOrder
      def getData(stream: Seq[List[Int]]): Seq[List[Int]] = {
        val dataStream: Seq[List[Int]] = stream.map(x => x.map(y => Integer.parseInt(y.toBinaryString.take(8), 2)))
        dataStream
      }
      /*for (i <- 0 until pSumRouterNum) {
        var realColPSum: List[Int] = Nil
        for (pSumRow <- theOrWeights.head.indices) {
          for (pSumCol <- theOrInActs.head.head.indices) {
            var pSum = 0
            for (addTimes <- theOrInActs.head.indices) {
              for (row <- 0 until peRowNum) {
                pSum += theOrWeights(row)(pSumRow)(addTimes) * theOrInActs(row+i)(addTimes)(pSumCol)
              }
            }
            pSum += addendRand(i)(pSumRow*theOrInActs.head.head.length + pSumCol)
            realColPSum = realColPSum:::List(pSum)
          }
        }
        println(s"pSum$i = ")
        println(realColPSum)
      }*/
      theOrInActs.take(inActRouterNum*2).zipWithIndex.foreach({ case (list, i) => println(s"goldenInAct$i = $list")})
      println(s"pokeInAct = ${getData(theInActDataStreams).zip(theInActAdrStreams)}")
      theOrWeights.take(peRowNum).zipWithIndex.foreach({ case (list, i) => println(s"goldenWeight$i = $list")})
      println(s"pokeWeight = ${getData(theWeightDataStreams).zip(theWeightAdrStreams)}")
    }
  }
}

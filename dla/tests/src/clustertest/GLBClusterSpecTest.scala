package dla.tests.clustertest

import chisel3._
import chisel3.tester._
import chisel3.util.DecoupledIO
import chisel3.tester.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import dla.cluster._
import dla.pe.{CSCStreamIO, StreamBitsIO}

import scala.util.Random

class GLBClusterSpecTest extends ClusterSpecTestBasic {
  override val printLogDetails = false
  private def readOutInAct(outIO: StreamBitsIO, debugIO: SRAMCommonDebugIO with InActSpecialDebugIO, doneIO: Bool,
                           theData: List[Int], idx: Int, theClock: Clock, prefix: String
                          ): Unit = {
    def getDebugInfo(stage: Int) : String = {
      s"[$prefix] $idx.$stage done = ${doneIO.peek()}\n" +
        s"[$prefix] $idx.$stage valid= ${outIO.data.valid.peek()}\n" +
        s"[$prefix] $idx.$stage crDt = ${debugIO.currentData.peek()}\n" +
        s"[$prefix] $idx.$stage inInc= ${debugIO.indexAcc.peek()}\n" +
        s"[$prefix] $idx.$stage waFR = ${debugIO.waitForRead.peek()}\n" +
        s"[$prefix] $idx.$stage doRd = ${debugIO.doReadWire.peek()}\n" +
        s"[$prefix] $idx.$stage data = ${outIO.data.bits.peek()}\n" +
        s"[$prefix] $idx.$stage idx  = ${debugIO.idx.peek()}"
    }
    println(s"------------- $idx-th $prefix read cycle ------------")
    outIO.data.valid.expect(false.B, s"$prefix should not valid now")
    if (printLogDetails) println(getDebugInfo(0))
    debugIO.waitForRead.expect(false.B, s"[$prefix] it should be false as this is the first read cycle")
    theClock.step()
    outIO.data.bits.expect(theData(idx).U,
      s"[$prefix] theData($idx) = ${theData(idx)}, current idx = " +
        s"${debugIO.idx.peek()}, data length = ${theData.length}\n" +
        s"theData is\n $theData \n" + getDebugInfo(5))
    outIO.data.valid.expect(true.B, s"$prefix should valid now")
    if (printLogDetails) println(getDebugInfo(5))
    outIO.data.valid.expect(true.B, s"$prefix should valid now")
    debugIO.waitForRead.expect(true.B, s"$prefix should be true as this is the second read cycle")
    debugIO.doReadWire.expect(true.B, s"$prefix should be true")
    if (theData(idx) != 0) {
      doneIO.expect(false.B,
        s"$prefix current data equals to ${theData(idx)}, does not equal to zero, should be unfinished now")
    }
    if (printLogDetails) {
      println(s"[$prefix] ${idx + 1} done = ${doneIO.peek()}")
    }
    println(s"[$prefix] ${idx + 1} PASS $idx")
  }
  private def readOutPSumData(OutIO: DecoupledIO[UInt], debugIO: SRAMCommonDebugIO, doneIO: Bool, adrIO: UInt,
                                theData: List[Int], startIndex: Int, theClock: Clock, prefix: String): Unit = {
    for (i <- 0 until pSumOneSPadNum) {
      println(s"--------------- $i-th read cycle -----------")
      if (printLogDetails) println(
        s"[$prefix] $i data = ${theData(i)} \n" +
          s"[$prefix] $i waFR = ${debugIO.waitForRead.peek()}\n" +
          s"[$prefix] $i doRd = ${debugIO.doReadWire.peek()}"
      )
      debugIO.waitForRead.expect(false.B, s"$prefix@$i should be false as this is the first read cycle")
      debugIO.doReadWire.expect(true.B, s"$prefix@$i should be true")
      adrIO.poke((i + startIndex).U)
      OutIO.ready.poke(true.B)
      OutIO.valid.expect(false.B, s"$prefix@$i should not valid now")
      theClock.step()
      if (printLogDetails) println(
        s"[$prefix] $i data = ${theData(i)} \n" +
          s"[$prefix] $i waFR = ${debugIO.waitForRead.peek()}\n" +
          s"[$prefix] $i doRd = ${debugIO.doReadWire.peek()}"
      )
      debugIO.waitForRead.expect(true.B, s"$prefix@$i should be true as this is the second read cycle")
      debugIO.doReadWire.expect(true.B, s"$prefix@$i should be true")
      OutIO.bits.expect(theData(i).U, s"$prefix@$i, theData should be ${theData(i)}")
      OutIO.valid.expect(true.B, s"$prefix@$i should valid now")
      theClock.step()
      println(s"[$prefix] PASS $i")
    }
  }
  private def inActStateBeZero(theTopIO: GLBClusterIO, prefix: String): Unit = {
    theTopIO.debugIO.inActDebugIO.zipWithIndex.foreach({ case ( x, idx) =>
      if (printLogDetails) {
        println(s"[$prefix] inActBankState$idx =  ${x.theState.peek()}")
        println(s"[$prefix] adrZeroState$idx   =  ${x.adrDebug.commonDebug.zeroState.peek()}")
        println(s"[$prefix] dataZeroState$idx  =  ${x.dataDebug.commonDebug.zeroState.peek()}")
      }
      x.theState.expect(0.U, "every inActSRAMBank still needs to be idle")
    })
  }
  private def topReadPSum(theTopIO: GLBClusterIO, dataStream: Seq[List[Int]], startIndexes: Seq[Int],
                            theClock: Clock): Unit = {
    val theCtrlIO = theTopIO.ctrlPath.pSumIO.map(x => x.readIO)
    val thePSumDebugIOs = theTopIO.debugIO.pSumDebugIO
    val theOutIOs = theTopIO.dataPath.pSumIO.map(x => x.outIOs)
    def forkReadOutPSumHelper(index: Int): Unit = {
      theCtrlIO(index).enable.poke(true.B)
      println(s"-------- PSumSRAM$index Read Enabled ----------")
      theClock.step() // the topPSum from idle to oneSRAMDoing
      readOutPSumData(OutIO = theOutIOs(index), debugIO = thePSumDebugIOs(index),
        doneIO = theTopIO.debugIO.onePSumSRAMDone(index), theData = dataStream(index),
        adrIO = theCtrlIO(index).adr, startIndex = startIndexes(index), theClock = theClock, prefix = s"PSumSRAM$index")
      println(s"[PSumBank] $index PSum finish")
    }
    // read begin
    (1 until pSumSRAMNum).foldLeft(fork(forkReadOutPSumHelper(0))) {
      case (left, right) => left.fork(forkReadOutPSumHelper(right))
    } .join()
  }
  private def forkReadAdrAndData(theOutIO: CSCStreamIO, theDebugIO: InActSRAMBankDebugIO, theEnIO: Bool,
                                   adrStream: List[Int], dataStream: List[Int], adrStrIdx: Int, dataStrIdx: Int,
                                   theClock: Clock, prefix: String): Unit = {
    var adrRdIdx = adrStrIdx
    var dataRdIdx = dataStrIdx
    theEnIO.poke(false.B)
    fork {
      while (!theDebugIO.dataDebug.subDone.peek().litToBoolean) {
        theClock.step()
        theOutIO.dataIOs.data.ready.poke(true.B)
        readOutInAct(theOutIO.dataIOs, theDebugIO.dataDebug.commonDebug,
          theDebugIO.dataDebug.subDone, dataStream, dataRdIdx,
          theClock, prefix = s"$prefix@data"
        )
        dataRdIdx = dataRdIdx + 1
      }
      println("------------ data read one stream -------------")
      theDebugIO.dataDebug.subDone.expect(true.B,
        s"current data equals to ${if (dataRdIdx > 0) dataStream(dataRdIdx - 1)}, data should finish now")
      theOutIO.dataIOs.data.ready.poke(false.B)
    }.fork {
      while (!theDebugIO.adrDebug.subDone.peek().litToBoolean) {
        theClock.step()
        theOutIO.adrIOs.data.ready.poke(true.B)
        readOutInAct(theOutIO.adrIOs, theDebugIO.adrDebug.commonDebug,
          theDebugIO.adrDebug.subDone, adrStream, adrRdIdx,
          theClock, prefix = s"$prefix@adr"
        )
        adrRdIdx = adrRdIdx + 1
      }
      println("------------ adr read one stream --------------")
      theOutIO.adrIOs.data.ready.poke(false.B)
    }.join()
    println("---------- both read out one stream ------------")
    theClock.step()
    theDebugIO.theState.expect(0.U, s"$prefix inActSRAM state should be idle after all read")
    if (printLogDetails) println(s"[$prefix] InActSRAMState = " + theDebugIO.theState.peek())
  }
  private def topReadOutActAdrAndData(theTopIO: GLBClusterIO, adrStreams: Seq[List[Int]], dataStreams: Seq[List[Int]],
                                        theClock: Clock): Unit = {
    val theCtrlIO = theTopIO.ctrlPath.inActIO.map(x => x.readIO)
    val theInActDebugIOs = theTopIO.debugIO.inActDebugIO
    val theOutIOs = theTopIO.dataPath.inActIO.map(x => x.outIOs)
    val adrLookup = adrStreams.map(x => getStreamLookUp(x))
    val dataLookup = dataStreams.map(x => getStreamLookUp(x))
    val adrIdx = Array.fill(inActSRAMNum) {0}
    val dataIdx = Array.fill(inActSRAMNum) {0}
    def forkHelper(sramIdx: Int, inActStreamIdx: Int): Unit = {
      theClock.step(cycles = (new Random).nextInt(50) + (new Random).nextInt(50))
      println(s"------------- SRAM$sramIdx Begins -------------")
      adrIdx(sramIdx) = adrLookup(sramIdx)(inActStreamIdx)
      dataIdx(sramIdx) = dataLookup(sramIdx)(inActStreamIdx)
      println(s"currently reading $inActStreamIdx index InAct, adr from ${adrIdx(sramIdx)}, data from ${dataIdx(sramIdx)}")
      theCtrlIO(sramIdx).enable.poke(true.B)
      theCtrlIO(sramIdx).adr.poke(inActStreamIdx.U)
      theClock.step() // subInAct from idle to doing
      if (printLogDetails) println(s"-------- SRAM$sramIdx state = ${theInActDebugIOs(sramIdx).theState.peek()}")
      theInActDebugIOs(sramIdx).theState.expect(1.U, s"the inAct$sramIdx State should be doing one cycle later")
      forkReadAdrAndData(
        theOutIO = theOutIOs(sramIdx), theDebugIO = theInActDebugIOs(sramIdx), theCtrlIO(sramIdx).enable,
        adrStreams(sramIdx), dataStreams(sramIdx), adrIdx(sramIdx), dataIdx(sramIdx), theClock,
        prefix = s"SRAM$sramIdx@Stream$inActStreamIdx"
      )
      println(s"------------- SRAM$sramIdx finish -------------")
      theTopIO.debugIO.oneInActSRAMDone.zipWithIndex.foreach({case (x, idx) =>
        if (printLogDetails) println(s"[SRAM$idx@Stream$inActStreamIdx] Done = ${x.peek()}")
      })
    }
    for (g2 <- 0 until G2) {
      for (n2 <- 0 until N2) {
        for (m2 <- 0 until M2) {
          for (f2 <- 0 until F2) {
            for (c2 <- 0 until C2) {
              for (s2 <- 0 until S2) {
                val inActIdx: Int = g2*N2*C2*(F2 + S2) + n2*C2*(F2 + S2) + c2*(F2 + S2) + (f2 + s2)
                println(s"currently reading $inActIdx index InAct, $g2, $n2, $m2, $f2, $c2, $s2")
                println("------------- begin SRAMs read -------------")
                (1 until inActSRAMNum).foldLeft(fork(forkHelper(sramIdx = 0, inActStreamIdx = inActIdx))) {
                  case (left, right) => left.fork(forkHelper(sramIdx = right, inActStreamIdx = inActIdx))
                } .join()
                println("------------- three SRAMs finish -------------")
                if (printLogDetails) {
                  theTopIO.debugIO.oneInActSRAMDone.zipWithIndex.foreach({case (x, idx) =>
                    println(s"[SRAM$idx@Stream$inActIdx] Done = ${x.peek()}")
                  })
                }
                theClock.step()
                println("------------- one cycle later -------------")
                if (printLogDetails) {
                  theTopIO.debugIO.oneInActSRAMDone.zipWithIndex.foreach({case (x, idx) =>
                    println(s"[SRAM$idx@Stream$inActIdx] Done = ${x.peek()}")
                  })
                }
                theClock.step()
                println("------------- one cycle later -------------")
                theTopIO.debugIO.oneInActSRAMDone.foreach(_.expect(false.B,"inActReg should be idle after doEn disabled"))
                if (printLogDetails) {
                  theTopIO.debugIO.oneInActSRAMDone.zipWithIndex.foreach({case (x, idx) =>
                    println(s"[SRAM$idx@Stream$inActIdx] Done = ${x.peek()}")
                  })
                }
                theClock.step(cycles = (new Random).nextInt(50) + 50)
                theTopIO.debugIO.oneInActSRAMDone.foreach(_.expect(false.B,"after several cycles, " +
                  "every Vec Reg should be false as initial state"))
                inActStateBeZero(theTopIO, prefix = s"SRAMTop@Stream$inActIdx")
              }
            }
          }
        }
      }
    }
    println("------------- all streams read finish -------------")
  }
  private def readOutActAdrAndData(theOutIO: CSCStreamIO, theCtrlIO:MeMReWrIO, theDebugIO: InActSRAMBankDebugIO,
                                     adrStream: List[Int], dataStream: List[Int], theClock: Clock): Unit = {
    var (adrRdIdx, dataRdIdx): (Int, Int) = (0, 0)
    val adrLookup = getStreamLookUp(adrStream)
    val dataLookup = getStreamLookUp(dataStream)
    for (g2 <- 0 until G2) {
      for (n2 <- 0 until N2) {
        for (m2 <- 0 until M2) {
          for (f2 <- 0 until F2) {
            for (c2 <- 0 until C2) {
              for (s2 <- 0 until S2) {
                val inActIdx: Int = g2*N2*C2*(F2 + S2) + n2*C2*(F2 + S2) + c2*(F2 + S2) + (f2 + s2)
                println(s"currently reading $inActIdx index InAct, $g2, $n2, $m2, $f2, $c2, $s2")
                adrRdIdx = adrLookup(inActIdx)
                dataRdIdx = dataLookup(inActIdx)
                println(s"------- inActStream $inActIdx begins -------")
                theCtrlIO.enable.poke(true.B)
                theCtrlIO.adr.poke(inActIdx.U)
                theClock.step() // from idle to doing
                theDebugIO.theState.expect(1.U, "theSRAM state should be doing now")
                forkReadAdrAndData(
                  theOutIO, theDebugIO, theCtrlIO.enable,
                  adrStream, dataStream, adrRdIdx, dataRdIdx, theClock, prefix = "inActSRAMBank"
                )
                theClock.step(cycles = (new Random).nextInt(50) + 50)
              }
            }
          }
        }
      }
    }
  }
  private def writeInPSumData(inIO: DecoupledIO[UInt], debugIO: SRAMCommonDebugIO, doneIO: Bool, theData: List[Int],
                                startIndex: Int, adrIO: UInt , theClock: Clock, prefix: String): Unit = {
    for (i <- 0 until pSumOneSPadNum) {
      println(s"--------------- $i-th PSum write cycle -----------")
      adrIO.poke((startIndex + i).U)
      inIO.bits.poke(theData(i).U)
      inIO.valid.poke(true.B)
      if (printLogDetails) {
        println(s"[$prefix] data = ${theData(i)} \n" +
          s"[$prefix] index = ${startIndex + i}")
      }
      inIO.ready.expect(true.B, s"[$prefix] $i should be ready now")
      theClock.step()
      println(s"[$prefix] $i PASS")
    }
  }
  private def writeInAct(inIO: StreamBitsIO, debugIO: SRAMCommonDebugIO with InActSpecialDebugIO,
                           doneIO: Bool, theData: Seq[List[Int]], theClock: Clock, prefix: String) : Unit = {
    require(theData.length == 2, s"it should have two group of data, but ${theData.length}")
    def pokeData(currentPokeData: List[Int], pokeIdx: Int): Unit = {
      inIO.data.bits.poke(currentPokeData(pokeIdx).U)
      inIO.data.valid.poke(true.B)
      inIO.data.ready.expect(true.B, s"[$prefix@$pokeIdx@former] should be ready now")
      if (printLogDetails) {
        println(
          s"[$prefix@$pokeIdx] done     = ${doneIO.peek()}\n" +
            s"[$prefix@$pokeIdx] data     = ${currentPokeData(pokeIdx)}\n" +
            s"[$prefix@$pokeIdx] index    = ${debugIO.idx.peek()}\n" +
            s"[$prefix@$pokeIdx] doWrite  = ${debugIO.doWriteWire.peek()}\n" +
            s"[$prefix@$pokeIdx] lookupIdx= ${debugIO.lookupIdx.peek()}\n" +
            s"[$prefix@$pokeIdx] nextValid= ${debugIO.doReadWire.peek()}"
        )
      }
    }
    for (i <- theData.head.indices) {
      println(s"------------- $i-th $prefix write former cycle ----------")
      pokeData(currentPokeData = theData.head, pokeIdx = i)
      theClock.step()
      if (printLogDetails) println(s"[$prefix@$i]  done     = ${doneIO.peek()}")
      doneIO.expect(false.B, "it shouldn't done in former group")
      println(s"[$prefix@$i] PASS $i")
    }
    for (i <- theData.last.indices) {
      println(s"------------- $i-th $prefix write later cycle ----------")
      pokeData(currentPokeData = theData.last, pokeIdx = i)
      theClock.step()
      if (printLogDetails) println(s"[$prefix@$i]  done     = ${doneIO.peek()}")
      doneIO.expect((i == theData.last.length - 1).B,
        s"[$prefix@$i@later] Data($i) = ${theData.last(i)}, $prefix should finish?")
      println(s"[$prefix@$i] PASS $i")
    }
  }
  private def writeInActAdrAndData(theInIO: CSCStreamIO, theDebugIO: InActSRAMBankDebugIO, adrStream: Seq[List[Int]],
                                     dataStream: Seq[List[Int]], theClock: Clock): Unit = {
    fork {
      writeInAct(theInIO.dataIOs, theDebugIO.dataDebug.commonDebug,
        theDebugIO.dataDebug.subDone, dataStream, theClock, prefix = "inActSRAM@data"
      )
      println("-------------- data write finish ---------------")
    } .fork {
      writeInAct(theInIO.adrIOs, theDebugIO.adrDebug.commonDebug,
        theDebugIO.adrDebug.subDone, adrStream, theClock,prefix = "inActSRAM@adr"
      )
      println("-------------- adr write finish ----------------")
    } .join()
  }

  behavior of "test the spec of GLB Cluster"
  it should "work well on PSumSRAMBank" in {
    test(new PSumSRAMBank(pSumSRAMSize, psDataWidth, true)) { thePSumBank =>
      val theTopIO = thePSumBank.io
      val theClock = thePSumBank.clock
      val theData = pSumStream.head
      val startIndex = (new Random).nextInt(maxPSumStreamNum - 1) * pSumOneSPadNum
      println("---------------- test begin ----------------")
      println("---------- Partial Sum SRAM Bank -----------")
      println("---------- test basic functions ------------")
      println(s"[PSumSRAMBank] startIndex = $startIndex")
      thePSumBank.reset.poke(true.B)
      theClock.step()
      thePSumBank.reset.poke(false.B)
      println("-------------- begin to write --------------")
      theTopIO.ctrlPath.writeIO.enable.poke(true.B)
      writeInPSumData(inIO = theTopIO.dataPath.inIOs, debugIO = theTopIO.debugIO, doneIO = theTopIO.ctrlPath.writeIO.done,
        theData = theData, startIndex = startIndex, adrIO = theTopIO.ctrlPath.writeIO.adr, theClock = theClock,
        prefix = "PSumSRAMBank")
      println("---------------- write finish --------------")
      theTopIO.ctrlPath.writeIO.enable.poke(false.B)
      theClock.step(cycles = (new Random).nextInt(5) + 1)
      println("---------------- begin to read -------------")
      theTopIO.ctrlPath.readIO.enable.poke(true.B)
      readOutPSumData(OutIO = theTopIO.dataPath.outIOs, debugIO = theTopIO.debugIO, doneIO = theTopIO.ctrlPath.readIO.done,
        adrIO = theTopIO.ctrlPath.readIO.adr , theData = theData, startIndex = startIndex, theClock = theClock,
        prefix = "PSumSRAMBank"
      )
      println("---------------- read finish ---------------")
      println("---------------- test finish ---------------")
    }
  }

  it should "work well on InActSRAMCommon" in {
    test(new InActSRAMCommon(inActAdrSRAMSize, inActAdrWidth, true)).withAnnotations(Seq(WriteVcdAnnotation)) { theAdrSRAM =>
      val theTopIO = theAdrSRAM.io
      val theClock = theAdrSRAM.clock
      val InActAdrStream = inActAdrStream.take(2)
      val inActAdrLookupTmp = InActAdrStream.map(x => getStreamLookUp(x))
      /** we need to modify the later look up table
        * each element should add the end element of the former's last look up table value
        * and drop the head, as it's zero */
      val inActAdrLookupLaterTmp = inActAdrLookupTmp.last.tail.map(x => x + inActAdrLookupTmp.head.last)
      val inActAdrLookUp = inActAdrLookupTmp.head ::: inActAdrLookupLaterTmp
      println(s"have ${inActAdrLookUp.length} inActStreams")
      println(s"inActAdrLookUp = $inActAdrLookUp")
      println(s"theDataStream = $InActAdrStream")
      println(s"streamNum = ${inActStreamNum*2}")
      println(s"zeroNum = ${InActAdrStream.flatten.collect({case x if x == 0 => x}).length}")
      require(InActAdrStream.flatten.length <= inActAdrSRAMSize,
        s"the size of current inAct should less than inActAdrSRAMSize, " +
        s"but ${InActAdrStream.flatten.length} > $inActAdrSRAMSize")
      println("----------------- test begin -----------------")
      println("----------- InputActAdr SRAM Bank ------------")
      println("----------- test basic functions -------------")
      theAdrSRAM.reset.poke(true.B)
      theClock.step()
      theAdrSRAM.reset.poke(false.B)
      println("--------------- begin to write ---------------")
      theTopIO.ctrlPath.readIO.enable.poke(false.B)
      theTopIO.ctrlPath.writeIO.enable.poke(true.B)
      // begin to write in data
      writeInAct(theTopIO.dataPath.inIOs, theTopIO.debugIO, theTopIO.ctrlPath.writeIO.done,
        InActAdrStream, theClock, prefix = "inActCommonSRAM")
      theTopIO.ctrlPath.readIO.enable.poke(false.B)
      println("--------------- write finish -----------------")
      theTopIO.ctrlPath.writeIO.enable.poke(false.B)
      println(s"[inActCommonSRAMAdr] inInc = ${theTopIO.debugIO.indexAcc.peek()}")
      theClock.step(cycles = (new Random).nextInt(5) + 5)
      println("--------------- begin to read ----------------")
      println(s"[inActCommonSRAMAdr] inInc = ${theTopIO.debugIO.indexAcc.peek()}")
      // begin to read out data
      for (g2 <- 0 until G2) {
        for (n2 <- 0 until N2) {
          for (m2 <- 0 until M2) {
            for (f2 <- 0 until F2) {
              for (c2 <- 0 until C2) {
                for (s2 <- 0 until S2) {
                  val inActIdx: Int = g2*N2*C2*(F2 + S2) + n2*C2*(F2 + S2) + c2*(F2 + S2) + (f2 + s2)
                  println(s"currently reading $inActIdx index InAct, $g2, $n2, $m2, $f2, $c2, $s2")
                  var theRdIdx = inActAdrLookUp(inActIdx)
                  while (!theTopIO.ctrlPath.readIO.done.peek().litToBoolean) {
                    theClock.step()
                    theTopIO.ctrlPath.readIO.enable.poke(true.B)
                    theTopIO.ctrlPath.readIO.adr.poke(inActIdx.U)
                    theTopIO.dataPath.outIOs.data.ready.poke(true.B)
                    readOutInAct(theTopIO.dataPath.outIOs, theTopIO.debugIO, theTopIO.ctrlPath.readIO.done,
                      InActAdrStream.flatten.toList, theRdIdx, theClock, prefix = "inActCommonSRAMAdr")
                    theRdIdx = theRdIdx + 1
                  }
                  theTopIO.ctrlPath.readIO.enable.poke(false.B)
                  theTopIO.dataPath.outIOs.data.ready.poke(false.B)
                  println("------------ read out one stream -------------")
                  println(s"[inActCommonSRAMAdr] theZeroState = ${theTopIO.debugIO.zeroState.peek()}")
                  theClock.step(cycles = (new Random).nextInt(50) + 50)
                }
              }
            }
          }
        }
      }
      // read out finish
      println("---------------- read finish -----------------")
      println(s"[inActCommonSRAMAdr] streamNum = $inActStreamNum")
      println("---------------- test finish -----------------")
    }
  }

  it should "work well on InActSRAMBank" in {
    test(new InActSRAMBank(true)) { theInAct =>
      val theTopIO = theInAct.io
      val theClock = theInAct.clock
      val InActAdrStream = inActAdrStream.take(2)
      val InActDataStream = inActDataStream.take(2)
      println(s"----- inActReadCycle = ${InActAdrStream.flatten.length}")
      println("----------------- test begin -----------------")
      println(s"--------  theInActStreamNum = $inActStreamNum")
      println("----------- InputActAdr SRAM Bank ------------")
      println("----------- test basic functions -------------")
      theInAct.reset.poke(true.B)
      theClock.step()
      theInAct.reset.poke(false.B)
      println("--------------- begin to write ---------------")
      theTopIO.ctrlPath.writeIO.enable.poke(true.B)
      theClock.step() // from idle to doing
      // begin to write streams into data sram and address sram
      writeInActAdrAndData(theTopIO.dataPath.inIOs, theTopIO.debugIO, InActAdrStream, InActDataStream, theClock)
      // write finish
      theClock.step()
      theTopIO.debugIO.theState.expect(0.U, "after all write, the state should be idle now")
      println(s"[inActSRAMBank] theState = ${theTopIO.debugIO.theState.peek()}")
      println("------------- all write finish ---------------")
      theTopIO.ctrlPath.writeIO.enable.poke(false.B)
      theClock.step(cycles = (new Random).nextInt(5) + 1)
      println("--------------- begin to read ----------------")
      // begin to read out streams into data sram and address sram
      readOutActAdrAndData(theTopIO.dataPath.outIOs, theTopIO.ctrlPath.readIO, theTopIO.debugIO,
        InActAdrStream.head, InActDataStream.head, theClock)
      theClock.step()
      println(s"[inActSRAMBank] theState = ${theTopIO.debugIO.theState.peek()}")
      println("-------------- all read finish ---------------")
      println(s"[inActSRAMBank] streamNum = $inActStreamNum")
      println("---------------- test finish -----------------")
    }
  }

  it should "work well on GLB Cluster" in {
    test (new GLBCluster(true)) { theGLB =>
      val theTopIO = theGLB.io
      val theClock = theGLB.clock
      val theInActCtrl = theTopIO.ctrlPath.inActIO
      val thePSumCtrl = theTopIO.ctrlPath.pSumIO
      val gnmfcs1Stream = Seq.fill(6){(new Random).nextInt(6) + 1}
      val pSumStartIdx = gnmfcs1Stream.head*N2*M2*F2 + gnmfcs1Stream(1)*M2*F2 + gnmfcs1Stream(2)*F2 + gnmfcs1Stream(3) // FIXME
      require(pSumStartIdx + pSumOneSPadNum < pSumSRAMSize, "pSum's start index plus oneSPad size should less than pSumSRAMSize")
      def forkWriteInPSumHelper(index: Int, startIndex: Int): Unit = {
        thePSumCtrl(index).writeIO.enable.poke(true.B)
        writeInPSumData(inIO = theTopIO.dataPath.pSumIO(index).inIOs, debugIO = theTopIO.debugIO.pSumDebugIO(index),
          doneIO = theTopIO.debugIO.onePSumSRAMDone(index), theData = thePSumDataStreams(index),
          startIndex = startIndex, adrIO = thePSumCtrl(index).writeIO.adr, theClock = theClock, prefix = s"PSumSRAMBank$index")
        println(s"[PSumSRAMBank$index] PSum finish")
        thePSumCtrl(index).writeIO.enable.poke(false.B) // false the en after receiving done signal
      }
      def forkWriteInInActHelper(index: Int): Unit = {
        val pokeFormerLaterInActAdr = Seq(theInActAdrStreams(index), theInActAdrStreams(index+inActSRAMNum))
        val pokeFormerLaterInActData = Seq(theInActDataStreams(index), theInActDataStreams(index+inActSRAMNum))
        theInActCtrl(index).writeIO.enable.poke(true.B)
        theClock.step() // sub from idle to doing;
        theTopIO.debugIO.inActDebugIO(index).theState.expect(1.U, s"the inActSRAM $index should doing now")
        println(s"[inActSRAMBank$index] inActSRAMState$index =  ${theTopIO.debugIO.inActDebugIO(index).theState.peek()}")
        writeInActAdrAndData(theTopIO.dataPath.inActIO(index).inIOs, theTopIO.debugIO.inActDebugIO(index),
          pokeFormerLaterInActAdr, pokeFormerLaterInActData, theClock)
        println(s"-------- $index InAct finish")
        theInActCtrl(index).writeIO.done.expect(true.B, "after all inActSRAMs done, top inAct should done now")
        theClock.step()
        println(s"[inActSRAMBank] $index InActSRAM State = ${theTopIO.debugIO.inActDebugIO(index).theState.peek()}")
        theTopIO.debugIO.inActDebugIO(index).theState.expect(0.U, s"as inActSRAM$index finishes writing, " +
          s"the state should be idle now after one cycle")
        theInActCtrl(index).writeIO.enable.poke(false.B)
      }
      println("----------------- test begin -----------------")
      println("----------- InputActAdr SRAM Bank ------------")
      println("----------- test basic functions -------------")
      theGLB.reset.poke(true.B)
      theClock.step(cycles = (new Random).nextInt(5) + 1)
      theGLB.reset.poke(false.B)
      println("--------------- begin to write ---------------")
      fork {
        theClock.step(cycles = (new Random).nextInt(5) + 1)
        //gnmfcs1IOs.zip(gnmfcs1Stream).foreach({ case (io, cfg) => io.poke(cfg.U)})
        println("------------- Enable PSum Now ---------------")
        theClock.step(2) // top from idle to doing, sub from idle to doing;
        (1 until pSumSRAMNum).foldLeft(fork(forkWriteInPSumHelper(0, startIndex = pSumStartIdx))) {
          case (left, right) => left.fork(forkWriteInPSumHelper(right, startIndex = pSumStartIdx)) // FIXME: check the start index
        } .join()
        println("---------- all pSumSRAMs are written ------------")
        println("----------- pSum write verification -------------")
        theClock.step()
        println("--------------- one cycle later ---------------")
        theClock.step()
        println("--------------- one cycle later ---------------")
        theClock.step(cycles = (new Random).nextInt(5) + 1)
        println("------------ several cycles later -------------")
      } .fork {
        theClock.step(cycles = (new Random).nextInt(5) + 1)
        println("-------------- Enable InAct Now ---------------")
        (1 until inActSRAMNum).foldLeft(fork(forkWriteInInActHelper(0))) {
          case (left, right) => left.fork(forkWriteInInActHelper(index = right))
        } .join()
        println("--------- all inActSRAMs are written ----------")
        inActStateBeZero(theTopIO, prefix = "inActSRAMTop")
        theClock.step()
        println("----------------- one cycle later ---------------")
        inActStateBeZero(theTopIO, prefix = "inActSRAMTop")
        theClock.step(cycles = (new Random).nextInt(5) + 1)
        println("----------- several cycles later ----------")
        inActStateBeZero(theTopIO, prefix = "inActSRAMTop")
      } .join()
      // write test finish
      println("--------------- begin to read ----------------")
      fork {
        theClock.step(cycles = (new Random).nextInt(30) + 1)
        theClock.step()
        theTopIO.ctrlPath.pSumIO.head.readIO.adr.poke(pSumStartIdx.U)
        topReadPSum(theTopIO = theTopIO, dataStream = thePSumDataStreams,
          startIndexes = Seq.fill(pSumSRAMNum){pSumStartIdx}, theClock = theClock)
        theClock.step()
        println("------------ all pSum read finish --------------")
      } .fork {
        theClock.step(cycles = (new Random).nextInt(5) + 1)
        // begin to read out streams into data sram and address sram
        topReadOutActAdrAndData(theTopIO, theInActAdrStreams, theInActDataStreams, theClock)
        theClock.step()
        if (printLogDetails) theTopIO.debugIO.oneInActSRAMDone.zipWithIndex.foreach({case (x, idx) =>
          println(s"[SRAM$idx] Done = ${x.peek()}")
        })
        println("------------ all inAct read finish -------------")
        println(s"[inActSRAMBank] InActStreamNum = $inActStreamNum")
      } .join()
      println("---------------- test finish -----------------")
    }
  }
}

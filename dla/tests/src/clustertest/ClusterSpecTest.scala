package dla.tests.clustertest

import Chisel.DecoupledIO
import chisel3._
import chisel3.experimental.{DataMirror, Direction}
import chisel3.tester._
import dla.cluster._
import dla.eyerissTop.EyerissTopConfig
import dla.pe.{CSCStreamIO, MCRENFConfig, SPadSizeConfig, StreamBitsIO}
import dla.tests.GenOneStreamData
import org.scalatest._
import scala.util.Random
import scala.math.max

class ClusterSpecTest extends FlatSpec with ChiselScalatestTester with Matchers
  with ClusterSRAMConfig with MCRENFConfig with SPadSizeConfig with GNMFCS2Config
  with EyerissTopConfig with GNMFCS1Config {
  private val printLogDetails = false // true to print more detailed logs
  //private val printLogDetails = true // true to print more detailed logs
  private val maxPSumStreamNum: Int = pSumSRAMSize/pSumOneSPadNum
  private val addendRand = Seq.fill(peColNum, pSumOneSPadNum){(new Random).nextInt(10)}
  private val peNum = peRowNum * peColNum * cgRowNum * cgColNum
  private val oneStreamData = new GenOneStreamData
  private val inActAdrStream = oneStreamData.inActAdrStream
  private val inActDataStream = oneStreamData.inActDataStream
  private val weightAdrStream = oneStreamData.weightAdrStream
  private val weightDataStream = oneStreamData.weightDataStream
  private val pSumStream = oneStreamData.outPSumStream
  private val theInActAdrStreams = inActAdrStream.take(inActRouterNum) // in actual, it needs s2 + f2
  private val theInActDataStreams = inActDataStream.take(inActRouterNum)
  private val theWeightAdrStreams = weightAdrStream.take(weightRouterNum)
  private val theWeightDataStreams = weightDataStream.take(weightRouterNum)
  private val thePSumDataStreams = pSumStream.take(pSumRouterNum)
  private def getStreamLookUp(streamData: List[Int]): List[Int] = {
    var lookList: List[Int] = Nil
    lookList = lookList:::List(0)
    for (i <- 0 until streamData.length - 1) {
      if (streamData(i) == 0) {
        lookList = lookList:::List(i + 1)
      }
    }
    lookList.init
  }
  private def readOutAct(outIO: StreamBitsIO, debugIO: SRAMCommonDebugIO with InActSpecialDebugIO, doneIO: Bool,
                         theData: List[Int], idx: Int, theClock: Clock, prefix: String
                        ): Unit = {
    println(s"------------- $idx-th $prefix read cycle ------------")
    outIO.data.valid.expect(false.B, s"$prefix should not valid now")
    if (printLogDetails) println(
      s"[$prefix] $idx.0 done = ${doneIO.peek()}\n" +
        s"[$prefix] $idx.0 valid= ${outIO.data.valid.peek()}\n" +
        s"[$prefix] $idx.0 crDt = ${debugIO.currentData.peek()}\n" +
        s"[$prefix] $idx.0 inInc= ${debugIO.indexInc.peek()}\n" +
        s"[$prefix] $idx.0 waFR = ${debugIO.waitForRead.peek()}\n" +
        s"[$prefix] $idx.0 doRd = ${debugIO.doReadWire.peek()}\n" +
        s"[$prefix] $idx.0 data = ${outIO.data.bits.peek()}\n" +
        s"[$prefix] $idx.0 idx  = ${debugIO.idx.peek()}"
    )
    debugIO.waitForRead.expect(false.B, s"[$prefix] it should be false as this is the first read cycle")
    theClock.step()
    outIO.data.bits.expect(theData(idx).U, s"[$prefix] theData($idx) = ${theData(idx)}, current idx = " +
      s"${debugIO.idx.peek()}")
    outIO.data.valid.expect(true.B, s"$prefix should valid now")
    if (printLogDetails) println(
      s"[$prefix] $idx.5 done = ${doneIO.peek()}\n" +
        s"[$prefix] $idx.5 valid= ${outIO.data.valid.peek()}\n" +
        s"[$prefix] $idx.5 crDt = ${debugIO.currentData.peek()}\n" +
        s"[$prefix] $idx.5 inInc= ${debugIO.indexInc.peek()}\n" +
        s"[$prefix] $idx.5 waFR = ${debugIO.waitForRead.peek()}\n" +
        s"[$prefix] $idx.5 doRd = ${debugIO.doReadWire.peek()}\n" +
        s"[$prefix] $idx.5 data = ${outIO.data.bits.peek()}\n" +
        s"[$prefix] $idx.5 idx  = ${debugIO.idx.peek()}"
    )
    outIO.data.valid.expect(true.B, s"$prefix should valid now")
    debugIO.waitForRead.expect(true.B, s"$prefix should be true as this is the second read cycle")
    debugIO.doReadWire.expect(true.B, s"$prefix should be true")
    if (theData(idx) != 0) {
      doneIO.expect(false.B, s"$prefix current data equals to ${theData(idx)}, does not equal to zero, " +
        s"should be unfinished now")
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
  private def topReadPSum(theTopIO: GLBClusterIO, dataStream: Seq[List[Int]], startIndexes: Seq[Int], theClock: Clock): Unit = {
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
        readOutAct(theOutIO.dataIOs, theDebugIO.dataDebug.commonDebug,
          theDebugIO.dataDebug.subDone, dataStream, dataRdIdx,
          theClock, prefix = s"$prefix@data"
        )
        dataRdIdx = dataRdIdx + 1
      }
      println("------------ data read one stream -------------")
      theDebugIO.dataDebug.subDone.expect(true.B, s"current data equals to " +
        s"${if (dataRdIdx > 0) dataStream(dataRdIdx - 1)}, data should finish now")
      theOutIO.dataIOs.data.ready.poke(false.B)
    }.fork {
      while (!theDebugIO.adrDebug.subDone.peek().litToBoolean) {
        theClock.step()
        theOutIO.adrIOs.data.ready.poke(true.B)
        readOutAct(theOutIO.adrIOs, theDebugIO.adrDebug.commonDebug,
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
  private def writeInAct(inIO: StreamBitsIO, debugIO: SRAMCommonDebugIO with InActSpecialDebugIO, doneIO: Bool, theData: List[Int],
                         theClock: Clock, prefix: String) : Unit = {
    for (i <- theData.indices) {
      println(s"------------- $i-th $prefix write cycle ----------")
      inIO.data.bits.poke(theData(i).U)
      inIO.data.valid.poke(true.B)
      inIO.data.ready.expect(true.B, s"$prefix@$i should be ready now")
      if (printLogDetails) {
        println(
          s"[$prefix@$i] done     = ${doneIO.peek()}\n" +
            s"[$prefix@$i] data     = ${theData(i)}\n" +
            s"[$prefix@$i] index    = ${debugIO.idx.peek()}\n" +
            s"[$prefix@$i] doWrite  = ${debugIO.doWriteWire.peek()}\n" +
            s"[$prefix@$i] lookupIdx= ${debugIO.lookupIdx.peek()}\n" +
            s"[$prefix@$i] nextValid= ${debugIO.doReadWire.peek()}"
        )
      }
      theClock.step()
      if (printLogDetails) println(s"[$prefix@$i]  done     = ${doneIO.peek()}")
      doneIO.expect((i == theData.length - 1).B, s"[$prefix@$i] Data($i) = ${theData(i)}, " +
        s"$prefix should finish?")
      println(s"[$prefix@$i] PASS $i")
    }
  }
  private def writeInActAdrAndData(theInIO: CSCStreamIO, theDebugIO: InActSRAMBankDebugIO, adrStream: List[Int],
                                   dataStream: List[Int], theClock: Clock): Unit = {
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
  // test GLB Cluster
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
      test(new InActSRAMCommon(inActAdrSRAMSize, inActAdrWidth, true)) { theAdrSRAM =>
        val theTopIO = theAdrSRAM.io
        val theClock = theAdrSRAM.clock
        val InActAdrStream = inActAdrStream.head
        val inActAdrLookUp = getStreamLookUp(InActAdrStream)
        println(s"have ${inActAdrLookUp.length} inActStreams")
        println(s"inActAdrLookUp = $inActAdrLookUp")
        println(s"theDataStream = $InActAdrStream")
        println(s"streamNum = $inActStreamNum")
        println(s"zeroNum = ${InActAdrStream.collect({case x if x == 0 => x}).length}")
        require(InActAdrStream.length <= inActAdrSRAMSize, s"the size of current inAct should less than inActAdrSRAMSize, " +
          s"but ${InActAdrStream.length} > $inActAdrSRAMSize")
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
        println(s"[inActCommonSRAMAdr] inInc = ${theTopIO.debugIO.indexInc.peek()}")
        theClock.step(cycles = (new Random).nextInt(5) + 5)
        println("--------------- begin to read ----------------")
        println(s"[inActCommonSRAMAdr] inInc = ${theTopIO.debugIO.indexInc.peek()}")
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
                      readOutAct(theTopIO.dataPath.outIOs, theTopIO.debugIO, theTopIO.ctrlPath.readIO.done,
                        InActAdrStream, theRdIdx, theClock, prefix = "inActCommonSRAMAdr")
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
      val InActAdrStream = inActAdrStream.head
      val InActDataStream = inActDataStream.head
      println("----- inActReadCycle = " + InActAdrStream.length)
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
        InActAdrStream, InActDataStream, theClock)
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
        theInActCtrl(index).writeIO.enable.poke(true.B)
        theClock.step() // sub from idle to doing;
        theTopIO.debugIO.inActDebugIO(index).theState.expect(1.U, s"the inActSRAM $index should doing now")
        println(s"[inActSRAMBank$index] inActSRAMState$index =  ${theTopIO.debugIO.inActDebugIO(index).theState.peek()}")
        writeInActAdrAndData(theTopIO.dataPath.inActIO(index).inIOs, theTopIO.debugIO.inActDebugIO(index),
          theInActAdrStreams(index), theInActDataStreams(index), theClock)
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

  behavior of "test the spec of PE Cluster"
  it should "work well on PE inAct Controller" in {
    test (new PEClusterInAct) { thePEAct =>
      val theTopIO = thePEAct.io
      val theClock = thePEAct.clock
      val theTopToCtrlDataIO = theTopIO.inActToArrayData.inActIO // inActRouter number
      val theCtrlToPEDataIO = theTopIO.inActToArrayData.muxInActData // (peRow, peCol)
      val theCtrlIO = theTopIO.inActCtrlSel
      val theDoneIO = theTopIO.inActWriteFinVec // (peRow, peCol)
      def inActIOPokeHelper(idx: Int): Unit = {
        fork {
          var pokeIdx: Int = 0
          while (theInActAdrStreams(idx)(pokeIdx) != 0) {
            val prefix: String = s"inActIO$idx@Adr@$pokeIdx"
            println(s"[$prefix] poke ${theInActAdrStreams(idx)(pokeIdx)} now")
            theTopToCtrlDataIO(idx).adrIOs.data.bits.poke(theInActAdrStreams(idx)(pokeIdx).U)
            theTopToCtrlDataIO(idx).adrIOs.data.valid.poke(true.B)
            theClock.step()
            //theTopToCtrlDataIO(idx).adrIOs.data.ready.expect(true.B)
            println(s"[$prefix] t + 1, ready = ${theTopToCtrlDataIO(idx).adrIOs.data.ready.peek()}")
            pokeIdx = pokeIdx + 1
          }
          theTopToCtrlDataIO(idx).adrIOs.data.valid.poke(false.B)
          theClock.step()
        } .fork {
          var pokeIdx: Int = 0
          while (theInActDataStreams(idx)(pokeIdx) != 0) {
            val prefix: String = s"inActIO$idx@Data@$pokeIdx"
            println(s"[$prefix] poke ${theInActDataStreams(idx)(pokeIdx)} now")
            theTopToCtrlDataIO(idx).dataIOs.data.bits.poke(theInActDataStreams(idx)(pokeIdx).U)
            theTopToCtrlDataIO(idx).dataIOs.data.valid.poke(true.B)
            theClock.step()
            //theTopToCtrlDataIO(idx).dataIOs.data.ready.expect(true.B)
            println(s"[$prefix] t + 1, ready = ${theTopToCtrlDataIO(idx).dataIOs.data.ready.peek()}")
            pokeIdx = pokeIdx + 1
          }
          theTopToCtrlDataIO(idx).dataIOs.data.valid.poke(false.B)
          theClock.step()
        } .join()
      }
      def muxInActPeekHelper(row: Int, col: Int): Unit = {
        val inActIdx: Int = (row + col) % inActRouterNum
        fork {
          var formerOrLater: Boolean = (row + col) < inActRouterNum
          var peekIdx: Int = 0
          var prefix: String = s"muxInActIO@$row@$col@Adr@$peekIdx"
          // while (formerOrLater) {} TODO: when finish, then later ones should begin to peek
          while (theInActAdrStreams(inActIdx)(peekIdx) != 0 && formerOrLater) {
            println(s"[$prefix] now valid = ${theCtrlToPEDataIO(row)(col).adrIOs.data.valid.peek()}")
            while (theCtrlToPEDataIO(row)(col).adrIOs.data.valid.peek().litToBoolean) {
              prefix = s"muxInActIO@$row@$col@Adr@$peekIdx"
              println(s"[$prefix] peek ${theInActAdrStreams(inActIdx)(peekIdx)} now")
              theCtrlToPEDataIO(row)(col).adrIOs.data.ready.poke(true.B)
              theCtrlToPEDataIO(row)(col).adrIOs.data.bits.expect(theInActAdrStreams(inActIdx)(peekIdx).U)
              theClock.step()
              peekIdx = peekIdx + 1
            }
          }
          theClock.step()
        } .fork {
          var formerOrLater: Boolean = (row + col) < inActRouterNum
          var peekIdx: Int = 0
          var prefix: String = s"muxInActIO@$row@$col@Data@$peekIdx"
          while (theInActDataStreams(inActIdx)(peekIdx) != 0 && formerOrLater) {
            println(s"[$prefix] now valid = ${theCtrlToPEDataIO(row)(col).dataIOs.data.valid.peek()}")
            while (theCtrlToPEDataIO(row)(col).dataIOs.data.valid.peek().litToBoolean) {
              prefix = s"muxInActIO@$row@$col@Adr@$peekIdx"
              println(s"[$prefix] peek ${theInActDataStreams(inActIdx)(peekIdx)} now")
              theCtrlToPEDataIO(row)(col).dataIOs.data.ready.poke(true.B)
              theCtrlToPEDataIO(row)(col).dataIOs.data.bits.expect(theInActDataStreams(inActIdx)(peekIdx).U)
              theClock.step()
              peekIdx = peekIdx + 1
            }
            theClock.step()
          }
        } .join()
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
      fork { // poke via inActIO
        (1 until inActRouterNum).foldLeft(fork(inActIOPokeHelper(0))) {
          case (left, right) => left.fork(inActIOPokeHelper(idx = right))
        } .join()
      } .fork.withRegion(Monitor) { // peek via muxInActData, so we need to use region to peek from another thread
        (1 until peRowNum).foldLeft((1 until peColNum).foldLeft(fork(muxInActPeekHelper(0, 0))){
          case (left, right) => left.fork(muxInActPeekHelper(0, col = right))
        }) {
          case (left, right) => (1 until peColNum).foldLeft(fork(muxInActPeekHelper(row = right, col = 0))) {
            case (leftt, rightt) => leftt.fork(muxInActPeekHelper(row = right, col = rightt))
          }
        } .join()
      } .joinAndStep(theClock)
    }
  }
  it should "work well on PE Cluster" in {
    test (new PECluster(true)) { thePECluster =>
      val theTopIO = thePECluster.io
      val theClock = thePECluster.clock
      val pSumDataIO = theTopIO.dataPath.pSumIO
      val inActDataIO = theTopIO.dataPath.inActIO
      val weightDataIO = theTopIO.dataPath.weightIO
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
        pSumDataIO.inIOs(idx).valid.poke(true.B)
        pSumDataIO.inIOs(idx).bits.poke(addendRand(idx).head.U) // FIXME
        println(s"-------- $idx-th Column PEs receive all inPSum")
      }
      def forkWriteCSCHelper(index: Int, theCSCIO: CSCStreamIO, theAdrStream: List[Int],
                             theDataStream: List[Int]): Unit = {
        fork {
          println(s"------------- write adr $index begin --------------")
          writeCSCPECluster(theCSCIO.adrIOs, theAdrStream)
          println(s"------------ write adr $index finish --------------")
        } .fork {
          println(s"------------ write data $index begin --------------")
          writeCSCPECluster(theCSCIO.dataIOs, theDataStream)
          println(s"------------ write data $index finish -------------")
        } .joinAndStep(theClock)
      }
      println("----------------- test begin -----------------")
      println("------------ PE Cluster Top Spec -------------")
      println("----------- test basic functions -------------")
      thePECluster.reset.poke(true.B)
      theClock.step()
      thePECluster.reset.poke(false.B)
      theClock.step()
      println("--------------- begin to load ----------------")
      theTopIO.ctrlPath.pSumCtrlSel.inDataSel.poke(true.B) // receive data from PSum Router
      theTopIO.ctrlPath.inActCtrlSel.inDataSel.poke(false.B) // not broad-cast
      theTopIO.ctrlPath.doEn.poke(true.B) // begin to load inAct and weight, then cal
      theClock.step()
      val inActRegion = Monitor
      val weightRegion = Monitor
      fork {
        (1 until inActSRAMNum).foldLeft( fork.withRegion(inActRegion){forkWriteCSCHelper(index = 0, theCSCIO = inActDataIO(0),
          theAdrStream = theInActAdrStreams.head, theDataStream = theInActDataStreams.head)}) {
          case (left, right) => left.fork{forkWriteCSCHelper(index = right, theCSCIO = inActDataIO(right),
            theAdrStream = theInActAdrStreams(right), theDataStream = theInActDataStreams(right))}
        } .joinAndStep(theClock)
      } .fork {
        (1 until weightRouterNum).foldLeft( fork.withRegion(weightRegion){forkWriteCSCHelper(index = 0, theCSCIO = weightDataIO(0),
          theAdrStream = theWeightAdrStreams.head, theDataStream = theWeightDataStreams.head)}) {
          case (left, right) => left.fork{forkWriteCSCHelper(index = right, theCSCIO = weightDataIO(right),
            theAdrStream = theWeightAdrStreams(right), theDataStream = theWeightDataStreams(right))}
        } .joinAndStep(theClock)
      } .join()
      // after finish
      //theTopIO.ctrlPath.allCalFin.expect(true.B)
      theTopIO.ctrlPath.pSumLoadEn.poke(true.B) // begin to accumulate PSum
      (1 until peColNum).foldLeft(fork(forkPSumHelper(0))) {
        case (left, right) => left.fork(forkPSumHelper(idx = right))
      }.join()
    }
  }
  behavior of "test the spec of Router Cluster"
  it should "work well on PSum Router Cluster" in {
    test (new PSumRouter) { thePSRouter =>
      val theTop = thePSRouter.io
      val theCtrlIO = theTop.ctrlPath
      val theDataIO = theTop.dataPath
      val theClock = thePSRouter.clock
      val randomDelay = (new Random).nextInt(50) + 1
      println("----------------- test begin -----------------")
      println("-------------- PSum Router Spec --------------")
      println("----------- test basic functions -------------")
      thePSRouter.reset.poke(true.B)
      theClock.step()
      thePSRouter.reset.poke(false.B)
      theClock.step()
      // always read from PE and send to GLB
      theCtrlIO.inDataSel.poke(true.B) // read from GLB
      theCtrlIO.outDataSel.poke(true.B) // send it to PE
      // begin to load PSum from GLB and send it into PEArray,
      // then back from PEArray and write back to GLB
      fork {
        println("------------ begin to poke @ GLB -------------")
        theDataIO.inIOs(1).bits.poke(1.U)
        theDataIO.inIOs(1).valid.poke(true.B)
        //theDataIO.inIOs(1).ready.expect(true.B)
        println(theDataIO.inIOs(1).ready.peek())
        theClock.step()
        println(theDataIO.inIOs(1).ready.peek())
        theClock.step(randomDelay)
      } .fork.withRegion(Monitor).withName("PSumReadToPE") {
        println("---------- begin to peek @ PEArray -----------")
        theDataIO.outIOs(0).bits.expect(1.U)
        theDataIO.outIOs(0).valid.expect(true.B)
        theDataIO.outIOs(0).ready.poke(true.B)
        theClock.step()
        theClock.step(randomDelay)
      } .fork.withRegion(Monitor).withName("PSumBackFromPE") {
        println("---------- begin to poke @ PEArray -----------")
        theClock.step(randomDelay)
        theDataIO.inIOs(0).bits.poke(2.U)
        theDataIO.inIOs(0).valid.poke(true.B)
        theDataIO.inIOs(0).ready.expect(true.B)
        theClock.step()
      } .fork.withRegion(Monitor).withName("PSumBackToGLB") {
        println("------------ begin to peek @ GLB -------------")
        theClock.step(randomDelay)
        theDataIO.outIOs(1).bits.expect(2.U)
        theDataIO.outIOs(1).valid.expect(true.B)
        theDataIO.outIOs(1).ready.poke(true.B)
        theClock.step()
      } .joinAndStep(theClock)
    }
  }
  it should "work well on Router Cluster" in {
    test (new RouterCluster(true)) { theRCluster =>
      val theTop = theRCluster.io
      val theClock = theRCluster.clock
      theTop.dataPath.routerData.iRIO.head.inIOs.foreach(x =>
        require(DataMirror.directionOf(x.dataIOs.data.bits) == Direction.Input))
      theTop.dataPath.routerData.iRIO.head.outIOs.foreach(x =>
        require(DataMirror.directionOf(x.dataIOs.data.bits) == Direction.Output))
      println("----------------- test begin -----------------")
      println("---------- Router Cluster Top Spec -----------")
      println("----------- test basic functions -------------")
      theRCluster.reset.poke(true.B)
      theClock.step()
      theRCluster.reset.poke(false.B)
      theClock.step()
    }
  }
  behavior of "test the spec of Cluster Group"
  //chisel3.Driver.emitVerilog(new ClusterGroup(false))
  it should "work well on Cluster Group" in {
    test (new ClusterGroup(true)) { theCG =>
      val theTop = theCG.io
      val theClock = theCG.clock
      theCG.reset.poke(true.B)
      theClock.step()
      theCG.reset.poke(false.B)
    }
  }
}

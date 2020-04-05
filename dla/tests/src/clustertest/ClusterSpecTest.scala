package dla.tests.clustertest

import Chisel.DecoupledIO
import chisel3._
import chisel3.experimental.{DataMirror, Direction}
import chisel3.stage._
import chisel3.tester._
import dla.cluster._
import dla.eyerissTop.EyerissTopConfig
import dla.pe.{CSCStreamIO, MCRENFConfig, SPadSizeConfig, StreamBitsIO}
import dla.tests.GenOneStreamData
import firrtl.options.TargetDirAnnotation
import org.scalatest._

import scala.util.Random
import scala.util.matching.Regex
import scala.math.pow

class ClusterSpecTest extends FlatSpec with ChiselScalatestTester with Matchers
  with ClusterSRAMConfig with MCRENFConfig with SPadSizeConfig with GNMFCS2Config
  with EyerissTopConfig with GNMFCS1Config {
  //private val printLogDetails = false // true to print more detailed logs
  private val printLogDetails = true // true to print more detailed logs
  private val maxPSumStreamNum: Int = pSumSRAMSize/pSumOneSPadNum
  private val addendRand = Seq.fill(peColNum, pSumOneSPadNum){(new Random).nextInt(10)}
  private val peNum = peRowNum * peColNum * cgRowNum * cgColNum
  private val oneStreamData = new GenOneStreamData
  private val inActAdrStream = oneStreamData.inActAdrStream
  private val inActDataStream = oneStreamData.inActDataStream
  private val weightAdrStream = oneStreamData.weightAdrStream
  private val weightDataStream = oneStreamData.weightDataStream
  private val pSumStream = oneStreamData.outPSumStream
  private val theInActAdrStreams = inActAdrStream.take(inActRouterNum*2) // in actual, it needs s2 + f2
  private val theInActDataStreams = inActDataStream.take(inActRouterNum*2)
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
    outIO.data.bits.expect(theData(idx).U,
      s"[$prefix] theData($idx) = ${theData(idx)}, current idx = " +
      s"${debugIO.idx.peek()}, data length = ${theData.length}\n" +
        s"theData is\n $theData \n" +
      s"[$prefix] $idx.5 done = ${doneIO.peek()}\n" +
      s"[$prefix] $idx.5 valid= ${outIO.data.valid.peek()}\n" +
      s"[$prefix] $idx.5 crDt = ${debugIO.currentData.peek()}\n" +
      s"[$prefix] $idx.5 inInc= ${debugIO.indexInc.peek()}\n" +
      s"[$prefix] $idx.5 waFR = ${debugIO.waitForRead.peek()}\n" +
      s"[$prefix] $idx.5 doRd = ${debugIO.doReadWire.peek()}\n" +
      s"[$prefix] $idx.5 data = ${outIO.data.bits.peek()}\n" +
      s"[$prefix] $idx.5 idx  = ${debugIO.idx.peek()}")
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
  private def writeInAct(inIO: StreamBitsIO, debugIO: SRAMCommonDebugIO with InActSpecialDebugIO,
                         doneIO: Bool, theData: List[Int], theClock: Clock, prefix: String) : Unit = {
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
    test (new PEClusterInAct(debug = true)) { thePEAct =>
      val theTopIO = thePEAct.io
      val theClock = thePEAct.clock
      val theTopToCtrlDataIO = theTopIO.inActToArrayData.inActIO // inActRouter number
      val theCtrlToPEDataIO = theTopIO.inActToArrayData.muxInActData // (peRow, peCol)
      val theCtrlIO = theTopIO.inActCtrlSel
      val theDoneIO = theTopIO.inActWriteFinVec // (peRow, peCol)
      val theDebugIO = theTopIO.debugIO
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
      /*fork { // poke via inActIO
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
      } .joinAndStep(theClock)*/
    }
  }

  it should "work well on PE Cluster" in {
    test (new PECluster(true)) { thePECluster =>
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
                  timescope {
                    theClock.step()
                    println(s"[$thePrefix@R$i] t1 inActState = ${int.peek()}")
                    int.expect(1.U, s"[$thePrefix@R$i] should it be former or later?\n " +
                      s"adr = ${whetherInActDone(i)}\n" +
                      s"data = ${whetherInActDone(i+inActRouterNum)}")
                  }
                }
              } else {
                if (!(whetherInActDone(i+inActRouterNum*2) && whetherInActDone(i+inActRouterNum*3))) {
                  int.expect(1.U, s"[$thePrefix@R$i] should it be former or later?\n " +
                    s"adr = ${whetherInActDone(i+inActRouterNum*2)}\n" +
                    s"data = ${whetherInActDone(i+inActRouterNum*3)}")
                } else {
                  timescope {
                    theClock.step()
                    println(s"[$thePrefix@R$i] t1 inActState = ${int.peek()}")
                    int.expect(0.U, s"[$thePrefix@R$i] should it be former or later?\n " +
                      s"adr = ${whetherInActDone(i+inActRouterNum*2)}\n" +
                      s"data = ${whetherInActDone(i+inActRouterNum*3)}")
                  }
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
            if (theStreamReadIdx(routerIdx) < theLookup(routerIdx)(1)) { // until the end of first stream
              if (true)
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
                    if (formerOrLater) {
                      o.writeFinishRegVec(writeFinRegIdx).expect(conFunc(row, col).B,
                        s"[$prefix] should $writeFinRegIdx done?")
                    } else {
                      o.writeFinishRegVec(writeFinRegIdx).expect(true.B,
                        s"[$prefix] should $writeFinRegIdx done?")
                    }
                  }
                })})
              println(s"[$thePrefix@Router$routerIdx] have poked" +
                s" ${theStreamReadIdx(routerIdx) - 1} data, the last one is " +
                s"${thePokeStream(routerIdx)(theStreamReadIdx(routerIdx) - 1)}")
            }
          }
          theClock.step()
          /** check finish reg*/
          if (itIsInAct) {
            for (row <- 0 until peRowNum) {
              for (col <- 0 until peColNum) {
                val routerIdx = (row + col) % inActRouterNum
                val lastPoke: Boolean = thePokeStream(routerIdx)(theStreamReadIdx(routerIdx) - 1) == 0
                val validIdx: Int = if (itIsData) 1 else 0
                val whetherValid: Boolean = !lastPoke && conFunc(row, col)
                if (lastPoke) {
                  thePokeIO(routerIdx).valid.poke(false.B)
                  if (formerOrLater) {
                    theDebugIO.eachPETopDebug(row)(col).writeFinishRegVec(validIdx).expect(conFunc(row, col).B,
                      s"[$thePrefix@Router$routerIdx@Row$row@Col$col] should it finish?")
                  } else {
                    theDebugIO.eachPETopDebug(row)(col).writeFinishRegVec(validIdx).expect(true.B,
                      s"[$thePrefix@Router$routerIdx@Row$row@Col$col] should it finish?")
                  }
                  if (printLogDetails) println(s"[$thePrefix@Router$routerIdx@Row$row@Col$col] " +
                    s"DataValid ${theDebugIO.eachPEInActValid(validIdx)(row)(col).peek()}\n" +
                    s"[$thePrefix@Router$routerIdx@Row$row@Col$col] " +
                    s"writeFinReg = ${theDebugIO.eachPETopDebug(row)(col).writeFinishRegVec(validIdx).peek()}")
                  theDebugIO.eachPEInActValid(validIdx)(row)(col).expect(whetherValid.B,
                    s"[$thePrefix@Router$routerIdx@Row$row@Col$col] should it Valid now? " +
                      s"writeFinReg = ${theDebugIO.eachPETopDebug(row)(col).writeFinishRegVec(validIdx).peek()}")
                  println(s"[$thePrefix@Router$routerIdx] ioState = ${theDebugIO.inActDataIOState(routerIdx).peek()}")
                } else {
                  theDebugIO.eachPEInActValid(validIdx)(row)(col).expect(conFunc(row, col).B,
                    s"[$thePrefix@Router$routerIdx@Row$row@Col$col] should it Valid now? " +
                      s"writeFinReg = ${theDebugIO.eachPETopDebug(row)(col).writeFinishRegVec(validIdx).peek()}")
                }
              }
            }
          }
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
            if (conFunc(row, col)) {
              theDebugIO.eachPETopDebug(row)(col).writeFinishRegVec(theRFRegVecIdx).expect(true.B, conMessage)
            } else {
              theDebugIO.eachPETopDebug(row)(col).writeFinishRegVec(theRFRegVecIdx).expect(false.B, elseMessage)
            }
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
      /*
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
      */
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
      val theOrWeights = oneStreamData.weightStream
      val theOrInActs = oneStreamData.inActStream
      def getData(stream: Seq[List[Int]]): Seq[List[Int]] = {
        val dataStream: Seq[List[Int]] = stream.map(x => x.map(y => Integer.parseInt(y.toBinaryString.take(8), 2)))
        dataStream
      }
      for (i <- 0 until pSumRouterNum) {
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
      }
      theOrInActs.take(inActRouterNum*2).zipWithIndex.foreach({ case (list, i) => println(s"goldenInAct$i = $list")})
      println(s"pokeInAct = ${getData(theInActDataStreams).zip(theInActAdrStreams)}")
      theOrWeights.take(peRowNum).zipWithIndex.foreach({ case (list, i) => println(s"goldenWeight$i = $list")})
      println(s"pokeWeight = ${getData(theWeightDataStreams).zip(theWeightAdrStreams)}")
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
      println("------------ begin to poke @ GLB -------------")
      theDataIO.inIOs(1).bits.poke(1.U)
      theDataIO.inIOs(1).valid.poke(true.B)
      println("---------- begin to peek @ PEArray -----------")
      theDataIO.outIOs(0).bits.expect(1.U)
      theDataIO.outIOs(0).valid.expect(true.B)
      theDataIO.outIOs(0).ready.poke(true.B)
      println("------------ expect ready @ GLB --------------")
      theDataIO.inIOs(1).ready.expect(true.B, "port from GLB should ready now")
      theClock.step()
      theDataIO.inIOs(1).valid.poke(false.B)
      theDataIO.outIOs(0).ready.poke(false.B)
      theDataIO.inIOs(1).ready.expect(false.B)
      theClock.step(randomDelay)
      println("---------- begin to poke @ PEArray -----------")
      theDataIO.inIOs(0).bits.poke(2.U)
      theDataIO.inIOs(0).valid.poke(true.B)
      println("------------ begin to peek @ GLB -------------")
      theDataIO.outIOs(1).bits.expect(2.U)
      theDataIO.outIOs(1).valid.expect(true.B)
      theDataIO.outIOs(1).ready.poke(true.B)
      theDataIO.outIOs(0).valid.expect(false.B, "not use now, so you should be false")
      theDataIO.outIOs(2).valid.expect(false.B, "not use now, so you should be false")
      println("---------- expect ready @ PEArray ------------")
      theDataIO.inIOs(0).ready.expect(true.B)
      theDataIO.inIOs.takeRight(2).foreach(x => x.ready.expect(false.B, "not use now, so you should be false"))
      theClock.step()
    }
  }

  it should "work well on InAct Router Cluster" in {
    test (new InActRouter) { theInActRouter =>
      val theTop = theInActRouter.io
      val theClock = theInActRouter.clock
      val theDataIO = theTop.dataPath
      val theCtrlIO = theTop.ctrlPath
      val randomInIO = (new Random).nextInt(inActPortNum)
      val randomInAdr = Seq.fill(inActPortNum) {(new Random).nextInt(pow(2, inActAdrWidth).toInt)}
      println("----------------- test begin -----------------")
      println("------------- InAct Router Spec --------------")
      println("------ test uni-cast with random inPort ------")
      theInActRouter.reset.poke(true.B)
      theClock.step()
      theInActRouter.reset.poke(false.B)
      theClock.step()
      theCtrlIO.inDataSel.poke(randomInIO.U) // from random port
      theCtrlIO.outDataSel.poke(0.U) // uni-cast
      for (inPort <- 0 until inActPortNum) {
        println(s"peek inActIO$inPort with data ${randomInAdr(inPort)}, valid = ${inPort == randomInIO}")
        theDataIO.inIOs(inPort).adrIOs.data.bits.poke(randomInAdr(inPort).U)
        theDataIO.inIOs(inPort).adrIOs.data.valid.poke((inPort == randomInIO).B)
      }
      theDataIO.outIOs(0).adrIOs.data.bits.expect(randomInAdr(randomInIO).U)
      theDataIO.outIOs(0).adrIOs.data.valid.expect(true.B)
      theDataIO.outIOs.takeRight(inActPortNum - 1).foreach(x => x.adrIOs.data.valid.expect(false.B))
      theDataIO.outIOs(0).adrIOs.data.ready.poke(true.B)
      for (inPort <- 0 until inActPortNum) {
        println(s"expect inActIO$inPort ready = ${inPort == randomInIO}")
        theDataIO.inIOs(inPort).adrIOs.data.ready.expect((inPort == randomInIO).B, s"randomInIO = $randomInIO" +
          s"should it ready now?")
      }
      theClock.step()
    }
    // TODO: add more test case for different cast mode
  }

  it should "work well on Weight Router Cluster" in {
    test (new WeightRouter) { theWeightRouter =>
      val theTop = theWeightRouter.io
      val theClock = theWeightRouter.clock
      val theCtrlIO = theTop.ctrlPath
      val theDataIO = theTop.dataPath
      val randomInIO = Seq.fill(2){(new Random).nextInt(weightPortNum)}
      val randomInAdr = Seq.fill(weightPortNum) {(new Random).nextInt(pow(2, weightAdrWidth).toInt)}
      println("----------------- test begin -----------------")
      println("------------- Weight Router Spec -------------")
      println("---------- test weight connections -----------")
      println("----------- with random in/OutPort -----------")
      theWeightRouter.reset.poke(true.B)
      theClock.step()
      theWeightRouter.reset.poke(false.B)
      theClock.step()
      theCtrlIO.inDataSel.poke((randomInIO.head == 1).B) // true to port 1, false to port 0
      theCtrlIO.outDataSel.poke((randomInIO.last == 1).B)
      for (inPort <- 0 until weightPortNum) {
        val prefix: String = s"weight@inIOsAdr@$inPort"
        theDataIO.inIOs(inPort).adrIOs.data.bits.poke(randomInAdr(inPort).U)
        theDataIO.inIOs(inPort).adrIOs.data.valid.poke((inPort == randomInIO.head).B)
        println(s"[$prefix] bits = ${randomInAdr(inPort)}, valid = ${inPort == randomInIO.head}")
      }
      for (outPort <- 0 until weightPortNum) {
        val prefix: String = s"weight@outIOsAdr@$outPort"
        println(s"[$prefix] valid = ${outPort === randomInIO.last || outPort == 0}")
        if (outPort === randomInIO.last || outPort == 0) { // always send to PEArray
          println(s"[$prefix] bits = ${randomInAdr(outPort)}")
          theDataIO.outIOs(outPort).adrIOs.data.valid.expect(true.B)
          theDataIO.outIOs(outPort).adrIOs.data.bits.expect(randomInAdr(randomInIO.head).U)
          theDataIO.outIOs(outPort).adrIOs.data.ready.poke(true.B)
        } else {
          theDataIO.outIOs(outPort).adrIOs.data.valid.expect(false.B)
        }
      }
      theDataIO.inIOs(randomInIO.head).adrIOs.data.ready.expect(true.B)
      theClock.step()
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
      // TODO: add some detailed tests on RouterCluster
    }
  }
  behavior of "test the spec of Cluster Group"
  it should "work well on Cluster Group Controller" in {
    test (new ClusterGroupController(debug = true)) { theCGCtrl =>
      val theTop = theCGCtrl.io
      val theDebugIO = theTop.debugIO
      val theClock = theCGCtrl.clock
      var inActReadAdr: List[Int] = Nil
      var pSumWriteAdr: List[Int] = Nil
      var pSumReadAdr: List[Int] = Nil
      def randomGiveFin(pokeIO: Seq[Bool], expectIO: Seq[Bool], idx: Int, prefix: String): Unit = {
        theClock.step((new Random).nextInt(15))
        pokeIO(idx).poke(true.B)
        println(s"[$prefix$idx] poke true now")
        theClock.step()
        pokeIO(idx).poke(false.B)
        expectIO(idx).expect(true.B, s"[$prefix$idx] one cycle later, the reg should be true")
        if (printLogDetails) println(s"[$prefix$idx] cgState = ${theDebugIO.cgState.peek().litValue()}")
      }
      def inActReadHelper(pokeIO: Seq[Bool], expectIO: Seq[Bool], idx: Int, prefix: String): Unit = {
        println(s"[$prefix] inActReadAdr = ${theTop.glbInActCtrlIOs(idx).readIO.adr.peek()}")
        val currentAdr: Int = theTop.glbInActCtrlIOs(idx).readIO.adr.peek().litValue().toInt
        if (!inActReadAdr.contains(currentAdr)) {
          inActReadAdr = inActReadAdr:::List(currentAdr)
        }
        //theTop.glbInActCtrlIOs(idx).readIO.adr.expect() // TODO: check the address
        randomGiveFin(pokeIO, expectIO, idx, prefix)
        theTop.glbInActCtrlIOs(idx).readIO.enable.expect(false.B, s"as inAct $idx has been read from GLB")
      }
      def pSumRWHelper(pokeIO: Vec[SRAMCommonCtrlIO], expectIO: Seq[Bool], idx: Int, prefix: String): Unit = {
        println(s"[$prefix] pSumReadAdr = ${theTop.glbPSumCtrlIOs(idx).readIO.adr.peek()}")
        println(s"[$prefix] pSumWriteAdr = ${theTop.glbPSumCtrlIOs(idx).writeIO.adr.peek()}")
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
        println(s"[${prefix}Write$idx] poke true now")
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
      theTop.allPSumAddFin.poke(false.B)
      theTop.allCalFin.poke(false.B)
      theTop.topIO.cgEnable.poke(true.B)
      theClock.step()
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
      for (n2 <-0 until N2) {
        for (m2 <- 0 until M2) {
          for (f2 <- 0 until F2) {
            for (c2 <- 0 until C2) {
              for (s2 <- 0 until S2) {
                if (printLogDetails) println(s"[$c2,$s2] cgState = ${theDebugIO.cgState.peek().litValue()}")
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
                theDebugIO.cgState.expect(3.U, s"[$c2,$s2] after read inAct from GLB, it should do computation")
                theClock.step((new Random).nextInt(15))
                theTop.allCalFin.poke(true.B)
                theClock.step()
                theTop.allCalFin.poke(false.B)
              } // end of S2 loop
            } // end of C2 loop
            println(s"state = ${theDebugIO.cgState.peek()}")
            theDebugIO.cgState.expect(4.U, s"after S2 = $S2 computations, it should read PSum Now")
            theTop.glbPSumCtrlIOs.foreach(_.readIO.enable.expect(true.B, s"[] Should read PSum out from GLB now"))
            (1 until pSumSRAMNum).foldLeft( fork {
              pSumRWHelper( pokeIO = theTop.glbPSumCtrlIOs,
                expectIO = theDebugIO.pSumWriteFinVecIO, idx = 0, prefix = s"@pSumWrite")
            }) {
              case (left, right) =>
                left.fork {
                  pSumRWHelper( pokeIO = theTop.glbPSumCtrlIOs,
                    expectIO = theDebugIO.pSumWriteFinVecIO, idx = right, prefix = s"@pSumWrite")
                  theDebugIO.cgState.expect(4.U, "it should be cgRead")
                }
            }.join()
            theTop.allPSumAddFin.poke(true.B)
            theClock.step() // wait for state machine
            theTop.allPSumAddFin.poke(false.B)
            theDebugIO.cgState.expect(2.U, "it should be PELoad again")
          } // end of F2 loop
        } // end of M2 loop
      } // end of N2 loop
      println(s"inActReadAdr = \n $inActReadAdr")
      println(s"pSumWriteAdr = \n $pSumWriteAdr")
      println(s"pSumReadAdr = \n $pSumReadAdr")
      println("----------------- test success -----------------")
    }
  }
/*
  it should "work well on reading and writing via inner SRAM" in {
    test (new ClusterGroup(true)) { theCG =>
      val theTop = theCG.io
      val theClock = theCG.clock
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
      def pokeData(pokeIO: DecoupledIO[UInt], pokeData: List[Int], prefix: String): Unit = {
        var pokeIdx = 0
        while (pokeIdx < pokeData.length) {
          pokeIO.bits.poke(pokeData(pokeIdx).U)
          pokeIO.valid.poke(true.B)
          if (pokeIO.ready.peek().litToBoolean) {
            println(s"[$prefix@$pokeIdx] poked ${pokeData(pokeIdx)}")
            pokeIdx += 1
          } else {
            println(s"[$prefix@$pokeIdx] not ready now")
          }
          theClock.step()
        }
        pokeIO.valid.poke(false.B)
      }
      theTop.ctrlPath.doMacEn.poke(true.B)
      theClock.step(2)
      fork.withName("pokeInActAdr") {
        theTop.dataPath.glbDataPath.inActIO.zipWithIndex.foreach({ case (o, i) =>
          val prefix: String = s"inActAdr$i"
          pokeData(o.inIOs.adrIOs.data, theInActAdrStreams(i), prefix)
        })
      } .fork.withName("pokeInActData") {
        theTop.dataPath.glbDataPath.inActIO.zipWithIndex.foreach({ case (o, i) =>
          val prefix: String = s"inActData$i"
          pokeData(o.inIOs.dataIOs.data, theInActDataStreams(i), prefix)
        })
      } .joinAndStep(theClock)
      println("poke later data now")
      fork.withName("pokeInActAdrLater") {
        theTop.dataPath.glbDataPath.inActIO.zipWithIndex.foreach({ case (o, i) =>
          val prefix: String = s"inActAdr${i+inActRouterNum}"
          pokeData(o.inIOs.adrIOs.data, theInActAdrStreams(i+inActRouterNum), prefix)
        })
      } .fork.withName("pokeInActDataLater") {
        theTop.dataPath.glbDataPath.inActIO.zipWithIndex.foreach({ case (o, i) =>
          val prefix: String = s"inActData${i+inActRouterNum}"
          pokeData(o.inIOs.dataIOs.data, theInActDataStreams(i+inActRouterNum), prefix)
        })
      } .joinAndStep(theClock)
      println("when it begins to cal, when it will need weight")
      theClock.step()
      fork.withName("pokeWeightAdr") {
        theTop.dataPath.glbDataPath.weightIO.zipWithIndex.foreach({ case (o, i) =>
            val prefix: String = s"weightAdr$i"
            pokeData(o.inIOs.adrIOs.data, theWeightAdrStreams(i), prefix)
          })
        } .fork.withName("pokeWeightData") {
          theTop.dataPath.glbDataPath.weightIO.zipWithIndex.foreach({ case (o, i) =>
            val prefix: String = s"weightData$i"
            pokeData(o.inIOs.dataIOs.data, theWeightDataStreams(i), prefix)
          })
        } .join()
    }
  }*/
}

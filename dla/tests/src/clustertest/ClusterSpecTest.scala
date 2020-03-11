package dla.tests.clustertest

import Chisel.DecoupledIO
import chisel3._
import chisel3.experimental.{DataMirror, Direction}
import chisel3.tester._
import dla.cluster._
import dla.pe.{CSCStreamIO, MCRENFConfig, SPadSizeConfig, StreamBitsIO}
import dla.tests.GenOneStreamData
import org.scalatest._
import scala.util.Random
import scala.math.max

class ClusterSpecTest extends FlatSpec with ChiselScalatestTester with Matchers
  with ClusterSRAMConfig with MCRENFConfig with SPadSizeConfig with GNMFCS2Config {
  //private val printLogDetails = false // true to print more detailed logs
  private val printLogDetails = true // true to print more detailed logs
  private val maxPSumStreamNum: Int = pSumSRAMSize/pSumOneSPadNum
  private val addendRand = Seq.fill(peColNum, pSumOneSPadNum){(new Random).nextInt(10)}
  private val oneStreamData = Seq.fill(max(inActRouterNum, pSumRouterNum)){new GenOneStreamData}
  private val inActAdrStream = oneStreamData.map(_.inActAdrStream)
  private val inActDataStream = oneStreamData.map(_.inActDataStream)
  private val inActCountStream = oneStreamData.map(_.inActCountStream)
  private val weightAdrStream = oneStreamData.map(_.weightAdrStream)
  private val weightDataStream = oneStreamData.map(_.weightDataStream)
  private val weightCountStream = oneStreamData.map(_.weightCountStream)
  private val pSumStream = oneStreamData.map(_.outPSumStream)
  private def readOutAct(outIO: StreamBitsIO, debugIO: SRAMCommonDebugIO with InActSpecialDebugIO, doneIO: Bool,
                         theData: List[Int], idx: Int, theClock: Clock, adrOrData: Boolean
                        ): Unit = {
    var currentType: String = "Default"
    if (adrOrData) {
      currentType = "adrs"
    } else {
      currentType = "data"
    }
    println(s"------------- $idx-th $currentType read cycle ------------")
    outIO.data.valid.expect(false.B, s"$currentType should not valid now")
    if (printLogDetails) println(
      s"-------- $idx.0 $currentType done = ${doneIO.peek()}\n" +
        s"-------- $idx.0 $currentType valid= ${outIO.data.valid.peek()}\n" +
        s"-------- $idx.0 $currentType crDt = ${debugIO.currentData.peek()}\n" +
        s"-------- $idx.0 $currentType inInc= ${debugIO.indexInc.peek()}\n" +
        s"-------- $idx.0 $currentType waFR = ${debugIO.waitForRead.peek()}\n" +
        s"-------- $idx.0 $currentType doRd = ${debugIO.doReadWire.peek()}\n" +
        s"-------- $idx.0 $currentType data = ${outIO.data.bits.peek()}\n" +
        s"-------- $idx.0 $currentType idx  = ${debugIO.idx.peek()}"
    )
    debugIO.waitForRead.expect(false.B, "it should be false as this is the first read cycle")
    theClock.step()
    outIO.data.bits.expect(theData(idx).U, s"theData($idx) = ${theData(idx)}")
    outIO.data.valid.expect(true.B, s"$currentType should valid now")
    if (printLogDetails) println(
      s"-------- $idx.5 $currentType done = ${doneIO.peek()}\n" +
        s"-------- $idx.5 $currentType valid= ${outIO.data.valid.peek()}\n" +
        s"-------- $idx.5 $currentType crDt = ${debugIO.currentData.peek()}\n" +
        s"-------- $idx.5 $currentType inInc= ${debugIO.indexInc.peek()}\n" +
        s"-------- $idx.5 $currentType waFR = ${debugIO.waitForRead.peek()}\n" +
        s"-------- $idx.5 $currentType doRd = ${debugIO.doReadWire.peek()}\n" +
        s"-------- $idx.5 $currentType data = ${outIO.data.bits.peek()}\n" +
        s"-------- $idx.5 $currentType idx  = ${debugIO.idx.peek()}"
    )
    outIO.data.valid.expect(true.B, s"$currentType should valid now")
    debugIO.waitForRead.expect(true.B, "it should be true as this is the second read cycle")
    debugIO.doReadWire.expect(true.B, "should be true")
    theClock.step()
    if (theData(idx) != 0) {
      doneIO.expect(false.B, s"current data equals to ${theData(idx)}, does not equal to zero, " +
        s"should $currentType be unfinished now")
    }
    if (printLogDetails) {
      println(s"-------- ${idx + 1} $currentType done = ${doneIO.peek()}")
    }
    println(s"-------- ${idx + 1} $currentType PASS $idx")
  }
  private def readOutPSumData(OutIO: DecoupledIO[UInt], debugIO: SRAMCommonDebugIO, doneIO: Bool, adrIO: UInt,
                              theData: List[Int], startIndex: Int, theClock: Clock): Unit = {
    for (i <- 0 until pSumOneSPadNum) {
      println(s"--------------- $i-th read cycle -----------")
      if (printLogDetails) println(
        s"-------- data = ${theData(i)} \n" +
          s"-------- waFR = ${debugIO.waitForRead.peek()}\n" +
          s"-------- doRd = ${debugIO.doReadWire.peek()}"
      )
      debugIO.waitForRead.expect(false.B, "it should be false as this is the first read cycle")
      debugIO.doReadWire.expect(true.B, "should be true")
      adrIO.poke((i + startIndex).U)
      OutIO.ready.poke(true.B)
      OutIO.valid.expect(false.B, "it should not valid now")
      theClock.step()
      if (printLogDetails) println(
        s"-------- data = ${theData(i)} \n" +
          s"-------- waFR = ${debugIO.waitForRead.peek()}\n" +
          s"-------- doRd = ${debugIO.doReadWire.peek()}"
      )
      debugIO.waitForRead.expect(true.B, "it should be true as this is the second read cycle")
      debugIO.doReadWire.expect(true.B, "should be true")
      OutIO.bits.expect(theData(i).U, s"$i, theData should be ${theData(i)}")
      OutIO.valid.expect(true.B, "it should valid now")
      theClock.step()
      println(s"-------- pSum PASS $i")
    }
  }
  private def inActStateBeZero(theTopIO: GLBClusterIO): Unit = {
    theTopIO.debugIO.inActDebugIO.zipWithIndex.foreach({ case ( x, idx) =>
      if (printLogDetails) {
        println(s"-------- inActBankState$idx =  ${x.theState.peek()}")
        println(s"-------- adrZeroState$idx   =  ${x.adrDebug.commonDebug.zeroState.peek()}")
        println(s"-------- dataZeroState$idx  =  ${x.dataDebug.commonDebug.zeroState.peek()}")
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
        adrIO = theCtrlIO(index).adr, startIndex = startIndexes(index), theClock = theClock)
      println(s"-------- $index PSum finish")
    }
    // read begin
    (1 until pSumSRAMNum).foldLeft(fork(forkReadOutPSumHelper(0))) {
      case (left, right) => left.fork(forkReadOutPSumHelper(right))
    } .join()
  }
  private def forkReadAdrAndData(theOutIO: CSCStreamIO, theDebugIO: InActSRAMBankDebugIO,
                                 adrStream: List[Int], dataStream: List[Int], adrStrIdx: Int, dataStrIdx: Int,
                                 theClock: Clock): (Int, Int) = {
    var adrRdIdx = adrStrIdx
    var dataRdIdx = dataStrIdx
    fork {
      while (!theDebugIO.dataDebug.subDone.peek().litToBoolean) {
        theOutIO.dataIOs.data.ready.poke(true.B)
        readOutAct(theOutIO.dataIOs, theDebugIO.dataDebug.commonDebug,
          theDebugIO.dataDebug.subDone, dataStream, dataRdIdx,
          theClock, adrOrData = false
        )
        dataRdIdx = dataRdIdx + 1
      }
      println("------------ data read one stream -------------")
      theDebugIO.dataDebug.subDone.expect(true.B, s"current data equals to " +
        s"${if (dataRdIdx > 0) dataStream(dataRdIdx - 1)}, data should finish now")
      theOutIO.dataIOs.data.ready.poke(false.B)
    } .fork {
      while (!theDebugIO.adrDebug.subDone.peek().litToBoolean) {
        theOutIO.adrIOs.data.ready.poke(true.B)
        readOutAct(theOutIO.adrIOs, theDebugIO.adrDebug.commonDebug,
          theDebugIO.adrDebug.subDone, adrStream, adrRdIdx,
          theClock, adrOrData = true
        )
        adrRdIdx = adrRdIdx + 1
      }
      println("------------ adr read one stream --------------")
      theOutIO.adrIOs.data.ready.poke(false.B)
    } .join()
    println("---------- both read out one stream ------------")
    timescope {
      theClock.step()
      theDebugIO.theState.expect(0.U, "inActSRAM state should be idle after all read")
      if (printLogDetails) println("the state = " + theDebugIO.theState.peek())
    }
    (adrRdIdx, dataRdIdx)
  }
  private def topReadOutActAdrAndData(theTopIO: GLBClusterIO, adrStreams: Seq[List[Int]], dataStreams: Seq[List[Int]],
                                      theClock: Clock): Unit = {
    val theCtrlIO = theTopIO.ctrlPath.inActIO.map(x => x.readIO)
    val theInActDebugIOs = theTopIO.debugIO.inActDebugIO
    val theOutIOs = theTopIO.dataPath.inActIO.map(x => x.outIOs)
    var (adrRdIdx_0, dataRdIdx_0): (Int, Int) = (0, 0)
    var (adrRdIdx_1, dataRdIdx_1): (Int, Int) = (0, 0)
    var (adrRdIdx_2, dataRdIdx_2): (Int, Int) = (0, 0)
    for (i <- 0 until inActStreamNum) {
      println("------------- begin SRAMs read -------------")
      require(inActSRAMNum == 3, "you need to change the fork number if inActSRAMNum not equals to 3")
      fork {
        theClock.step(cycles = (new Random).nextInt(50) + (new Random).nextInt(50))
        println("------------- SRAM0 Begins -------------")
        theCtrlIO(0).enable.poke(true.B)
        theCtrlIO(0).adr.poke(i.U)
        theClock.step() // subInAct from idle to doing
        if (printLogDetails) println(s"-------- SRAM0 state = ${theInActDebugIOs(0).theState.peek()}")
        theInActDebugIOs(0).theState.expect(1.U, "the inAct0 State should be doing one cycle later")
        val (adrRdIdxTmp: Int, dataRdIdxTmp: Int) = forkReadAdrAndData(theOutIOs(0), theInActDebugIOs(0),
          adrStreams.head, dataStreams.head, adrRdIdx_0, dataRdIdx_0, theClock
        )
        adrRdIdx_0 = adrRdIdxTmp
        dataRdIdx_0 = dataRdIdxTmp
        println("------------- SRAM0 finish -------------")
        theTopIO.debugIO.oneInActSRAMDone.zipWithIndex.foreach({case (x, idx) =>
          if (printLogDetails) println(s"-------- oneSRAM${idx}Done = ${x.peek()}")
        })
        theClock.step()
        theCtrlIO(0).enable.poke(false.B) // one cycle after it done, when in idle, en should be disabled
      } .fork {
        theClock.step(cycles = (new Random).nextInt(50) + (new Random).nextInt(50))
        println("------------- SRAM1 Begins -------------")
        theCtrlIO(1).enable.poke(true.B)
        theCtrlIO(1).adr.poke(i.U)
        theClock.step() // subInAct from idle to doing
        if (printLogDetails) println(s"-------- SRAM1 state = ${theInActDebugIOs(1).theState.peek()}")
        theInActDebugIOs(1).theState.expect(1.U, "the inAct1 State should be doing one cycle later")
        val (adrRdIdxTmp: Int, dataRdIdxTmp: Int) = forkReadAdrAndData(
          theOutIO = theOutIOs(1), theDebugIO = theInActDebugIOs(1),
          adrStreams(1), dataStreams(1), adrRdIdx_1, dataRdIdx_1, theClock
        )
        adrRdIdx_1 = adrRdIdxTmp
        dataRdIdx_1 = dataRdIdxTmp
        println("------------- SRAM1 finish -------------")
        theTopIO.debugIO.oneInActSRAMDone.zipWithIndex.foreach({case (x, idx) =>
          if (printLogDetails) println(s"-------- oneSRAM${idx}Done = ${x.peek()}")
        })
        theClock.step()
        theCtrlIO(1).enable.poke(false.B) // one cycle after it done, when in idle, en should be disabled
      } .fork {
        theClock.step(cycles = (new Random).nextInt(50) + (new Random).nextInt(50))
        println("------------- SRAM2 Begins -------------")
        theCtrlIO(2).enable.poke(true.B)
        theCtrlIO(2).adr.poke(i.U)
        theClock.step() // subInAct from idle to doing
        if (printLogDetails) println(s"-------- SRAM2 state = ${theInActDebugIOs(2).theState.peek()}")
        theInActDebugIOs(2).theState.expect(1.U, "the inAct2 State should be doing one cycle later")
        val (adrRdIdxTmp: Int, dataRdIdxTmp: Int) = forkReadAdrAndData(theOutIOs(2), theInActDebugIOs(2),
          adrStreams(2), dataStreams(2), adrRdIdx_2, dataRdIdx_2, theClock
        )
        adrRdIdx_2 = adrRdIdxTmp
        dataRdIdx_2 = dataRdIdxTmp
        println("------------- SRAM2 finish -------------")
        theTopIO.debugIO.oneInActSRAMDone.zipWithIndex.foreach({case (x, idx) =>
          if (printLogDetails) println(s"-------- oneSRAM${idx}Done = ${x.peek()}")
        })
        theClock.step()
        theCtrlIO(2).enable.poke(false.B) // one cycle after it done, when in idle, en should be disabled
      } .join()
      println("------------- three SRAMs finish -------------")
      if (printLogDetails) {
        theTopIO.debugIO.oneInActSRAMDone.zipWithIndex.foreach({case (x, idx) =>
          println(s"-------- oneSRAM${idx}Done = ${x.peek()}")
        })
        theTopIO.debugIO.oneInActSRAMDone.zipWithIndex.foreach({case (x, idx) =>
          println(s"-------- oneSRAM${idx}Done = ${x.peek()}")
        })
      }
      theClock.step()
      println("------------- one cycle later -------------")
      if (printLogDetails) {
        theTopIO.debugIO.oneInActSRAMDone.zipWithIndex.foreach({case (x, idx) =>
          println(s"-------- oneSRAM${idx}Done = ${x.peek()}")
        })
        theTopIO.debugIO.oneInActSRAMDone.zipWithIndex.foreach({case (x, idx) =>
          println(s"-------- oneSRAM${idx}Done = ${x.peek()}")
        })
      }
      theClock.step()
      println("------------- one cycle later -------------")
      theTopIO.debugIO.oneInActSRAMDone.foreach(_.expect(false.B,"inActReg should be idle after doEn disabled"))
      if (printLogDetails) {
        theTopIO.debugIO.oneInActSRAMDone.zipWithIndex.foreach({case (x, idx) =>
          println(s"-------- oneSRAM${idx}Done = ${x.peek()}")
        })
        theTopIO.debugIO.oneInActSRAMDone.zipWithIndex.foreach({case (x, idx) =>
          println(s"-------- oneSRAM${idx}Done = ${x.peek()}")
        })
      }
      theClock.step(cycles = (new Random).nextInt(50) + 50)
      theTopIO.debugIO.oneInActSRAMDone.foreach(_.expect(false.B,"after several cycles, " +
        "every Vec Reg should be false as initial state"))
      inActStateBeZero(theTopIO)
    }
    println("------------- all streams read finish -------------")
  }
  private def readOutActAdrAndData(theOutIO: CSCStreamIO, theCtrlIO:MeMReWrIO, theDebugIO: InActSRAMBankDebugIO,
                                   adrStream: List[Int], dataStream: List[Int], theClock: Clock): Unit = {
    var (adrRdIdx, dataRdIdx): (Int, Int) = (0, 0)
    for (i <- 0 until inActStreamNum) {
      theCtrlIO.enable.poke(true.B)
      theCtrlIO.adr.poke(i.U)
      theClock.step() // from idle to doing
      val (adrRdIdxTmp: Int, dataRdIdxTmp: Int) = forkReadAdrAndData(theOutIO, theDebugIO,
        adrStream, dataStream, adrRdIdx, dataRdIdx, theClock)
      adrRdIdx = adrRdIdxTmp
      dataRdIdx = dataRdIdxTmp
      theCtrlIO.enable.poke(false.B)
      theClock.step(cycles = (new Random).nextInt(50) + 50)
    }
  }
  private def writeInPSumData(inIO: DecoupledIO[UInt], debugIO: SRAMCommonDebugIO, doneIO: Bool, theData: List[Int],
                              startIndex: Int, adrIO: UInt , theClock: Clock): Unit = {
    for (i <- 0 until pSumOneSPadNum) {
      println(s"--------------- $i-th PSum write cycle -----------")
      adrIO.poke((startIndex + i).U)
      inIO.bits.poke(theData(i).U)
      inIO.valid.poke(true.B)
      if (printLogDetails) {
        println(s"--------       data = ${theData(i)} \n" +
          s"--------      index = ${startIndex + i}")
      }
      inIO.ready.expect(true.B, s"$i, it should be ready now")
      theClock.step()
      println(s"-------- $i PASS")
    }
  }
  private def writeInAct(inIO: StreamBitsIO, debugIO: SRAMCommonDebugIO with InActSpecialDebugIO, doneIO: Bool, theData: List[Int],
                         theClock: Clock, adrOrData: Boolean) : Unit = {
    var currentType: String = "Default"
    if (adrOrData) {
      currentType = "adrs"
    } else {
      currentType = "data"
    }
    for (i <- theData.indices) {
      println(s"------------- $i-th $currentType write cycle ----------")
      inIO.data.bits.poke(theData(i).U)
      inIO.data.valid.poke(true.B)
      inIO.data.ready.expect(true.B, s"$i, $currentType should be ready now")
      if (printLogDetails) {
        println(s"-------- $currentType done     = ${doneIO.peek()}\n" +
          s"-------- $currentType data     = ${theData(i)}\n" +
          s"-------- $currentType index    = ${debugIO.idx.peek()}\n" +
          s"-------- $currentType doWrite  = ${debugIO.doWriteWire.peek()}\n" +
          s"-------- $currentType lookupIdx= ${debugIO.lookupIdx.peek()}\n" +
          s"-------- $currentType nextValid= ${debugIO.doReadWire.peek()}")
      }
      theClock.step()
      if (printLogDetails) println(s"-------- $currentType done     = ${doneIO.peek()}")
      doneIO.expect((i == theData.length - 1).B, s"$currentType Data($i) = ${theData(i)}, " +
        s"should $currentType finish?")
      println(s"-------- $currentType PASS $i")
    }
  }
  private def writeInActAdrAndData(theInIO: CSCStreamIO, theDebugIO: InActSRAMBankDebugIO, adrStream: List[Int],
                                   dataStream: List[Int], theClock: Clock): Unit = {
    fork {
      writeInAct(theInIO.dataIOs, theDebugIO.dataDebug.commonDebug,
        theDebugIO.dataDebug.subDone, dataStream, theClock, adrOrData = false
      )
      println("-------------- data write finish ---------------")
    } .fork {
      writeInAct(theInIO.adrIOs, theDebugIO.adrDebug.commonDebug,
        theDebugIO.adrDebug.subDone, adrStream, theClock, adrOrData = true
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
      val theData = pSumStream.head.flatten
      val startIndex = (new Random).nextInt(maxPSumStreamNum - 1) * pSumOneSPadNum
      println("---------------- test begin ----------------")
      println("---------- Partial Sum SRAM Bank -----------")
      println("---------- test basic functions ------------")
      println(s"-------- startIndex = $startIndex")
      thePSumBank.reset.poke(true.B)
      theClock.step()
      thePSumBank.reset.poke(false.B)
      println("-------------- begin to write --------------")
      theTopIO.ctrlPath.writeIO.enable.poke(true.B)
      writeInPSumData(inIO = theTopIO.dataPath.inIOs, debugIO = theTopIO.debugIO, doneIO = theTopIO.ctrlPath.writeIO.done,
        theData = theData, startIndex = startIndex, adrIO = theTopIO.ctrlPath.writeIO.adr, theClock = theClock)
      println("---------------- write finish --------------")
      theTopIO.ctrlPath.writeIO.enable.poke(false.B)
      theClock.step(cycles = (new Random).nextInt(5) + 1)
      println("---------------- begin to read -------------")
      theTopIO.ctrlPath.readIO.enable.poke(true.B)
      readOutPSumData(OutIO = theTopIO.dataPath.outIOs, debugIO = theTopIO.debugIO, doneIO = theTopIO.ctrlPath.readIO.done,
        adrIO = theTopIO.ctrlPath.readIO.adr , theData = theData, startIndex = startIndex, theClock = theClock
      )
      println("---------------- read finish ---------------")
      println("---------------- test finish ---------------")
    }
  }

  it should "work well on InActSRAMCommon" in {
      test(new InActSRAMCommon(inActAdrSRAMSize, inActAdrWidth, true)) { theAdrSRAM =>
        val theTopIO = theAdrSRAM.io
        val theClock = theAdrSRAM.clock
        val InActAdrStream = inActAdrStream.head.flatten.toList
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
          InActAdrStream, theClock, adrOrData = true)
        theTopIO.ctrlPath.readIO.enable.poke(false.B)
        println("--------------- write finish -----------------")
        theTopIO.ctrlPath.writeIO.enable.poke(false.B)
        println(s"inInc = ${theTopIO.debugIO.indexInc.peek()}")
        theClock.step(cycles = (new Random).nextInt(5) + 5)
        println("--------------- begin to read ----------------")
        println(s"inInc = ${theTopIO.debugIO.indexInc.peek()}")
        // begin to read out data
        var theRdIdx = 0
        for (i <- 0 until inActStreamNum) {
          while (!theTopIO.ctrlPath.readIO.done.peek().litToBoolean) {
            theTopIO.ctrlPath.readIO.enable.poke(true.B)
            theTopIO.ctrlPath.readIO.adr.poke(i.U)
            theTopIO.dataPath.outIOs.data.ready.poke(true.B)
            readOutAct(theTopIO.dataPath.outIOs, theTopIO.debugIO, theTopIO.ctrlPath.readIO.done,
              InActAdrStream, theRdIdx, theClock, adrOrData = true)
            theRdIdx = theRdIdx + 1
          }
          theTopIO.ctrlPath.readIO.enable.poke(false.B)
          theTopIO.dataPath.outIOs.data.ready.poke(false.B)
          println("------------ read out one stream -------------")
          theClock.step(cycles = (new Random).nextInt(50) + 50)
        }
        // read out finish
        println("---------------- read finish -----------------")
        println(s"-------- streamNum = $inActStreamNum")
        println("---------------- test finish -----------------")
      }
  }

  it should "work well on InActSRAMBank" in {
    test(new InActSRAMBank(true)) { theInAct =>
      val theTopIO = theInAct.io
      val theClock = theInAct.clock
      val InActAdrStream = inActAdrStream.head.flatten.toList
      val InActDataStream = inActDataStream.head.flatten.toList
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
      println(s"-------- theState = ${theTopIO.debugIO.theState.peek()}")
      println("------------- all write finish ---------------")
      theTopIO.ctrlPath.writeIO.enable.poke(false.B)
      theClock.step(cycles = (new Random).nextInt(5) + 1)
      println("--------------- begin to read ----------------")
      // begin to read out streams into data sram and address sram
      readOutActAdrAndData(theTopIO.dataPath.outIOs, theTopIO.ctrlPath.readIO, theTopIO.debugIO,
        InActAdrStream, InActDataStream, theClock)
      theClock.step()
      theTopIO.debugIO.theState.expect(0.U, "after all read, the state should be idle now")
      println(s"-------- theState = ${theTopIO.debugIO.theState.peek()}")
      println("-------------- all read finish ---------------")
      println(s"-------- streamNum = $inActStreamNum")
      println("---------------- test finish -----------------")
    }
  }

  it should "work well on GLB Cluster" in {
    test (new GLBCluster(true)) { theGLB =>
      val theTopIO = theGLB.io
      val theClock = theGLB.clock
      val theInActCtrl = theTopIO.ctrlPath.inActIO
      val thePSumCtrl = theTopIO.ctrlPath.pSumIO
      val theInActAdrStreams = inActAdrStream.take(inActRouterNum).map(_.flatten.toList)
      val theInActDataStreams = inActDataStream.take(inActRouterNum).map(_.flatten.toList)
      val thePSumDataStreams = pSumStream.take(pSumRouterNum).map(_.flatten)
      val gnmfcs1Stream = Seq.fill(6){(new Random).nextInt(6) + 1}
      val pSumStartIdx = gnmfcs1Stream.head*N2*M2*F2 + gnmfcs1Stream(1)*M2*F2 + gnmfcs1Stream(2)*F2 + gnmfcs1Stream(3) // FIXME
      require(pSumStartIdx + pSumOneSPadNum < pSumSRAMSize, "pSum's start index plus oneSPad size should less than pSumSRAMSize")
      def forkWriteInPSumHelper(index: Int, startIndex: Int): Unit = {
        thePSumCtrl(index).writeIO.enable.poke(true.B)
        writeInPSumData(inIO = theTopIO.dataPath.pSumIO(index).inIOs, debugIO = theTopIO.debugIO.pSumDebugIO(index),
          doneIO = theTopIO.debugIO.onePSumSRAMDone(index), theData = thePSumDataStreams(index),
          startIndex = startIndex, adrIO = thePSumCtrl(index).writeIO.adr, theClock = theClock)
        println(s"-------- $index PSum finish")
        thePSumCtrl(index).writeIO.enable.poke(false.B) // false the en after receiving done signal
      }
      def forkWriteInInActHelper(index: Int): Unit = {
        theInActCtrl(index).writeIO.enable.poke(true.B)
        theClock.step() // sub from idle to doing;
        theTopIO.debugIO.inActDebugIO(index).theState.expect(1.U, s"the inActSRAM $index should doing now")
        println(s"-------- inActSRAMState$index =  ${theTopIO.debugIO.inActDebugIO(index).theState.peek()}")
        writeInActAdrAndData(theTopIO.dataPath.inActIO(index).inIOs, theTopIO.debugIO.inActDebugIO(index),
          theInActAdrStreams(index), theInActDataStreams(index), theClock)
        println(s"-------- $index InAct finish")
        theInActCtrl(index).writeIO.done.expect(true.B, "after all inActSRAMs done, top inAct should done now")
        theClock.step()
        println(s"-------- $index InActSRAM State = ${theTopIO.debugIO.inActDebugIO(index).theState.peek()}")
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
        inActStateBeZero(theTopIO)
        theClock.step()
        println("----------------- one cycle later ---------------")
        inActStateBeZero(theTopIO)
        theClock.step(cycles = (new Random).nextInt(5) + 1)
        println("----------- several cycles later ----------")
        inActStateBeZero(theTopIO)
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
        if (printLogDetails) theTopIO.debugIO.oneInActSRAMDone.foreach(x => println(s"-------- oneSRAMDone = ${x.peek()}"))
        println("------------ all inAct read finish -------------")
        println(s"-------- InActStreamNum = $inActStreamNum")
      } .join()
      println("---------------- test finish -----------------")
    }
  }

  behavior of "test the spec of PE Cluster"
  it should "work well on PE Cluster" in {
    test (new PECluster(true)) { thePECluster =>
      val theTopIO = thePECluster.io
      val theClock = thePECluster.clock
      val pSumDataIO = theTopIO.dataPath.pSumIO
      pSumDataIO.inIOs.foreach(_.setSourceClock(theClock))
      def forkPSumHelper(idx: Int): Unit = {
        theClock.step((new Random).nextInt(10) + 1)
        pSumDataIO.inIOs(idx).enqueueSeq(addendRand(idx).map(x => x.U))
        println(s"-------- $idx-th Column PEs receive all inPSum")
      }
      println("----------------- test begin -----------------")
      println("------------ PE Cluster Top Spec -------------")
      println("----------- test basic functions -------------")
      thePECluster.reset.poke(true.B)
      theClock.step()
      thePECluster.reset.poke(false.B)
      theClock.step()
      theTopIO.ctrlPath.pSumCtrlSel.inDataSel.poke(true.B) // receive data from PSum Router
      theTopIO.ctrlPath.inActCtrlSel.inDataSel.poke(false.B) // not broad-cast
      theTopIO.ctrlPath.doEn.poke(true.B) // begin to load inAct and weight, then cal
      // after finish
      theTopIO.ctrlPath.pSumLoadEn.poke(true.B) // begin to accumulate PSum
      (1 until peColNum).foldLeft(fork(forkPSumHelper(0))) {
        case (left, right) => left.fork(forkPSumHelper(idx = right))
      }.join()
    }
  }
  behavior of "test the spec of Router Cluster"
  it should "work well on Router Cluster" in {
    test (new RouterCluster(true)) { theRCluster =>
      val theTop = theRCluster.io
      val theClock = theRCluster.clock
      theTop.dataPath.routerData.iRIO.head.inIOs.foreach(x =>
        require(DataMirror.directionOf(x.dataIOs.data.bits) == Direction.Input))
      theTop.dataPath.routerData.iRIO.head.outIOs.foreach(x =>
        require(DataMirror.directionOf(x.dataIOs.data.bits) == Direction.Output))
      theRCluster.reset.poke(true.B)
      theClock.step()
      theRCluster.reset.poke(false.B)
    }
  }
  behavior of "test the spec of Cluster Group"
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

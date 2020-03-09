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

class ClusterSpecTest extends FlatSpec with ChiselScalatestTester with Matchers
  with ClusterSRAMConfig with MCRENFConfig with SPadSizeConfig with GNMFCS2Config {
  private val oneSPadPSum: Int = M0*E*N0*F0 // when read counts this, then stop
  private val printLogDetails = true // true to print more detailed logs
  private val inActStreamNum: Int = G2*N2*F2*C2*S2
  private val weightStreamNum: Int = G2*M2*C2*S2
  private val maxPSumStreamNum: Int = pSumSRAMSize/oneSPadPSum
  private val addendRand = Seq.fill(peColNum, oneSPadPSum){(new Random).nextInt(10)}
  private val oneStreamData = new GenOneStreamData
  private val inActAdrStream = oneStreamData.inActAdrStream
  private val inActDataStream = oneStreamData.inActDataStream
  private val inActCountStream = oneStreamData.inActCountStream
  private val weightAdrStream = oneStreamData.weightAdrStream
  private val weightDataStream = oneStreamData.weightDataStream
  private val weightCountStream = oneStreamData.weightCountStream
  private val pSumStream = oneStreamData.outPSumStream
  private def readOutAct(outIO: StreamBitsIO, debugIO: SRAMCommonDebugIO, doneIO: Bool,
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
    debugIO.idxInc.expect(false.B, s"$idx, index should not increase now")
    if (printLogDetails) println(
      s"-------- $idx.0 $currentType done = ${doneIO.peek()}\n" +
        s"-------- $idx.5 $currentType valid= ${outIO.data.valid.peek()}\n" +
        s"-------- $idx.5 $currentType doEn = ${debugIO.idxCount.peek()}\n" +
      s"-------- $idx.0 $currentType crDt = ${debugIO.currentData.peek()}\n" +
      s"-------- $idx.0 $currentType waFR = ${debugIO.waitForRead.peek()}\n" +
      s"-------- $idx.0 $currentType doRd = ${debugIO.doReadWire.peek()}\n" +
      s"-------- $idx.0 $currentType idIc = ${debugIO.idxInc.peek()}"
    )
    theClock.step()
    debugIO.idxInc.expect(true.B, s"$idx, $currentType index should increase now")
    outIO.data.bits.expect(theData(idx).U, s"theData($idx) = ${theData(idx)}")
    outIO.data.valid.expect(true.B, s"$currentType should valid now")
    if (printLogDetails) println(
      s"-------- $idx.5 $currentType done = ${doneIO.peek()}\n" +
      s"-------- $idx.5 $currentType valid= ${outIO.data.valid.peek()}\n" +
      s"-------- $idx.5 $currentType doEn = ${debugIO.idxCount.peek()}\n" +
      s"-------- $idx.5 $currentType crDt = ${debugIO.currentData.peek()}\n" +
      s"-------- $idx.5 $currentType waFR = ${debugIO.waitForRead.peek()}\n" +
      s"-------- $idx.5 $currentType doRd = ${debugIO.doReadWire.peek()}\n" +
      s"-------- $idx.5 $currentType idIc = ${debugIO.idxInc.peek()}\n" +
      s"-------- $idx.5 $currentType data = ${outIO.data.bits.peek()}\n" +
      s"-------- $idx.5 $currentType idx  = ${debugIO.idx.peek()}"
    )
    outIO.data.valid.expect(true.B, s"$currentType should valid now")
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
  private def readOutPSumData(OutIO: DecoupledIO[UInt], debugIO: SRAMCommonDebugIO, doneIO: Bool, theData: List[Int],
                      startIndex: Int, theClock: Clock): Unit = {
    for (i <- 0 until oneSPadPSum) {
      println(s"--------------- $i-th read cycle -----------")
      if (printLogDetails) {
        println(s"-------- data       = ${theData(i)} \n" +
          s"-------- index      = ${debugIO.idx.peek()} \n" +
          s"-------- whetherInc = ${debugIO.idxInc.peek()}")
      }
      OutIO.ready.poke(true.B)
      debugIO.idx.expect((i + startIndex).U, s"the read index should be $i + $startIndex")
      debugIO.idxInc.expect(false.B, s"$i, index should not increase now")
      OutIO.valid.expect(false.B, "it should not valid now")
      theClock.step()
      if (printLogDetails) {
        println(s"-------- data       = ${theData(i)} \n" +
          s"-------- index      = ${debugIO.idx.peek()} \n" +
          s"-------- whetherInc = ${debugIO.idxInc.peek()}")
      }
      debugIO.idx.expect((i + startIndex).U, s"the read index should be $i + $startIndex")
      debugIO.idxInc.expect(true.B, s"index should increase")
      OutIO.bits.expect(theData(i).U, s"$i, theData should be ${theData(i)}")
      OutIO.valid.expect(true.B, "it should valid now")
      theClock.step()
      doneIO.expect((i == oneSPadPSum - 1).B)
      println(s"-------- pSum PASS $i")
    }
  }
  private def inActStateBeZero(theTopIO: GLBClusterIO): Unit = {
    theTopIO.debugIO.inActDebugIO.foreach({ x =>
      if (printLogDetails) {
        println(s"-------- inActBankState =  ${x.theState.peek()}")
        println(s"-------- adrZeroState   =  ${x.adrDebug.commonDebug.theState.peek()}")
        println(s"-------- dataZeroState  =  ${x.dataDebug.commonDebug.theState.peek()}")
      }
      x.theState.expect(0.U, "every inActSRAMBank still needs to be idle")
      x.adrDebug.commonDebug.theState.expect(0.U, "every address SRAM still needs to be none zero")
      x.dataDebug.commonDebug.theState.expect(0.U, "every data SRAM still needs to be none zero")
    })
  }
  private def pSumStateBeZero(theTopIO: GLBClusterIO): Unit = {
    theTopIO.debugIO.pSumDebugIO.foreach({ x =>
      if (printLogDetails) {
        println(s"-------- inActBankState =  ${x.theState.peek()}")
      }
      x.theState.expect(0.U, "every pSumSRAMBank still needs to be idle")
    })
  }
  private def topReadPSum(theTopIO: GLBClusterIO, dataStream: Seq[List[Int]], startIndexes: Seq[Int], theClock: Clock): Unit = {
    val theCtrlIO = theTopIO.ctrlPath.pSumIO
    val thePSumDebugIOs = theTopIO.debugIO.pSumDebugIO
    val theOutIOs = theTopIO.dataPath.pSumIO.map(x => x.outIOs)
    def forkReadOutPSumHelper(index: Int): Unit = {
      readOutPSumData(OutIO = theOutIOs(index), debugIO = thePSumDebugIOs(index),
        doneIO = theTopIO.debugIO.onePSumSRAMDone(index), theData = dataStream(index),
        startIndex = startIndexes(index), theClock = theClock)
      println(s"-------- $index PSum finish")
      println(s"-------- $index PSumSRAM State = ${theTopIO.debugIO.pSumDebugIO(index).theState.peek()}")
      timescope{
        theClock.step()
        theTopIO.debugIO.pSumDebugIO(index).theState.expect(0.U, s"pSumSRAMBank $index should be idle one cycle later")
      }
    }
    // read begin
    thePSumDebugIOs.zip(startIndexes).foreach({ case (strIO, str) =>
      strIO.idx.expect(str.U, "the start index should be start index")
    })
    theCtrlIO.doEn.poke(true.B)
    theCtrlIO.writeOrRead.poke(false.B)
    theClock.step() // the topPSum from idle to oneSRAMDoing
    if (printLogDetails) println(s"-------- topPSumState  = ${theTopIO.debugIO.theState(1).peek()} ")
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
        s"${dataStream(dataRdIdx - 1)}, data should finish now")
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
      println("the state = " + theDebugIO.theState.peek())
    }
    (adrRdIdx, dataRdIdx)
  }
  private def topReadOutActAdrAndData(theTopIO: GLBClusterIO, adrStreams: Seq[List[Int]], dataStreams: Seq[List[Int]],
                                      theClock: Clock): Unit = {
    val theCtrlIO = theTopIO.ctrlPath.inActIO
    val theInActDebugIOs = theTopIO.debugIO.inActDebugIO
    val theOutIOs = theTopIO.dataPath.inActIO.map(x => x.outIOs)
    var (adrRdIdx_0, dataRdIdx_0): (Int, Int) = (0, 0)
    var (adrRdIdx_1, dataRdIdx_1): (Int, Int) = (0, 0)
    var (adrRdIdx_2, dataRdIdx_2): (Int, Int) = (0, 0)
    for (_ <- 0 until inActStreamNum) {
      println("------------- begin SRAMs read -------------")
      theCtrlIO.doEn.poke(true.B)
      theCtrlIO.writeOrRead.poke(false.B)
      theClock.step() // topInAct from idle/wait to doing
      if (printLogDetails) println(s"-------- topInActState  = ${theTopIO.debugIO.theState(0).peek()} ")
      theClock.step() // subInAct from idle to doing
      if (printLogDetails) println(s"-------- topInActState  = ${theTopIO.debugIO.theState(0).peek()} ")
      require(inActSRAMNum == 3, "you need to change the fork number if inActSRAMNum not equals to 3")
      fork {
        val (adrRdIdxTmp: Int, dataRdIdxTmp: Int) = forkReadAdrAndData(theOutIOs(0), theInActDebugIOs(0),
          adrStreams.head, dataStreams.head, adrRdIdx_0, dataRdIdx_0, theClock
        )
        adrRdIdx_0 = adrRdIdxTmp
        dataRdIdx_0 = dataRdIdxTmp
        println("------------- SRAM0 finish -------------")
        timescope {
          theClock.step()
          theInActDebugIOs(0).theState.expect(0.U, "the SRAMBank 0 should be idle after one clock")
          theTopIO.debugIO.oneInActSRAMDone(0).expect(true.B, "the Vec Reg SRAMBank 0 should be done after one clock")
        }
      } .fork {
        val (adrRdIdxTmp: Int, dataRdIdxTmp: Int) = forkReadAdrAndData(
          theOutIO = theOutIOs(1), theDebugIO = theInActDebugIOs(1),
          adrStreams(1), dataStreams(1), adrRdIdx_1, dataRdIdx_1, theClock
        )
        adrRdIdx_1 = adrRdIdxTmp
        dataRdIdx_1 = dataRdIdxTmp
        println("------------- SRAM1 finish -------------")
        timescope {
          theClock.step()
          theInActDebugIOs(1).theState.expect(0.U, "the SRAMBank 1 should be idle after one clock")
          theTopIO.debugIO.oneInActSRAMDone(1).expect(true.B, "the Vec Reg SRAMBank 1 should be done after one clock")
        }
      } .fork {
        val (adrRdIdxTmp: Int, dataRdIdxTmp: Int) = forkReadAdrAndData(theOutIOs(2), theInActDebugIOs(2),
          adrStreams(2), dataStreams(2), adrRdIdx_2, dataRdIdx_2, theClock
        )
        adrRdIdx_2 = adrRdIdxTmp
        dataRdIdx_2 = dataRdIdxTmp
        println("------------- SRAM2 finish -------------")
        timescope {
          theClock.step()
          theInActDebugIOs(2).theState.expect(0.U, "the SRAMBank 2 should be idle after one clock")
          theTopIO.debugIO.oneInActSRAMDone(2).expect(true.B, "the Vec Reg SRAMBank 2 should be done after one clock")
        }
      } .join()
      println("------------- three SRAMs finish -------------")
      if (printLogDetails) theTopIO.debugIO.oneInActSRAMDone.foreach(x => println(s"-------- oneSRAMDone = ${x.peek()}"))
      theTopIO.debugIO.oneInActSRAMDone.foreach(_.expect(true.B,"every inAct Vec Reg should be true after read"))
      theCtrlIO.done.expect(true.B)
      theClock.step()
      theCtrlIO.doEn.poke(false.B)
      theClock.step(cycles = (new Random).nextInt(50) + 50)
      theTopIO.debugIO.theState(0).expect(0.U, "the topInActState should be idle when done one stream")
      theTopIO.debugIO.oneInActSRAMDone.foreach(_.expect(false.B,"after several cycles, " +
        "every Vec Reg should be false as initial state"))
      if (printLogDetails) println(s"-------- topInActState  = ${theTopIO.debugIO.theState(0).peek()} ")
      inActStateBeZero(theTopIO)
    }
    println("------------- all streams read finish -------------")
  }
  private def readOutActAdrAndData(theOutIO: CSCStreamIO, theCtrlIO:SRAMCommonCtrlIO, theDebugIO: InActSRAMBankDebugIO,
                                   adrStream: List[Int], dataStream: List[Int], theClock: Clock): Unit = {
    var (adrRdIdx, dataRdIdx): (Int, Int) = (0, 0)
    for (_ <- 0 until inActStreamNum) {
      theCtrlIO.doEn.poke(true.B)
      theCtrlIO.writeOrRead.poke(false.B) // begin to read
      theClock.step() // from idle to doing
      val (adrRdIdxTmp: Int, dataRdIdxTmp: Int) = forkReadAdrAndData(theOutIO, theDebugIO,
        adrStream, dataStream, adrRdIdx, dataRdIdx, theClock)
      adrRdIdx = adrRdIdxTmp
      dataRdIdx = dataRdIdxTmp
      theCtrlIO.doEn.poke(false.B)
      theClock.step(cycles = (new Random).nextInt(50) + 50)
    }
  }
  private def writeInPSumData(inIO: DecoupledIO[UInt], debugIO: SRAMCommonDebugIO, doneIO: Bool, theData: List[Int],
                              startIndex: Int, theClock: Clock): Unit = {
    for (i <- 0 until oneSPadPSum) {
      println(s"--------------- $i-th PSum write cycle -----------")
      inIO.bits.poke(theData(i).U)
      inIO.valid.poke(true.B)
      debugIO.idx.expect((startIndex + i).U, s"startIdx = $startIndex")
      debugIO.idxInc.expect(true.B, s"$i, index should increase")
      if (printLogDetails) {
        println(s"--------       data = ${theData(i)} \n" +
          s"--------      index = ${debugIO.idx.peek()} \n" +
          s"-------- whetherInc = ${debugIO.idxInc.peek()}")
      }
      inIO.ready.expect(true.B, s"$i, it should be ready now")
      theClock.step()
      doneIO.expect((i == oneSPadPSum - 1).B, s"i = $i, write should finish?")
      println(s"-------- $i PASS")
    }
  }
  private def writeInAct(inIO: StreamBitsIO, debugIO: SRAMCommonDebugIO, doneIO: Bool, theData: List[Int],
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
        println(s"-------- $currentType done     = ${doneIO.peek()}\n-------- $currentType data     = ${theData(i)}\n" +
          s"-------- $currentType index    = ${debugIO.idx.peek()}\n-------- $currentType zeroState = ${debugIO.theState.peek()}")
      }
      theClock.step()
      doneIO.expect((i == theData.length - 1).B, s"$currentType Data($i) = ${theData(i)}, " +
        s"$currentType zeroState = ${debugIO.theState.peek()},should $currentType finish?")
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
      val theData = pSumStream.flatten
      val startIndex = (new Random).nextInt(maxPSumStreamNum - 1) * oneSPadPSum
      println("---------------- test begin ----------------")
      println("---------- Partial Sum SRAM Bank -----------")
      println("---------- test basic functions ------------")
      println(s"-------- startIndex = $startIndex")
      thePSumBank.reset.poke(true.B)
      theClock.step()
      thePSumBank.reset.poke(false.B)
      println("-------------- begin to write --------------")
      theTopIO.ctrlPath.writeOrRead.poke(true.B)
      theTopIO.ctrlPath.doEn.poke(true.B)
      theTopIO.ctrlPath.startIdx.poke(startIndex.U)
      writeInPSumData(inIO = theTopIO.dataPath.inIOs, debugIO = theTopIO.debugIO, doneIO = theTopIO.ctrlPath.done,
        theData = theData, startIndex = startIndex, theClock = theClock)
      println("---------------- write finish --------------")
      theTopIO.ctrlPath.doEn.poke(false.B)
      theClock.step(cycles = (new Random).nextInt(5) + 1)
      println("---------------- begin to read -------------")
      theTopIO.ctrlPath.writeOrRead.poke(false.B)
      theTopIO.ctrlPath.doEn.poke(true.B)
      theTopIO.ctrlPath.startIdx.poke(startIndex.U)
      readOutPSumData(OutIO = theTopIO.dataPath.outIOs, debugIO = theTopIO.debugIO, doneIO = theTopIO.ctrlPath.done,
        theData = theData, startIndex = startIndex, theClock = theClock
      )
      println("---------------- read finish ---------------")
      println("---------------- test finish ---------------")
    }
  }

  it should "work well on InActSRAMCommon" in {
      test(new InActSRAMCommon(inActAdrSRAMSize, inActAdrWidth, true)) { theAdrSRAM =>
        val theTopIO = theAdrSRAM.io
        val theClock = theAdrSRAM.clock
        val InActAdrStream = inActAdrStream.flatten.toList
        println("----------------- test begin -----------------")
        println("----------- InputActAdr SRAM Bank ------------")
        println("----------- test basic functions -------------")
        theAdrSRAM.reset.poke(true.B)
        theClock.step()
        theAdrSRAM.reset.poke(false.B)
        println("--------------- begin to write ---------------")
        theTopIO.ctrlPath.writeOrRead.poke(true.B)
        theTopIO.ctrlPath.doEn.poke(true.B)
        // begin to write in data
        writeInAct(theTopIO.dataPath.inIOs, theTopIO.debugIO, theTopIO.ctrlPath.done,
          InActAdrStream, theClock, adrOrData = true)
        println("--------------- write finish -----------------")
        theTopIO.ctrlPath.doEn.poke(false.B)
        theClock.step(cycles = (new Random).nextInt(5) + 1)
        println("--------------- begin to read ----------------")
        // begin to read out data
        var theRdIdx = 0
        for (_ <- 0 until inActStreamNum) {
          theTopIO.ctrlPath.writeOrRead.poke(false.B)
          while (!theTopIO.ctrlPath.done.peek().litToBoolean) {
            theTopIO.ctrlPath.doEn.poke(true.B)
            theTopIO.dataPath.outIOs.data.ready.poke(true.B)
            readOutAct(theTopIO.dataPath.outIOs, theTopIO.debugIO, theTopIO.ctrlPath.done,
              InActAdrStream, theRdIdx, theClock, adrOrData = true)
            theRdIdx = theRdIdx + 1
          }
          theTopIO.ctrlPath.doEn.poke(false.B)
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
      val InActAdrStream = inActAdrStream.flatten.toList
      val InActDataStream = inActDataStream.flatten.toList
      println("----- inActReadCycle = " + InActAdrStream.length)
      println("----------------- test begin -----------------")
      println(s"--------  theInActStreamNum = $inActStreamNum")
      println("----------- InputActAdr SRAM Bank ------------")
      println("----------- test basic functions -------------")
      theInAct.reset.poke(true.B)
      theClock.step()
      theInAct.reset.poke(false.B)
      println("--------------- begin to write ---------------")
      theTopIO.ctrlPath.writeOrRead.poke(true.B)
      theTopIO.ctrlPath.doEn.poke(true.B)
      theClock.step() // from idle to doing
      // begin to write streams into data sram and address sram
      writeInActAdrAndData(theTopIO.dataPath.inIOs, theTopIO.debugIO, InActAdrStream, InActDataStream, theClock)
      // write finish
      theClock.step()
      theTopIO.debugIO.theState.expect(0.U, "after all write, the state should be idle now")
      println(s"-------- theState = ${theTopIO.debugIO.theState.peek()}")
      println("------------- all write finish ---------------")
      theTopIO.ctrlPath.doEn.poke(false.B)
      theClock.step(cycles = (new Random).nextInt(5) + 1)
      println("--------------- begin to read ----------------")
      // begin to read out streams into data sram and address sram
      readOutActAdrAndData(theTopIO.dataPath.outIOs, theTopIO.ctrlPath, theTopIO.debugIO,
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
      //val gnmfcs1IOs = theTopIO.ctrlPath.configIOs
      val theInActAdrStreams = Seq.fill(inActSRAMNum){inActAdrStream.flatten.toList}
      val theInActDataStreams = Seq.fill(inActSRAMNum){inActDataStream.flatten.toList}
      val thePSumDataStreams = Seq.fill(pSumSRAMNum){pSumStream.flatten}
      val gnmfcs1Stream = Seq.fill(6){(new Random).nextInt(6) + 1}
      val pSumStartIdx = gnmfcs1Stream.head*N2*M2*F2 + gnmfcs1Stream(1)*M2*F2 + gnmfcs1Stream(2)*F2 + gnmfcs1Stream(3) // FIXME
      require(pSumStartIdx + oneSPadPSum < pSumSRAMSize, "pSum's start index plus oneSPad size should less than pSumSRAMSize")
      def forkWriteInPSumHelper(index: Int, startIndex: Int): Unit = {
        writeInPSumData(inIO = theTopIO.dataPath.pSumIO(index).inIOs, debugIO = theTopIO.debugIO.pSumDebugIO(index),
          doneIO = theTopIO.debugIO.onePSumSRAMDone(index), theData = thePSumDataStreams(index),
          startIndex = startIndex, theClock = theClock)
        println(s"-------- $index PSum finish")
        timescope{
          theClock.step()
          theTopIO.debugIO.pSumDebugIO(index).idxInc.expect(false.B, s"the $index-th pSum should not increase index now")
        }
      }
      def forkWriteInInActHelper(index: Int): Unit = {
        writeInActAdrAndData(theTopIO.dataPath.inActIO(index).inIOs, theTopIO.debugIO.inActDebugIO(index),
          theInActAdrStreams(index), theInActDataStreams(index), theClock)
        println(s"-------- $index InAct finish")
        println(s"-------- $index InActSRAM State = ${theTopIO.debugIO.inActDebugIO(index).theState.peek()}")
        timescope{
          theClock.step()
          theTopIO.debugIO.inActDebugIO(index).theState.expect(0.U, s"inActSRAMBank $index should be idle one cycle later")
        }
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
        thePSumCtrl.doEn.poke(true.B)
        thePSumCtrl.writeOrRead.poke(true.B)
        theTopIO.ctrlPath.pSumSRAMStrIdx.poke(pSumStartIdx.U)
        //gnmfcs1IOs.zip(gnmfcs1Stream).foreach({ case (io, cfg) => io.poke(cfg.U)})
        println("--------------- Enable PSum Now ---------------")
        theClock.step(2) // top from idle to doing, sub from idle to doing;
        theTopIO.debugIO.theState(1).expect(1.U, s"the PSumTopState should be oneSRAMDoing now")
        if (printLogDetails) println(s"-------- PSumTop State = ${theTopIO.debugIO.theState(1).peek()}")
        (1 until pSumSRAMNum).foldLeft(fork(forkWriteInPSumHelper(0, startIndex = pSumStartIdx))) {
          case (left, right) => left.fork(forkWriteInPSumHelper(right, startIndex = pSumStartIdx)) // FIXME: check the start index
        } .join()
        thePSumCtrl.done.expect(true.B, "after all pSumSRAMs done, top PSum should done now")
        println("---------- all pSumSRAMs are written ------------")
        println("----------- pSum write verification -------------")
        println(s"-------- topPSumState ${theTopIO.debugIO.theState(1).peek()} ")
        pSumStateBeZero(theTopIO)
        theClock.step()
        println("----------------- one cycle later ---------------")
        if (printLogDetails) {
          println(s"-------- topPSumState ${theTopIO.debugIO.theState(1).peek()} ")
        }
        pSumStateBeZero(theTopIO)
        theTopIO.debugIO.theState(1).expect(0.U, "after pSumSRAMs all written, the state should be idle now")
        thePSumCtrl.done.expect(true.B, "after all pSumSRAMs done, top PSum should done now")
        thePSumCtrl.doEn.poke(false.B) // false the en after receiving done signal
        theClock.step()
        println("----------------- one cycle later ---------------")
        if (printLogDetails) {
          println(s"-------- topPSumState ${theTopIO.debugIO.theState(1).peek()} ")
        }
        theTopIO.debugIO.theState(1).expect(0.U, "Without enable signal, the pSumTop state should be idle")
        pSumStateBeZero(theTopIO)
        theClock.step(cycles = (new Random).nextInt(5) + 1)
        println("-------------- several cycles later -------------")
        pSumStateBeZero(theTopIO)
        thePSumCtrl.done.expect(false.B, "after several cycles, " +
          "pSumTop done signal should be false as initial state")
        theTopIO.debugIO.theState(1).expect(0.U, "Without enable signal, the pSumTop state should be idle")
      } .fork {
        theClock.step(cycles = (new Random).nextInt(5) + 1)
        theInActCtrl.doEn.poke(true.B)
        theInActCtrl.writeOrRead.poke(true.B)
        println("--------------- Enable InAct Now ---------------")
        theClock.step(2) // top from idle to doing, sub from idle to doing;
        theTopIO.debugIO.theState(0).expect(1.U, s"the InActTopState should be oneSRAMDoing now")
        (1 until inActSRAMNum).foldLeft(fork(forkWriteInInActHelper(0))) {
          case (left, right) => left.fork(forkWriteInInActHelper(index = right))
        } .join()
        theInActCtrl.done.expect(true.B, "after all inActSRAMs done, top inAct should done now")
        println("---------- all inActSRAMs are written ----------")
        println(s"-------- topInActState ${theTopIO.debugIO.theState(0).peek()} ")
        theTopIO.debugIO.oneInActSRAMDone.foreach(_.expect(true.B,"should be true now to generate done signal"))
        inActStateBeZero(theTopIO)
        theClock.step()
        println("----------------- one cycle later ---------------")
        theTopIO.debugIO.oneInActSRAMDone.foreach(_.expect(true.B,"should be true now to generate done signal"))
        if (printLogDetails) {
          println(s"-------- topInActState ${theTopIO.debugIO.theState(0).peek()} ")
          println(s"-------- inActDone ${theInActCtrl.done.peek()}")
          theTopIO.debugIO.oneInActSRAMDone.foreach(x => println(s"-------- oneSRAMDone = ${x.peek()}"))
        }
        inActStateBeZero(theTopIO)
        theTopIO.debugIO.theState(0).expect(0.U, "after inActSRAMs all written, the state should be idle now")
        theInActCtrl.done.expect(true.B, "after all inActSRAMs done, top inAct should done now")
        theInActCtrl.doEn.poke(false.B)
        theTopIO.debugIO.oneInActSRAMDone.foreach(_.expect(true.B,"should be true now to generate done signal"))
        theClock.step()
        println("----------------- one cycle later ---------------")
        if (printLogDetails) {
          println(s"-------- topInActState ${theTopIO.debugIO.theState(0).peek()} ")
          println(s"-------- inActDone ${theInActCtrl.done.peek()}")
          theTopIO.debugIO.oneInActSRAMDone.foreach(x => println(s"-------- oneSRAMDone = ${x.peek()}"))
        }
        inActStateBeZero(theTopIO)
        theClock.step(cycles = (new Random).nextInt(5) + 1)
        println("----------- several cycles later ----------")
        inActStateBeZero(theTopIO)
        theInActCtrl.done.expect(false.B, "after several cycles, " +
          "inActTop done signal should be false as initial state")
        theTopIO.debugIO.theState(0).expect(0.U, "Without enable signal, the inActTop state should be idle")
        theTopIO.debugIO.oneInActSRAMDone.foreach(_.expect(false.B,"after several cycles, " +
          "every Vec Reg should be false as initial state"))
        if (printLogDetails) {
          println(s"-------- topInActState ${theTopIO.debugIO.theState(0).peek()} ")
          println(s"-------- allDone ${theInActCtrl.done.peek()}")
          theTopIO.debugIO.oneInActSRAMDone.foreach(x => println(s"-------- oneSRAMDone = ${x.peek()}"))
        }
      } .join()
      // write test finish
      println("--------------- begin to read ----------------")
      fork {
        theClock.step(cycles = (new Random).nextInt(5) + 1)
        theTopIO.debugIO.pSumDebugIO.foreach( _.idx.expect(0.U, "when pSum begins to read, the index should be zero"))
        theClock.step()
        theTopIO.ctrlPath.pSumSRAMStrIdx.poke(pSumStartIdx.U)
        //gnmfcs1IOs.zip(gnmfcs1Stream).foreach({ case (io, cfg) => io.poke(cfg.U)})
        topReadPSum(theTopIO = theTopIO, dataStream = thePSumDataStreams,
          startIndexes = Seq.fill(pSumSRAMNum){pSumStartIdx}, theClock = theClock)
        theClock.step()
        theTopIO.debugIO.theState(1).expect(0.U, "after all read, the PSumState should be idle now")
        println("------------ all pSum read finish --------------")
      } .fork {
        theClock.step(cycles = (new Random).nextInt(5) + 1)
        // begin to read out streams into data sram and address sram
        topReadOutActAdrAndData(theTopIO, theInActAdrStreams, theInActDataStreams, theClock)
        theClock.step()
        if (printLogDetails) theTopIO.debugIO.oneInActSRAMDone.foreach(x => println(s"-------- oneSRAMDone = ${x.peek()}"))
        theTopIO.debugIO.theState(0).expect(0.U, "after all read, the inActState should be idle now")
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

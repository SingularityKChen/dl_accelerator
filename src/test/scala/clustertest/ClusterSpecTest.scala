package dla.test.clustertest

import chisel3._
import chisel3.tester._
import chiseltest.internal.{AbstractTesterThread, TesterThreadList}
import dla.cluster._
import dla.pe.{CSCStreamIO, MCRENFConfig, SPadSizeConfig, StreamBitsIO}
import org.scalatest._

import scala.util.Random
import scala.math.{min, pow}

class ClusterSpecTest extends FlatSpec with ChiselScalatestTester with Matchers with ClusterSRAMConfig with MCRENFConfig with SPadSizeConfig with GNMFCS2Config {
  private val oneSPadPSum: Int = M0*E*N0*F0 // when read counts this, then stop
  private val printLogDetails = true // true to print more detailed logs
  private val maxInActStreamNum: Int = min(inActAdrSRAMSize/inActAdrSPadSize, inActDataSRAMSize/inActDataSPadSize)
  private val theInActStreamNum: Int = (new Random).nextInt(maxInActStreamNum - 5) + 5
  private val maxPSumStreamNum: Int = pSumSRAMSize/oneSPadPSum
  private def InActDataGen(n: Int, dataWidth: Int, maxLen: Int, minLen: Int): List[Int] = {
    require(maxLen > minLen, s"maxLen should larger than minLen, $maxLen should lg $minLen")
    var resultList: List[Int] = Nil
    for (_ <- 0 until n) {
      var temResultList: List[Int] = Nil
      val randomLen: Int = (new Random).nextInt(maxLen-minLen) + minLen // the length of one SPad range(minLen, maxLen)
      while (temResultList.length < randomLen) {
        val randomNum = (new Random).nextInt(pow(2, dataWidth).toInt - 1) + 1
        temResultList = temResultList:::List(randomNum)
      }
      temResultList = temResultList:::List(0) // one zero, that's the end of one SPad data
      resultList = resultList:::temResultList
    }
    resultList = resultList:::List(0) // two zeros, that's the end of this stream data
    resultList
  }
  private def PSumDataGen(n:Int, dataWidth: Int): List[Int] = {
    //require(n <= maxPSumStreamNum, s"you should assign a smaller 'n' oneSPadPSum = $oneSPadPSum, pSumSRAMSize = $pSumSRAMSize")
    var resultList: List[Int] = Nil
    while ( resultList.length < n ){
      val randomNum = (new Random).nextInt(pow(2, dataWidth).toInt)
      resultList = resultList:::List(randomNum)
      /*
      if( !resultList.exists( s=>s==randomNum )){
        resultList = resultList:::List(randomNum)
      }
      */
    }
    resultList
  }
  private def readOutAct(outIO: StreamBitsIO, debugIO: SRAMCommonDebugIO, doneIO: Bool, theData: List[Int], idx: Int, theClock: Clock, adrOrData: Boolean): Any = {
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
      s"-------- $idx.0 $currentType crDt = ${debugIO.currentData.peek()}\n" +
      s"-------- $idx.0 $currentType waFR = ${debugIO.waitForRead.peek()}\n" +
      s"-------- $idx.0 $currentType doRd = ${debugIO.doReadWire.peek()}\n" +
      s"-------- $idx.0 $currentType idIc = ${debugIO.idxInc.peek()}"
    )
    theClock.step(1)
    debugIO.idxInc.expect(true.B, s"$idx, $currentType index should increase now")
    outIO.data.bits.expect(theData(idx).U, s"theData($idx) = ${theData(idx)}")
    outIO.data.valid.expect(true.B, s"$currentType should valid now")
    if (printLogDetails) println(
      s"-------- $idx.5 $currentType crDt = ${debugIO.currentData.peek()}\n" +
        s"-------- $idx.5 $currentType waFR = ${debugIO.waitForRead.peek()}\n" +
        s"-------- $idx.5 $currentType doRd = ${debugIO.doReadWire.peek()}\n" +
        s"-------- $idx.5 $currentType idIc = ${debugIO.idxInc.peek()}\n" +
        s"-------- $idx.5 $currentType data = ${outIO.data.bits.peek()}\n" +
        s"-------- $idx.5 $currentType idx  = ${debugIO.idx.peek()}"
    )
    theClock.step(1)
    if (theData(idx) != 0) {
      doneIO.expect(false.B, s"current data equals to ${theData(idx)}, does not equal to zero, should $currentType be unfinished now")
    }
    if (printLogDetails) {
      println(s"-------- ${idx + 1} $currentType done = ${doneIO.peek()}")
    }
    println(s"-------- ${idx + 1} $currentType PASS $idx")
  }
  private def readOutActAdrAndData(theTopIO: InActSRAMBankIO, theDebugIO: InActSRAMBankDebugIO, adrStream: List[Int], dataStream: List[Int], theClock: Clock): Any = {
    val theOutIO = theTopIO.dataPath.outIOs
    val theCtrlIO = theTopIO.ctrlPath
    var adrRdIdx = 0
    var dataRdIdx = 0
    for (_ <- 0 until theInActStreamNum) {
      theCtrlIO.doEn.poke(true.B)
      theClock.step(1) // from idle to doing
      fork {
        while (!theDebugIO.dataDebug.subDone.peek().litToBoolean) {
          theOutIO.dataIOs.data.ready.poke(true.B)
          readOutAct(theOutIO.dataIOs, theDebugIO.dataDebug.commonDebug, theDebugIO.dataDebug.subDone, dataStream, dataRdIdx, theClock, adrOrData = false)
          dataRdIdx = dataRdIdx + 1
        }
        println("------------ data read one stream -------------")
        theDebugIO.dataDebug.subDone.expect(true.B, s"current data equals to ${dataStream(dataRdIdx - 1)}, data should finish now")
        theOutIO.dataIOs.data.ready.poke(false.B)
      } .fork {
        while (!theDebugIO.adrDebug.subDone.peek().litToBoolean) {
          theOutIO.adrIOs.data.ready.poke(true.B)
          readOutAct(theOutIO.adrIOs, theDebugIO.adrDebug.commonDebug, theDebugIO.adrDebug.subDone, adrStream, adrRdIdx, theClock, adrOrData = true)
          adrRdIdx = adrRdIdx + 1
        }
        println("------------ adr read one stream --------------")
        theOutIO.adrIOs.data.ready.poke(false.B)
      } .join()
      println("---------- both read out one stream ------------")
      theCtrlIO.doEn.poke(false.B)
      theClock.step(cycles = (new Random).nextInt(50) + 50)
    }
  }
  private def writeInAct(inIO: StreamBitsIO, debugIO: SRAMCommonDebugIO, doneIO: Bool, theData: List[Int], theClock: Clock, adrOrData: Boolean) : Any = {
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
        println(s"-------- $currentType done = ${doneIO.peek()}\n-------- $currentType data = ${theData(i)}\n" +
          s"-------- $currentType index = ${debugIO.idx.peek()}\n-------- $currentType theState = ${debugIO.theState.peek()}")
      }
      theClock.step(1)
      doneIO.expect((i == theData.length - 1).B, s"$currentType Data($i) = ${theData(i)}, $currentType theState = ${debugIO.theState.peek()},should $currentType finish?")
      println(s"-------- $currentType PASS $i")
    }
  }
  private def writeInActAdrAndData(theInIO: CSCStreamIO, theDebugIO: InActSRAMBankDebugIO, adrStream: List[Int], dataStream: List[Int], theClock: Clock): Any = {
    fork {
      writeInAct(theInIO.dataIOs, theDebugIO.dataDebug.commonDebug, theDebugIO.dataDebug.subDone, dataStream, theClock, adrOrData = false)
      println("------------- data write finish ---------------")
    } .fork {
      writeInAct(theInIO.adrIOs, theDebugIO.adrDebug.commonDebug, theDebugIO.adrDebug.subDone, adrStream, theClock, adrOrData = true)
      println("------------ adr write finish ---------------")
    } .join()
  }
  // test GLB Cluster
  behavior of "test the spec of GLB Cluster"
  it should "work well on PSumSRAMBank" in {
    test(new PSumSRAMBank(pSumSRAMSize, psDataWidth, true)) { thePSumBank =>
      val theTopIO = thePSumBank.io
      val theClock = thePSumBank.clock
      val theData = PSumDataGen(pSumSRAMSize, psDataWidth)
      val startIndex = (new Random).nextInt(maxPSumStreamNum - 1) * oneSPadPSum
      println("----------------- test begin -----------------")
      println("----------- Partial Sum SRAM Bank ------------")
      println("----------- test basic functions -------------")
      println(s"-------- startIndex = $startIndex")
      thePSumBank.reset.poke(true.B)
      theClock.step(1)
      thePSumBank.reset.poke(false.B)
      println("--------------- begin to write ---------------")
      theTopIO.ctrlPath.writeOrRead.poke(true.B)
      theTopIO.ctrlPath.doEn.poke(true.B)
      theTopIO.ctrlPath.startIdx.poke(startIndex.U)
      for (i <- 0 until oneSPadPSum) {
        println(s"--------------- $i-th write cycle -----------")
        theTopIO.dataPath.inIOs.bits.poke(theData(i).U)
        theTopIO.dataPath.inIOs.valid.poke(true.B)
        theTopIO.debugIO.idx.expect((startIndex + i).U, s"startIdx = $startIndex")
        theTopIO.debugIO.idxInc.expect(true.B, s"$i, index should increase")
        if (printLogDetails) {
          println(s"--------       data = ${theData(i)} \n" +
            s"--------      index = ${theTopIO.debugIO.idx.peek()} \n" +
            s"-------- whetherInc = ${theTopIO.debugIO.idxInc.peek()}")
        }
        theTopIO.dataPath.inIOs.ready.expect(true.B, s"$i, it should be ready now")
        theClock.step(1)
        theTopIO.ctrlPath.done.expect((i == oneSPadPSum - 1).B, s"i = $i, write should finish?")
        println("-------- PASS")
      }
      println("---------------- write finish ----------------")
      theTopIO.ctrlPath.doEn.poke(false.B)
      theClock.step(cycles = (new Random).nextInt(5) + 1)
      println("--------------- begin to read ----------------")
      theTopIO.ctrlPath.writeOrRead.poke(false.B)
      theTopIO.ctrlPath.doEn.poke(true.B)
      theTopIO.ctrlPath.startIdx.poke(startIndex.U)
      for (i <- 0 until oneSPadPSum) {
        println(s"--------------- $i-th read cycle -----------")
        if (printLogDetails) {
          println(s"--------       data = ${theData(i)} \n" +
            s"--------      index = ${theTopIO.debugIO.idx.peek()} \n" +
            s"-------- whetherInc = ${theTopIO.debugIO.idxInc.peek()}")
        }
        theTopIO.dataPath.outIOs.ready.poke(true.B)
        theTopIO.debugIO.idxInc.expect(false.B, s"$i, index should not increase now")
        theTopIO.dataPath.outIOs.valid.expect(false.B, "it should not valid now")
        theClock.step(1)
        if (printLogDetails) {
          println(s"--------       data = ${theData(i)} \n" +
            s"--------      index = ${theTopIO.debugIO.idx.peek()} \n" +
            s"-------- whetherInc = ${theTopIO.debugIO.idxInc.peek()}")
        }
        theTopIO.debugIO.idxInc.expect(true.B, s"index should increase")
        theTopIO.dataPath.outIOs.bits.expect(theData(i).U, s"$i, theData should be ${theData(i)}")
        theTopIO.dataPath.outIOs.valid.expect(true.B, "it should valid now")
        theClock.step(1)
        theTopIO.ctrlPath.done.expect((i == oneSPadPSum - 1).B)
        println("-------- PASS")
      }
      println("---------------- read finish -----------------")
      println("---------------- test finish -----------------")
    }
  }

  it should "work well on InActSRAMCommon" in {
      test(new InActSRAMCommon(inActAdrSRAMSize, inActAdrWidth, true)) { theAdrSRAM =>
        val theTopIO = theAdrSRAM.io
        val theClock = theAdrSRAM.clock
        val InActAdrStream = InActDataGen(theInActStreamNum, inActAdrWidth, 8, 3)
        println("----------------- test begin -----------------")
        println("----------- InputActAdr SRAM Bank ------------")
        println("----------- test basic functions -------------")
        theAdrSRAM.reset.poke(true.B)
        theClock.step(1)
        theAdrSRAM.reset.poke(false.B)
        println("--------------- begin to write ---------------")
        theTopIO.ctrlPath.writeOrRead.poke(true.B)
        theTopIO.ctrlPath.doEn.poke(true.B)
        // begin to write in data
        writeInAct(theTopIO.dataPath.inIOs, theTopIO.debugIO, theTopIO.ctrlPath.done, InActAdrStream, theClock, adrOrData = true)
        println("--------------- write finish -----------------")
        theTopIO.ctrlPath.doEn.poke(false.B)
        theClock.step(cycles = (new Random).nextInt(5) + 1)
        println("--------------- begin to read ----------------")
        // begin to read out data
        var theRdIdx = 0
        for (_ <- 0 until theInActStreamNum) {
          theTopIO.ctrlPath.writeOrRead.poke(false.B)
          while (!theTopIO.ctrlPath.done.peek().litToBoolean) {
            theTopIO.ctrlPath.doEn.poke(true.B)
            theTopIO.dataPath.outIOs.data.ready.poke(true.B)
            readOutAct(theTopIO.dataPath.outIOs, theTopIO.debugIO, theTopIO.ctrlPath.done, InActAdrStream, theRdIdx, theClock, adrOrData = true)
            theRdIdx = theRdIdx + 1
          }
          theTopIO.ctrlPath.doEn.poke(false.B)
          theTopIO.dataPath.outIOs.data.ready.poke(false.B)
          println("------------ read out one stream -------------")
          theClock.step(cycles = (new Random).nextInt(50) + 50)
        }
        // read out finish
        println("---------------- read finish -----------------")
        println(s"-------- streamNum = $theInActStreamNum")
        println("---------------- test finish -----------------")
      }
  }

  it should "work well on InActSRAMBank" in {
    test(new InActSRAMBank(true)) { theInAct =>
      val theTopIO = theInAct.io
      val theClock = theInAct.clock
      val InActAdrStream = InActDataGen(theInActStreamNum, inActAdrWidth, 8, 3)
      val InActDataStream = InActDataGen(theInActStreamNum, inActDataWidth, 15, 9)
      println("----------------- test begin -----------------")
      println(s"--------  theInActStreamNum = $theInActStreamNum")
      println("----------- InputActAdr SRAM Bank ------------")
      println("----------- test basic functions -------------")
      theInAct.reset.poke(true.B)
      theClock.step(1)
      theInAct.reset.poke(false.B)
      println("--------------- begin to write ---------------")
      theTopIO.ctrlPath.writeOrRead.poke(true.B)
      theTopIO.ctrlPath.doEn.poke(true.B)
      theClock.step(1) // from idle to doing
      // begin to write streams into data sram and address sram
      writeInActAdrAndData(theTopIO.dataPath.inIOs, theTopIO.debugIO, InActAdrStream, InActDataStream, theClock)
      // write finish
      theClock.step(1)
      theTopIO.debugIO.theState.expect(0.U, "after all write, the state should be idle now")
      println(s"-------- theState = ${theTopIO.debugIO.theState.peek()}")
      println("------------- all write finish ---------------")
      theTopIO.ctrlPath.doEn.poke(false.B)
      theClock.step(cycles = (new Random).nextInt(5) + 1)
      println("--------------- begin to read ----------------")
      theTopIO.ctrlPath.writeOrRead.poke(false.B)
      theTopIO.ctrlPath.doEn.poke(true.B)
      // begin to read out streams into data sram and address sram
      readOutActAdrAndData(theTopIO, theTopIO.debugIO, InActAdrStream, InActDataStream, theClock)
      theClock.step(1)
      theTopIO.debugIO.theState.expect(0.U, "after all read, the state should be idle now")
      println(s"-------- theState = ${theTopIO.debugIO.theState.peek()}")
      println("-------------- all read finish ---------------")
      println(s"-------- streamNum = $theInActStreamNum")
      println("---------------- test finish -----------------")
    }
  }

  it should "work well on GLBCluster" in {
    test (new GLBCluster(true)) { theGLB =>
      val theTopIO = theGLB.io
      val theClock = theGLB.clock
      val theInActAdrStreams = Seq.fill(inActSRAMNum){InActDataGen(theInActStreamNum, inActAdrWidth, 8, 3)}
      val theInActDataStreams = Seq.fill(inActSRAMNum){InActDataGen(theInActStreamNum, inActDataWidth, 15, 9)}
      println("----------------- test begin -----------------")
      println(s"-------- theInActStreamNum = $theInActStreamNum")
      println("----------- InputActAdr SRAM Bank ------------")
      println("----------- test basic functions -------------")
      theGLB.reset.poke(true.B)
      theClock.step(1)
      theGLB.reset.poke(false.B)
      println("--------------- begin to write ---------------")
      theTopIO.ctrlPath.inActIO.doEn.poke(true.B)
      theTopIO.ctrlPath.inActIO.writeOrRead.poke(true.B)
      theClock.step(2) // top from idle to doing, sub from idle to doing;
      /*// TODO: use for loop to create fork
      val inActW = theTopIO.dataPath.inActIO.zipWithIndex.map({ case (o, i) =>
        fork(writeInActAdrAndData(theTopIO.dataPath.inActIO(i).inIOs, theTopIO.debugIO.inActDebugIO(i), theInActAdrStreams(i), theInActDataStreams(i), theClock)).join()
      })
      val inActWriteThread = new TesterThreadList()
      val initTh = Seq[AbstractTesterThread]()
      inActW.foldLeft{case (a, b) =>
        fork{
          fork(a).join()
          b
        }
      }
      inActW
      */
      require(inActSRAMNum == 3, "if you have more or less inActSRAM, please adjust the fork thread")
      fork {
        writeInActAdrAndData(theTopIO.dataPath.inActIO(0).inIOs, theTopIO.debugIO.inActDebugIO(0), theInActAdrStreams(0), theInActDataStreams(0), theClock)
        println(s"-------- 0 finish")
        println(s"-------- 0 ${theTopIO.debugIO.inActDebugIO(0).theState.peek()}")
        timescope{
          theClock.step(1)
          theTopIO.debugIO.inActDebugIO(0).theState.expect(0.U, "inActSRAMBank 0 should be idle one cycle later")
        }
      } .fork {
        writeInActAdrAndData(theTopIO.dataPath.inActIO(1).inIOs, theTopIO.debugIO.inActDebugIO(1), theInActAdrStreams(1), theInActDataStreams(1), theClock)
        println(s"-------- 1 finish")
        println(s"-------- 1 ${theTopIO.debugIO.inActDebugIO(1).theState.peek()}")
        timescope{
          theClock.step(1)
          theTopIO.debugIO.inActDebugIO(1).theState.expect(0.U, "inActSRAMBank 1 should be idle one cycle later")
        }
      } .fork {
        writeInActAdrAndData(theTopIO.dataPath.inActIO(2).inIOs, theTopIO.debugIO.inActDebugIO(2), theInActAdrStreams(2), theInActDataStreams(2), theClock)
        println(s"-------- 2 finish")
        println(s"-------- 2 ${theTopIO.debugIO.inActDebugIO(2).theState.peek()}")
        timescope{
          theClock.step(1)
          theTopIO.debugIO.inActDebugIO(2).theState.expect(0.U, "inActSRAMBank 2 should be idle one cycle later")
        }
      } .join()
      println("------------- all write finish ---------------")
      theTopIO.debugIO.inActDebugIO.foreach({ x =>
        x.adrDebug.commonDebug.theState.expect(0.U, "every address SRAM should be idle now")
        x.dataDebug.commonDebug.theState.expect(0.U, "every data SRAM should be idle now")
        if (printLogDetails) {
          println(s"-------- inActBankState =  ${theTopIO.debugIO.inActDebugIO(0).theState.peek()}")
          println(s"-------- adrSRAMState   =  ${theTopIO.debugIO.inActDebugIO(0).adrDebug.commonDebug.theState.peek()}")
          println(s"-------- dataSRAMState  =  ${theTopIO.debugIO.inActDebugIO(0).dataDebug.commonDebug.theState.peek()}")
        }
      })
      theClock.step(1)
      println("----------- one cycle later ----------")
      theTopIO.debugIO.inActDebugIO.foreach(_.theState.expect(0.U, "every inActSRAMBank should be idle now"))
      theTopIO.ctrlPath.inActIO.doEn.poke(false.B)
      theTopIO.debugIO.theState(0).expect(0.U, "after all write, the state should be idle now")
      theTopIO.debugIO.allDone(0).expect(true.B, "it should all done now")
      theTopIO.debugIO.oneInActSRAMDone.foreach(_.expect(true.B,"should be true now to generate done signal"))
      if (printLogDetails) {
        println(s"-------- topInActState ${theTopIO.debugIO.theState(0).peek()} ")
        println(s"-------- allDone ${theTopIO.debugIO.allDone(0).peek()}")
        theTopIO.debugIO.oneInActSRAMDone.foreach(x => println(s"-------- oneSRAMDone = ${x.peek()}"))
      }
      theClock.step(1)
      println("----------- one cycle later ----------")
      if (printLogDetails) {
        println(s"-------- topInActState ${theTopIO.debugIO.theState(0).peek()} ")
        println(s"-------- allDone ${theTopIO.debugIO.allDone(0).peek()}")
        theTopIO.debugIO.oneInActSRAMDone.foreach(x => println(s"-------- oneSRAMDone = ${x.peek()}"))
      }
      theClock.step(cycles = (new Random).nextInt(5) + 1)
      theTopIO.debugIO.inActDebugIO.foreach({ x =>
        x.theState.expect(0.U, "every inActSRAMBank still needs to be idle")
        x.adrDebug.commonDebug.theState.expect(0.U, "every address SRAM still needs to be idle")
        x.dataDebug.commonDebug.theState.expect(0.U, "every data SRAM still needs to be idle")
        if (printLogDetails) {
          println(s"-------- inActBankState =  ${theTopIO.debugIO.inActDebugIO(0).theState.peek()}")
          println(s"-------- adrSRAMState   =  ${theTopIO.debugIO.inActDebugIO(0).adrDebug.commonDebug.theState.peek()}")
          println(s"-------- dataSRAMState  =  ${theTopIO.debugIO.inActDebugIO(0).dataDebug.commonDebug.theState.peek()}")
        }
      })
      theTopIO.debugIO.allDone(0).expect(false.B, "after several cycles, inActTop done signal should be false as initial state")
      theTopIO.debugIO.theState(0).expect(0.U, "Without enable signal, the inActTop state should be idle")
      theTopIO.debugIO.oneInActSRAMDone.foreach(_.expect(false.B,"after several cycles, every Vec Reg should be false as initial state"))
      if (printLogDetails) {
        println(s"-------- topInActState ${theTopIO.debugIO.theState(0).peek()} ")
        println(s"-------- allDone ${theTopIO.debugIO.allDone(0).peek()}")
        theTopIO.debugIO.oneInActSRAMDone.foreach(x => println(s"-------- oneSRAMDone = ${x.peek()}"))
      }
    }
  }
  //behavior of "work well on Processing Element Cluster"
  //behavior of "work well on Cluster Group"
  /*behavior of "test the spec of Router Cluster"
  it should "work well on Router Cluster" in {

  }*/
}

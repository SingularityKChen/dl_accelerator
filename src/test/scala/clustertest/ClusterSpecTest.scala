package dla.test.clustertest

import chisel3._
import chisel3.tester._
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
    for (i <- 0 until n) {
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
  private def readOutAct(outIO: StreamBitsIO, debugIO: SRAMCommonDebugIO, doneIO: Bool, theData: List[Int], theClock: Clock, adrOrData: Boolean): Any = {
    var currentType: String = "Default"
    if (adrOrData) {
      currentType = "adr"
    } else {
      currentType = "data"
    }
    theData.zipWithIndex.foreach({ case (x, idx) =>
      println(s"--------------- $idx-th read $currentType cycle --------------")
      outIO.data.valid.expect(false.B, s"$currentType should not valid now")
      debugIO.idxInc.expect(false.B, s"$idx, index should not increase now")
      if (printLogDetails) println(s"--------  currentData = ${debugIO.currentData.peek()}")
      theClock.step(1)
      debugIO.idxInc.expect(true.B, s"$idx, index should increase now")
      outIO.data.bits.expect(theData(idx).U, s"theData($idx) = ${theData(idx)}")
      outIO.data.valid.expect(true.B, s"$currentType should valid now")
      if (printLogDetails) {
        println(s"--------  currentData = ${debugIO.currentData.peek()}")
        println(s"--------  data = ${outIO.data.bits.peek()}\n" +
          s"-------- index = ${debugIO.idx.peek()}")
      }
      theClock.step(1)
      if (x != 0) {
        doneIO.expect(false.B, s"current data equals to ${theData(idx)}, does not equal to zero, should $currentType be unfinished now")
      } else {
        doneIO.expect(true.B, s"current data equals to ${theData(idx)}, should $currentType finish now")
      }
      if (printLogDetails) {
        println(s"--------  done = ${doneIO.peek()}")
      }
      println(s"-------- $currentType PASS $idx")
    })
  }
  private def readOutActAdrAndData(theOutIO: CSCStreamIO, theDebugIO: InActSRAMBankDebugIO, adrStream: List[Int], dataStream: List[Int], theClock: Clock): Any = {
    fork {
      readOutAct(theOutIO.dataIOs, theDebugIO.dataDebug, theDebugIO.dataDebug.subDone, dataStream, theClock, adrOrData = false)
      println("------------- data read finish ---------------")
    } .fork {
      readOutAct(theOutIO.adrIOs, theDebugIO.adrDebug, theDebugIO.adrDebug.subDone, adrStream, theClock, adrOrData = true)
      println("------------ adr read finish ---------------")
    } .join()

  }
  private def writeInAct(inIO: StreamBitsIO, debugIO: SRAMCommonDebugIO, doneIO: Bool, theData: List[Int], theClock: Clock, adrOrData: Boolean) : Any = {
    var currentType: String = "Default"
    if (adrOrData) {
      currentType = "adr"
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
      writeInAct(theInIO.dataIOs, theDebugIO.dataDebug, theDebugIO.dataDebug.subDone, dataStream, theClock, adrOrData = false)
      println("------------- data write finish ---------------")
    } .fork {
      writeInAct(theInIO.adrIOs, theDebugIO.adrDebug, theDebugIO.adrDebug.subDone, adrStream, theClock, adrOrData = true)
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
      theClock.step(1)
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
        val theData = InActDataGen(theInActStreamNum, inActAdrWidth, 8, 3)
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
        writeInAct(theTopIO.dataPath.inIOs, theTopIO.debugIO, theTopIO.ctrlPath.done, theData, theClock, adrOrData = true)
        println("--------------- write finish -----------------")
        theTopIO.ctrlPath.doEn.poke(false.B)
        theClock.step(1)
        println("--------------- begin to read ----------------")
        theTopIO.ctrlPath.writeOrRead.poke(false.B)
        theTopIO.ctrlPath.doEn.poke(true.B)
        theTopIO.dataPath.outIOs.data.ready.poke(true.B)
        // begin to read out data
        readOutAct(theTopIO.dataPath.outIOs, theTopIO.debugIO, theTopIO.ctrlPath.done, theData, theClock, adrOrData = true)
        // read out finish
        println("---------------- read finish -----------------")
        println("---------------- test finish -----------------")
      }
  }

  it should "work well on InActSRAMBank" in {
    test(new InActSRAMBank(true)) { theInAct =>
      val theTopIO = theInAct.io
      val theClock = theInAct.clock
      val theAdrStream = InActDataGen(theInActStreamNum, inActAdrWidth, 8, 3)
      val theDataStream = InActDataGen(theInActStreamNum, inActDataWidth, 15, 9)
      println("----------------- test begin -----------------")
      println(s"--------  ")
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
      writeInActAdrAndData(theTopIO.dataPath.inIOs, theTopIO.debugIO, theAdrStream, theDataStream, theClock)
      // write finish
      theClock.step(1)
      theTopIO.debugIO.theState.expect(0.U, "after all write, the state should be idle now")
      println(s"-------- theState = ${theTopIO.debugIO.theState.peek()}")
      println("------------- all write finish ---------------")
      theTopIO.ctrlPath.doEn.poke(false.B)
      theClock.step(1)
      println("--------------- begin to read ----------------")
      theTopIO.ctrlPath.writeOrRead.poke(false.B)
      theTopIO.ctrlPath.doEn.poke(true.B)
      theClock.step(1) // from idle to doing
      // begin to read out streams into data sram and address sram
      readOutActAdrAndData(theTopIO.dataPath.outIOs, theTopIO.debugIO, theAdrStream, theDataStream, theClock)
      theClock.step(1)
      theTopIO.debugIO.theState.expect(0.U, "after all read, the state should be idle now")
      println(s"-------- theState = ${theTopIO.debugIO.theState.peek()}")
      println("-------------- all read finish ---------------")
      println("---------------- test finish -----------------")
    }
  }
  //behavior of "work well on Processing Element Cluster"
  //behavior of "work well on Cluster Group"
  /*behavior of "test the spec of Router Cluster"
  it should "work well on Router Cluster" in {

  }*/
}

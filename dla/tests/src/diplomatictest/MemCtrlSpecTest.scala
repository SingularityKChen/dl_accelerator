package dla.tests.diplomatictest

import chisel3._
import chisel3.tester._
import chisel3.tester.experimental.TestOptionBuilder._
import chisel3.util._
import chiseltest.internal.WriteVcdAnnotation
import dla.cluster.{ClusterConfig, ClusterSRAMConfig}
import dla.diplomatic.{EyerissIDMapGenerator, EyerissMemCtrlModule, EyerissMemCtrlParameters}
import org.scalatest._

import scala.util.Random

object MemCtrlDriver {
  def pokeRespSourceId(reqList: List[Int], respList: List[Int], freeIO: DecoupledIO[UInt], theClock: Clock): List[Int] = {
    var newRespList: List[Int] = Nil
    theClock.step((new Random).nextInt(15))
    if (reqList.isEmpty) {
      println(s"[Info] reqList is empty now, reqList = $reqList")
    } else {
      val diffIdList = reqList.diff(respList)
      if (diffIdList.isEmpty) {
        println(Console.RED + s"[Error] there is no difference between reqList and respList" + Console.RESET)
      } else {
        val respId = diffIdList((new Random).nextInt(diffIdList.length))
        freeIO.valid.poke(true.B)
        freeIO.bits.poke(respId.U)
        println(s"[free@${respList.length}] the response source id is $respId")
        newRespList = respList:::List(respId)
      }
    }
    theClock.step()
    freeIO.valid.poke(false.B)
    newRespList
  }
}

object MemCtrlMonitor {
  def peekReqSourceId(reqList: List[Int], allocIO: DecoupledIO[UInt], theClock: Clock): List[Int] = {
    var newReqList: List[Int] = Nil
    val reqNum = reqList.length
    theClock.step((new Random).nextInt(10))
    allocIO.ready.poke(true.B)
    if (allocIO.valid.peek().litToBoolean) {
      val reqId = allocIO.bits.peek().litValue().toInt
      println(s"[alloc@$reqNum] the require source Id is $reqId")
      newReqList = reqList:::List(reqId)
    } else {
      println(Console.RED + s"[Error] alloc is not valid @$reqNum" + Console.RESET)
    }
    theClock.step()
    allocIO.ready.poke(false.B)
    newReqList
  }
}

class MemCtrlSpecTest extends FlatSpec with ChiselScalatestTester
  with Matchers with ClusterConfig with ClusterSRAMConfig {
  private val getSourceNum = inActRouterNum + weightRouterNum
  private val driver = MemCtrlDriver
  private val monitor = MemCtrlMonitor
  private val decoderSequencer = DecoderSequencer
  behavior of "test the spec of diplomatic memory controller"
  it should "work well on MemCtrlModule" in {
    implicit val p: EyerissMemCtrlParameters =
      EyerissMemCtrlParameters(addressBits = 32, // TODO: check
        inActSizeBits = 10, weightSizeBits = 10, pSumSizeBits = log2Ceil(pSumSRAMSize), // TODO: check
        inActIds = inActRouterNum, weightIds = weightRouterNum, pSumIds = pSumRouterNum)
    test(new EyerissMemCtrlModule()(p)).withAnnotations(Seq(WriteVcdAnnotation)) { theMemCtrl =>
      val theTopIO = theMemCtrl.io
      val theClock = theMemCtrl.clock
      var inActReqFormerList: List[Int] = Nil
      var inActRespFormerList: List[Int] = Nil
      var inActReqLaterList: List[Int] = Nil
      var inActRespLaterList: List[Int] = Nil
      var weightReqList: List[Int] = Nil
      var weightRespList: List[Int] = Nil
      theMemCtrl.reset.poke(true.B)
      theClock.step()
      theMemCtrl.reset.poke(false.B)
      println("----------------- test begin -----------------")
      println("------------ generate id source --------------")
      theTopIO.inActIO.startAdr.poke(decoderSequencer.inActAdr.hex.U)
      theTopIO.inActIO.reqSize.poke(decoderSequencer.reqSize.inAct.U)
      theTopIO.weightIO.startAdr.poke(decoderSequencer.weightAdr.hex.U)
      theTopIO.weightIO.reqSize.poke(decoderSequencer.reqSize.weight.U)
      theClock.step()
      /** load GLB, req for inAct*/
      fork {
        while (inActReqFormerList.length < inActRouterNum) {
          inActReqFormerList = monitor.peekReqSourceId(inActReqFormerList, theTopIO.inActIO.sourceAlloc, theClock)
        }
        println("------------ reqLater now --------------")
        while (inActReqLaterList.length < inActRouterNum) {
          inActReqLaterList = monitor.peekReqSourceId(inActReqLaterList, theTopIO.inActIO.sourceAlloc, theClock)
        }
      } .fork.withRegion(Monitor) {
        fork {
          while (inActRespFormerList.length < inActRouterNum) {
            inActRespFormerList = driver.pokeRespSourceId(inActReqFormerList,
              inActRespFormerList, theTopIO.inActIO.sourceFree, theClock)
          }
          while (inActRespLaterList.length < inActRouterNum) {
            inActRespLaterList = driver.pokeRespSourceId(inActReqFormerList,
              inActRespLaterList, theTopIO.inActIO.sourceFree, theClock)
          }
        } .fork {
          while (inActRespLaterList.length < inActRouterNum) {
            if (theTopIO.inActIO.sourceAlloc.ready.peek().litToBoolean &&
              theTopIO.inActIO.sourceAlloc.valid.peek().litToBoolean)
            {
              println(s"inAct address = ${theTopIO.inActIO.address.peek().litValue()}")
              theClock.step()
            } else {
              theClock.step()
            }
          }
        }.join()
      }.joinAndStep(theClock)
      /** load pe, req for weight*/
      theClock.step((new Random).nextInt(10))
      fork {
        while (weightReqList.length < weightRouterNum) {
          weightReqList = monitor.peekReqSourceId(weightReqList, theTopIO.weightIO.sourceAlloc, theClock)
        }
      } .fork.withRegion(Monitor) {
        fork {
          while (weightRespList.length < weightRouterNum) {
            weightRespList = driver.pokeRespSourceId(weightReqList, weightRespList, theTopIO.weightIO.sourceFree, theClock)
          }
        } .fork {
          while (weightRespList.length < weightRouterNum) {
            if (theTopIO.weightIO.sourceAlloc.ready.peek().litToBoolean &&
              theTopIO.weightIO.sourceAlloc.valid.peek().litToBoolean)
            {
              println(s"weight address = ${theTopIO.weightIO.address.peek().litValue()}")
              theClock.step()
            } else {
              theClock.step()
            }
          }
        }.join()
      }.joinAndStep(theClock)
    }
  }

  it should "work well on EyerissIDMapGenerator" in {
    test (new EyerissIDMapGenerator(getSourceNum)) { theIdMap =>
      val theTopIO = theIdMap.io
      val theClock = theIdMap.clock
      val theAllocIO = theTopIO.alloc
      val theFreeIO = theTopIO.free
      var reqList: List[Int] = Nil
      var respList: List[Int] = Nil
      theIdMap.reset.poke(true.B)
      theClock.step()
      theIdMap.reset.poke(false.B)
      println("----------------- test begin -----------------")
      println("------------ generate id source --------------")
      fork {
        while (reqList.length < getSourceNum) {
          reqList = monitor.peekReqSourceId(reqList, theAllocIO, theClock)
        }
        println(Console.YELLOW + "[Info] all sources have send requirements" + Console.RESET)
        theAllocIO.valid.expect(false.B, "valid should be false as all have send req")
      } .fork.withRegion(Monitor) {
        while (!theTopIO.finish.peek().litToBoolean) {
          respList = driver.pokeRespSourceId(reqList, respList, theFreeIO, theClock)
        }
      } .joinAndStep(theClock)
      println(Console.GREEN + "[Success] all sources have send requirements and received response" + Console.RESET)
    }
  }
}

package dla.tests.diplomatictest

import chisel3._
import chisel3.tester._
import dla.cluster.ClusterConfig
import dla.diplomatic.{EyerissIDMapGenerator, EyerissMemCtrlModule, MemCtrlParameters}
import org.scalatest._
import scala.util.Random

class MemCtrlSpecTest extends FlatSpec with ChiselScalatestTester
  with Matchers with ClusterConfig {
  private val getSourceNum = inActRouterNum + weightRouterNum
  private val putSourceNum = pSumRouterNum
  behavior of "test the spec of diplomatic memory controller"
  it should "work well on MemCtrlModule" in {
    implicit val p: MemCtrlParameters =
      MemCtrlParameters(addressBits = 5, sizeBits = 10, dataBits = 8, nIds = getSourceNum)
    test(new EyerissMemCtrlModule()(p)) { theMemCtrl =>
      val theTopIO = theMemCtrl.io
      val theClock = theMemCtrl.clock
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
          val reqNum = reqList.length
          theClock.step((new Random).nextInt(10))
          theAllocIO.ready.poke(true.B)
          if (theAllocIO.valid.peek().litToBoolean) {
            val reqId = theAllocIO.bits.peek().litValue().toInt
            println(s"[alloc@$reqNum] the require source Id is $reqId")
            reqList = reqList:::List(reqId)
          } else {
            println(Console.RED + s"[Error] alloc is not valid @$reqNum" + Console.RESET)
          }
          theClock.step()
          theAllocIO.ready.poke(false.B)
        }
        println(Console.YELLOW + "[Info] all sources have send requirements" + Console.RESET)
        theAllocIO.valid.expect(false.B, "valid should be false as all have send req")
      } .fork.withRegion(Monitor) {
        while (!theTopIO.finish.peek().litToBoolean) {
          theClock.step((new Random).nextInt(15))
          if (reqList.isEmpty) {
            println(s"[Info] reqList is empty now, reqList = $reqList")
          } else {
            val diffIdList = reqList.diff(respList)
            if (diffIdList.isEmpty) {
              println(Console.RED + s"[Error] there is no difference between reqList and respList" + Console.RESET)
            } else {
              val respId = diffIdList((new Random).nextInt(diffIdList.length))
              theFreeIO.valid.poke(true.B)
              theFreeIO.bits.poke(respId.U)
              println(s"[free@${respList.length}] the response source id is $respId")
              respList = respList:::List(respId)
            }
          }
          theClock.step()
          theFreeIO.valid.poke(true.B)
        }
      } .joinAndStep(theClock)
      println(Console.GREEN + "[Success] all sources have send requirements and received response" + Console.RESET)
    }
  }
}

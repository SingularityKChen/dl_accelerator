package dla.tests.diplomatictest

import chisel3._
import chisel3.tester._
import chiseltest.internal.WriteVcdAnnotation
import chisel3.tester.experimental.TestOptionBuilder._
import dla.cluster.{ClusterConfig, GNMFCS1Config, GNMFCS2Config}
import dla.diplomatic.{EyerissMemCtrlBundle, EyerissTop, EyerissTopParam}
import dla.pe.MCRENFConfig
import dla.tests.GenOneStreamData
import org.scalatest._

import scala.util.Random

object EyerissTLBundleDriver {
  def readReqAndResp(peekIO: EyerissMemCtrlBundle, respData: Seq[List[Int]], sourceIdNum: Int, starAdr: Int)
                    (implicit theClock: Clock): Unit = {
    var respSourceId: List[Int] = Nil
    val reqSourceIdMap: Map[Int, Int] = Map() // sourceId -> respDataIdx
    peekIO.legal.poke(true.B)
    fork.withName("manage requirement") {
      while (reqSourceIdMap.size < sourceIdNum) {
        peekIO.a.ready.poke(true.B)
        while (!peekIO.a.valid.peek().litToBoolean) {
          theClock.step()
        }
        val currentReqSource = peekIO.a.bits.source.peek().litValue().toInt
        if (reqSourceIdMap.contains(currentReqSource)) {
          peekIO.reqFirst.poke(false.B)
        } else {
          peekIO.reqFirst.poke(true.B)
          val reqSize = peekIO.reqSize.peek().litValue().toInt
          val address = peekIO.address.peek().litValue().toInt
          val reqDataIdx = (starAdr - address)/reqSize
          reqSourceIdMap.++(Map(currentReqSource -> reqDataIdx))
        }
        println(s"[req@${reqSourceIdMap.size}] the require source Id is $currentReqSource, reqSourceId = $reqSourceIdMap")
        theClock.step()
        peekIO.reqFirst.poke(false.B)
        peekIO.a.ready.poke(false.B)
      }
    }.fork.withName("manage response") {
      while (respSourceId.length < sourceIdNum) {
        peekIO.d.valid.poke(true.B)
        while (!peekIO.d.ready.peek().litToBoolean) {
          theClock.step()
        }
        val diffIdList = reqSourceIdMap.filter(map => !respSourceId.contains(map._1)).keys.toList
        if (diffIdList.isEmpty) {
          println(Console.RED + s"[Error] there is no difference between reqList and respList" + Console.RESET)
        } else {
          val respId = diffIdList((new Random).nextInt(diffIdList.length))
          val peekRespDataIdx = reqSourceIdMap(respId)
          peekIO.d.bits.source.poke(respId.U)
          respSourceId = respSourceId:::List(respId)
          println(s"[resp@${respSourceId.length}] the response source id is $respId, respSourceId = $respSourceId")
          /** now peek data */
          peekIO.respFirst.poke(true.B)
          for (dataIdx <- respData(peekRespDataIdx).indices) {
            peekIO.d.valid.poke(true.B)
            peekIO.d.bits.data.poke(respData(peekRespDataIdx)(dataIdx).U)
            while (!peekIO.d.ready.peek().litToBoolean) {
              theClock.step()
            }
            theClock.step()
            peekIO.respFirst.poke(false.B)
            peekIO.d.valid.poke(false.B)
          }
        }
        theClock.step()
      }
    }.joinAndStep(theClock)
  }
}

class EyerissTopSpecTest extends FlatSpec with ChiselScalatestTester with Matchers
  with ClusterConfig {
  private val param = EyerissTopParam(
    addressBits = 32,
    inActDataBits = 32,
    inActSourceBits = 3,
    weightDataBits = 32,
    weightSourceBits = 3,
    pSumDataBits = 32,
    pSumSourceBits = 3
  )
  private val decoderSequencer = DecoderSequencer
  private val dataSequencer = new GenOneStreamData
  private val dataDriver = EyerissTLBundleDriver
  behavior of "test the spec of EyerissTop"
  it should "work well on cal" in {
    test(new EyerissTop(param = param)).withAnnotations(Seq(WriteVcdAnnotation)) { eyeriss =>
      val theTopIO = eyeriss.io
      implicit val theClock: Clock = eyeriss.clock
      eyeriss.reset.poke(true.B)
      theClock.step()
      eyeriss.reset.poke(false.B)
      theTopIO.ctrlPath.instructions.poke(decoderSequencer.loadPart0.U)
      theClock.step(cycles = (new Random).nextInt(15) + 1)
      theTopIO.ctrlPath.instructions.poke(decoderSequencer.loadPart1.U)
      theClock.step(cycles = (new Random).nextInt(15) + 1)
      theTopIO.ctrlPath.instructions.poke(decoderSequencer.loadPart2.U)
      theClock.step(cycles = (new Random).nextInt(15) + 1)
      theTopIO.ctrlPath.instructions.poke(decoderSequencer.loadPart3.U)
      /** then it will begins to load GLB */
      dataDriver.readReqAndResp(theTopIO.ctrlPath.bundles.memInActBundles,
        dataSequencer.inActStream.map(x => x.flatten), sourceIdNum = inActRouterNum,
        starAdr = decoderSequencer.inActAdr.hex)
      /** load later inActGLB */
      dataDriver.readReqAndResp(theTopIO.ctrlPath.bundles.memInActBundles,
        dataSequencer.inActStream.map(x => x.flatten), sourceIdNum = inActRouterNum,
        starAdr = decoderSequencer.inActAdr.hex)
      /** load weight */
      dataDriver.readReqAndResp(theTopIO.ctrlPath.bundles.memWeightBundles,
        dataSequencer.weightStream.map(x => x.flatten), sourceIdNum = weightRouterNum,
        starAdr = decoderSequencer.weightAdr.hex)
    }
  }
}

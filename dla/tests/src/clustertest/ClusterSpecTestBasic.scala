package dla.tests.clustertest

import chisel3.tester._
import dla.cluster._
import dla.eyerissWrapper.EyerissTopConfig
import dla.pe.{MCRENFConfig, SPadSizeConfig}
import dla.tests.GenOneStreamData
import org.scalatest._

import scala.util.Random

class ClusterSpecTestBasic extends FlatSpec with ChiselScalatestTester with Matchers
  with ClusterSRAMConfig with MCRENFConfig with SPadSizeConfig with GNMFCS2Config
  with EyerissTopConfig with GNMFCS1Config {
  //protected val printLogDetails = false // true to print more detailed logs
  protected val printLogDetails = true // true to print more detailed logs
  protected val maxPSumStreamNum: Int = pSumSRAMSize/pSumOneSPadNum
  protected val addendRand: Seq[Seq[Int]] = Seq.fill(peColNum, pSumOneSPadNum){(new Random).nextInt(10)}
  protected val peNum: Int = peRowNum * peColNum * cgRowNum * cgColNum
  protected val oneStreamData = new GenOneStreamData
  protected val inActAdrStream: Seq[List[Int]] = oneStreamData.inActAdrStream
  protected val inActDataStream: Seq[List[Int]] = oneStreamData.inActDataStream
  protected val weightAdrStream: Seq[List[Int]] = oneStreamData.weightAdrStream
  protected val weightDataStream: Seq[List[Int]] = oneStreamData.weightDataStream
  protected val pSumStream: Seq[List[Int]] = oneStreamData.outPSumStream
  protected val theInActAdrStreams: Seq[List[Int]] = inActAdrStream.take(inActRouterNum*2) // in actual, it needs s2 + f2
  protected val theInActDataStreams: Seq[List[Int]] = inActDataStream.take(inActRouterNum*2)
  protected val theWeightAdrStreams: Seq[List[Int]] = weightAdrStream.take(weightRouterNum)
  protected val theWeightDataStreams: Seq[List[Int]] = weightDataStream.take(weightRouterNum)
  protected val thePSumDataStreams: Seq[List[Int]] = pSumStream.take(pSumRouterNum)
  protected def getStreamLookUp(streamData: List[Int]): List[Int] = {
    var lookList: List[Int] = Nil
    lookList = lookList:::List(0)
    for (i <- 0 until streamData.length - 1) {
      if (streamData(i) == 0) {
        lookList = lookList:::List(i + 1)
      }
    }
    lookList.init
  }
}

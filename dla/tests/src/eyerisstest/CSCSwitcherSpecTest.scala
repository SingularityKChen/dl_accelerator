package dla.tests.eyerisstest

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.tester._
import dla.cluster.GNMFCS2Config
import dla.eyerissTop.CSCSwitcher
import dla.pe.{MCRENFConfig, PESizeConfig}
import dla.tests.GenOneStreamData
import firrtl.options.TargetDirAnnotation
import org.scalatest._

class CSCSwitcherSpecTest  extends FlatSpec with ChiselScalatestTester with Matchers
  with PESizeConfig with MCRENFConfig with GNMFCS2Config {
  private val oneStreamData = new GenOneStreamData
  private val oneStreamInActInData = oneStreamData.inActStream.take(inActStreamNum)
  private val oneInActAdrStream = oneStreamInActInData.flatMap(x =>
    oneStreamData.genAdrCountData(x, inActOrWeight = true).head)
  private val oneInActDataStream = oneStreamData.inActDataStream.head
  private val oneStreamWeightInData = oneStreamData.weightStream.take(weightStreamNum)
  private val oneWeightAdrStream = oneStreamData.weightAdrStream.head
  private val oneWeightDataStream = oneStreamData.weightDataStream.head
  behavior of "test the function of CSCSwitcher"
  it should "compress common data" in {
    test (new CSCSwitcher(adrWidth = inActAdrWidth, debug = true)) { theSwitcher =>
      val theTopIO = theSwitcher.io
      val theClock = theSwitcher.clock
      var outAdr: List[Int] = Nil
      var outData: List[Int] = Nil
      theSwitcher.reset.poke(true.B)
      theClock.step()
      theSwitcher.reset.poke(false.B)
      println("----------------- test begin -----------------")
      println("---------- Compress Data into CSC ------------")
      println("--------- test basic data situation ----------")
      theTopIO.matrixHeight.poke(inActMatrixHeight.U)
      theTopIO.matrixWidth.poke(inActMatrixWidth.U)
      theTopIO.vectorNum.poke(inActStreamNum.U)
      theClock.step()
      for (streamNum <- oneStreamInActInData.indices) {
        val oneInActInData = oneStreamInActInData(streamNum).zipWithIndex.map({ case (ints, row) =>
            ints.zipWithIndex.map({ case (data, col) => (data, row, col)})})
        for (col <- oneInActInData.head.indices) { // each column
          for (row <- oneInActInData.indices) {
            val postfix: String = s"@stream$streamNum@row$row@col$col"
            theTopIO.inData.valid.poke(true.B)
            theTopIO.inData.bits.poke(oneInActInData(row)(col)._1.U)
            println(s"[inData$postfix] inData = (${oneInActInData(row)(col)}, ${theTopIO.inData.ready.peek()})")
            if (oneInActInData(row)(col)._1 != 0)
              println(s"[inData$postfix] inDataBinary = ${oneInActInData(row)(col)._1.toBinaryString}")
            theTopIO.outData.dataIOs.data.ready.poke(true.B)
            theTopIO.outData.adrIOs.data.ready.poke(true.B)
            if (theTopIO.outData.adrIOs.data.valid.peek().litToBoolean) {
              val adrOut = theTopIO.outData.adrIOs.data.bits.peek().litValue()
              println(s"[outData$postfix] adr = ($adrOut," +
                s" ${theTopIO.outData.adrIOs.data.valid.peek()}, firstNone ${theTopIO.debugIO.firstNoneZero.peek()}, " +
                s"zeroCol = ${theTopIO.debugIO.zeroColReg.peek()})")
              outAdr = outAdr:::List(adrOut.toInt)
            }
            if (theTopIO.outData.dataIOs.data.valid.peek().litToBoolean) {
              val dataOut = theTopIO.outData.dataIOs.data.bits.peek().litValue()
              println(s"[outData$postfix] data = " +
                s"(${dataOut.toInt.toBinaryString.take(cscDataWidth)}," +
                s" ${theTopIO.outData.dataIOs.data.valid.peek()})")
              outData = outData:::List(dataOut.toInt)
              println(s"firstNone = ${theTopIO.debugIO.firstNoneZero.peek()}")
            }
            println(s"endFlag = ${theTopIO.debugIO.endFlag.peek()}")
            theClock.step()
            println(s"one cycle later, cscAdrReg = ${theTopIO.debugIO.cscAdrReg.peek()}")
            println(s"columnCounter = ${theTopIO.debugIO.columnCounter.peek()}")
          }
          println("[info] finish one column")
        }
      }
      for (i <- oneStreamInActInData.head.indices) {
        oneStreamInActInData.head(i).foreach(x => print(s"$x\t\t"))
        println()
      }
      println(s"[info] goldenAdr Vs. outAdr = ${oneInActAdrStream.zip(outAdr)}")
      println(s"[info] goldenData Vs. outData = ${oneInActDataStream.zip(outData)}")
    }
  }
}
/*
object emitCSCSwitcher extends App with PESizeConfig {
  (new chisel3.stage.ChiselStage).run(Seq(
    ChiselGeneratorAnnotation(() => new CSCSwitcher(adrWidth = inActAdrWidth, debug = false)),
    TargetDirAnnotation(directory = "test_run_dir")
  ))
}
*/
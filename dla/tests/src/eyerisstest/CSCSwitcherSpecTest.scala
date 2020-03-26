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
      val theDebugIO = theTopIO.debugIO
      val theClock = theSwitcher.clock
      var outAdr: List[Int] = Nil
      var outData: List[Int] = Nil
      theTopIO.inData.setSourceClock(theClock)
      theSwitcher.reset.poke(true.B)
      theClock.step()
      theSwitcher.reset.poke(false.B)
      println("----------------- test begin -----------------")
      println("---------- Compress Data into CSC ------------")
      println("--------- test basic data situation ----------")
      theTopIO.matrixHeight.poke(inActMatrixHeight.U)
      theTopIO.matrixWidth.poke(inActMatrixWidth.U)
      theTopIO.vectorNum.poke(inActStreamNum.U)
      require(inActStreamNum == oneStreamInActInData.length, "inActStreamNum should eq oneStreamInActInData.length")
      require(inActMatrixHeight == oneStreamInActInData.head.length)
      require(inActMatrixWidth == oneStreamInActInData.head.head.length)
      theClock.step()
      var streamNum = 0
      var adrIdx = 0 // the out address vector index
      var dataIdx = 0
      while (streamNum < oneStreamInActInData.length) {
        val oneInActInData = oneStreamInActInData(streamNum).zipWithIndex.map({ case (ints, row) =>
          ints.zipWithIndex.map({ case (data, col) => (data, row, col)})})
        println(s"------------ begin $streamNum matrix now ------------")
        for (i <- oneStreamInActInData.head.indices) {
          oneStreamInActInData(streamNum)(i).foreach(x => print(s"$x\t\t"))
          println()
        }
        var col = 0
        while (col < oneInActInData.head.length) {
          var row = 0
          while (row < oneInActInData.length) {
            theTopIO.outData.dataIOs.data.ready.poke(true.B)
            theTopIO.outData.adrIOs.data.ready.poke(true.B)
            theTopIO.inData.valid.poke(true.B)
            val inPostfix: String = s"inData@stream$streamNum@row$row@col$col"
            val outCol = theDebugIO.columnCounter.peek().litValue()
            val outRow = theDebugIO.currentRow.peek().litValue()
            val outStreamNum = theDebugIO.currentStreamNum.peek().litValue()
            val outPostfix: String = s"@stream$outStreamNum@row$outRow@col$outCol"
            if (theTopIO.inData.ready.peek().litToBoolean) {
              theTopIO.inData.bits.poke(oneInActInData(row)(col)._1.U)
              if (oneInActInData(row)(col)._1 != 0) {
                println(s"[$inPostfix] inData = (${oneInActInData(row)(col)}, ${theTopIO.inData.ready.peek()})")
                println(s"[$inPostfix] inDataBinary = ${oneInActInData(row)(col)._1.toBinaryString}")
              } else {
                println(".")
              }
              row += 1
            } else {
              println(s"[!!!!!WARNING] inData is not ready now!!!")
              println(s"endFlag = ${theDebugIO.endFlag.peek()}")
              println(s"currentRow = $outRow")
            }
            if (theTopIO.outData.adrIOs.data.valid.peek().litToBoolean) {
              theTopIO.outData.adrIOs.data.bits.expect(oneInActAdrStream(adrIdx).U)
              val adrOut = theTopIO.outData.adrIOs.data.bits.peek().litValue()
              println(s"[outAdr$outPostfix] adr = ($adrOut," +
                s" ${theTopIO.outData.adrIOs.data.valid.peek()}, firstNone ${theDebugIO.firstNoneZero.peek()}, " +
                s"zeroCol = ${theDebugIO.zeroColReg.peek()})")
              outAdr = outAdr:::List(adrOut.toInt)
              if (adrOut == 0) {
                println(s"[outAdr$outPostfix] oneColFin ${theDebugIO.oneColFin.peek()} " +
                  s"oneVecFin ${theDebugIO.oneVecFin.peek()}\n" +
                  s"allVecFin ${theDebugIO.allVecFin.peek()} " +
                  s"vecNum $outStreamNum")
              }
              adrIdx += 1
            }
            if (theTopIO.outData.dataIOs.data.valid.peek().litToBoolean) {
              theTopIO.outData.dataIOs.data.bits.expect(oneInActDataStream(dataIdx).U)
              val dataOut = theTopIO.outData.dataIOs.data.bits.peek().litValue()
              println(s"[outData$outPostfix] data&&Count = " +
                s"(${dataOut.toInt.toBinaryString}," +
                s" ${theTopIO.outData.dataIOs.data.valid.peek()})")
              outData = outData:::List(dataOut.toInt)
              dataIdx += 1
            }
            theClock.step()
          }
          col += 1
          println("[info] finish one column")
        }
        streamNum += 1
      }
      fork {
        for (_ <- 0 until 3) {
          while (!theTopIO.outData.adrIOs.data.valid.peek().litToBoolean) {
            theClock.step()
          }
          if (theTopIO.outData.adrIOs.data.valid.peek().litToBoolean) {
            val adrOut = theTopIO.outData.adrIOs.data.bits.peek().litValue()
            println(s"adr = ($adrOut," +
              s" ${theTopIO.outData.adrIOs.data.valid.peek()}, firstNone ${theDebugIO.firstNoneZero.peek()}, " +
              s"zeroCol = ${theDebugIO.zeroColReg.peek()})")
            outAdr = outAdr:::List(adrOut.toInt)
            if (adrOut == 0) {
              println(s"oneColFin ${theDebugIO.oneColFin.peek()}\n" +
                s"oneVecFin ${theDebugIO.oneVecFin.peek()}\n" +
                s"allVecFin ${theDebugIO.allVecFin.peek()}\n")
            }
            theClock.step()
          }
        }
      } .fork {
        for (_ <- 0 until 3) {
          while (!theTopIO.outData.dataIOs.data.valid.peek().litToBoolean) {
            theClock.step()
          }
          if (theTopIO.outData.dataIOs.data.valid.peek().litToBoolean) {
            val dataOut = theTopIO.outData.dataIOs.data.bits.peek().litValue()
            println(s"data&&Count = " +
              s"(${dataOut.toInt.toBinaryString}," +
              s" ${theTopIO.outData.dataIOs.data.valid.peek()})")
            outData = outData:::List(dataOut.toInt)
            theClock.step()
          }
        }
      } .join()
      println(s"[info] goldenAdr Vs. outAdr = ")
      oneInActAdrStream.zip(outAdr).foreach({ case (golden, out) =>
        if (golden == out) print(s"$golden\t\t\t")
        else print(s"$golden, $out\t\t")
        if (golden == 0) println()
      })
      println()
      println(s"[info] goldenData Vs. outData = ")
      oneInActDataStream.zip(outData).foreach({ case (golden, out) =>
        if (golden == out) print(s"$golden\t")
        else print(s"$golden, $out\t")
        if (golden == 0) println()
      })
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